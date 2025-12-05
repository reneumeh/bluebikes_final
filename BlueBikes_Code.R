# Install all required packages (run once)
#install.packages(c("ggplot2", "dplyr", "MASS", "RSQLite", "readr", "sf", 
#                   "stringr", "DBI", "lubridate", "viridis", "ggpubr", "moments", 
#                   "ggridges", "scales"))
library(ggplot2); library(dplyr); library(MASS); library(RSQLite); library(readr); library(sf); 
library(stringr); library(DBI); library(lubridate); library(viridis)
if(!require(scales)) install.packages("scales")
library(scales)

# unzip("data/bluebikes/bluebikes.zip")

################################################################################
# SIX SIGMA ANALYSIS: BlueBikes AM Rush Hour by Station (Past 4 Years)
################################################################################

# Connect to database
con <- dbConnect(RSQLite::SQLite(), dbname = "bluebikes.sqlite")

# Load supplementary datasets
dates <- readRDS("dates.rds")
stationbg <- readRDS("stationbg_dataset.rds")

# Check what date range is available in the database
cat("\nChecking available date range in database...\n")
date_range <- tbl(con, "tally_rush_edges") %>%
  summarise(
    min_date = min(day, na.rm = TRUE),
    max_date = max(day, na.rm = TRUE)
  ) %>%
  collect()

cat("Available date range:", as.character(date_range$min_date), "to", as.character(date_range$max_date), "\n")

# Use all available data instead of filtering by date
# The database contains data from 2011-2021, so we'll use all of it
cat("\nAnalyzing all available data in database (2011-2021)...\n")

################################################################################
# STEP 1: Extract AM and PM Rush Hour Data by Station
################################################################################

# Query the tally_rush_edges table which has station-level data
# Note: tally_rush_edges uses 'start_code' and 'end_code', not station_id
# Create SEPARATE TIBBLES for AM and PM

# First, check what columns are available
cat("\nColumns in tally_rush_edges table:\n")
print(dbListFields(con, "tally_rush_edges"))

# AM RUSH HOUR TIBBLE - Get all available data
am_rush_data <- tbl(con, "tally_rush_edges") %>%
  filter(rush == "am") %>%
  collect() %>%
  mutate(day = as.Date(day))

cat("\nAM rush hour records collected:", nrow(am_rush_data), "\n")

# PM RUSH HOUR TIBBLE - Get all available data
pm_rush_data <- tbl(con, "tally_rush_edges") %>%
  filter(rush == "pm") %>%
  collect() %>%
  mutate(day = as.Date(day))

cat("PM rush hour records collected:", nrow(pm_rush_data), "\n")

# Check if we have data
if (nrow(am_rush_data) == 0 || nrow(pm_rush_data) == 0) {
  stop("ERROR: No data found in database. Please check database connection and data availability.")
}

################################################################################
# STEP 2: Add Day of Week Information (Weekday vs Weekend)
################################################################################

# Add metadata to AM tibble
am_rush_data <- am_rush_data %>%
  mutate(
    weekday = weekdays(day),
    is_weekend = weekday %in% c("Saturday", "Sunday"),
    month = as.numeric(format(day, "%m")),
    year = as.numeric(format(day, "%Y")),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    )
  )

# Add metadata to PM tibble
pm_rush_data <- pm_rush_data %>%
  mutate(
    weekday = weekdays(day),
    is_weekend = weekday %in% c("Saturday", "Sunday"),
    month = as.numeric(format(day, "%m")),
    year = as.numeric(format(day, "%Y")),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    )
  )

################################################################################
# STEP 3: Separate Weekday vs Weekend Analysis
################################################################################

# AM WEEKDAY DATA (most relevant for commuter patterns)
am_weekday_data <- am_rush_data %>%
  filter(!is_weekend) %>%
  # Exclude extreme weather months (Dec, Jan, Feb) to reduce weather variability
  filter(!month %in% c(12, 1, 2))

# PM WEEKDAY DATA
pm_weekday_data <- pm_rush_data %>%
  filter(!is_weekend) %>%
  filter(!month %in% c(12, 1, 2))

cat("\nAM Weekday records (excluding winter):", nrow(am_weekday_data), "\n")
cat("PM Weekday records (excluding winter):", nrow(pm_weekday_data), "\n")

# Check if we have data after filtering
if (nrow(am_weekday_data) == 0 || nrow(pm_weekday_data) == 0) {
  stop("ERROR: No weekday data found after filtering. Please check date range and filters.")
}

################################################################################
# STEP 4: Calculate Process Statistics by Station (AM and PM Separately)
################################################################################

# AM STATION STATISTICS
# Note: using 'start_code' which is the column name in tally_rush_edges
am_station_stats <- am_weekday_data %>%
  group_by(start_code) %>%
  summarise(
    n_days = n(),
    mean_rides = if(n() > 0) mean(count, na.rm = TRUE) else NA_real_,
    sd_rides = if(n() > 1) sd(count, na.rm = TRUE) else NA_real_,
    median_rides = if(n() > 0) median(count, na.rm = TRUE) else NA_real_,
    min_rides = if(n() > 0 && sum(!is.na(count)) > 0) min(count, na.rm = TRUE) else NA_real_,
    max_rides = if(n() > 0 && sum(!is.na(count)) > 0) max(count, na.rm = TRUE) else NA_real_,
    total_rides = if(n() > 0) sum(count, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(n_days >= 30, !is.na(mean_rides), !is.na(sd_rides)) %>%
  mutate(
    UCL = mean_rides + 3 * sd_rides,
    LCL = pmax(0, mean_rides - 3 * sd_rides),
    CV = ifelse(mean_rides > 0, (sd_rides / mean_rides) * 100, NA_real_),
    rush_period = "AM"
  ) %>%
  filter(!is.na(CV)) %>%
  arrange(desc(mean_rides))

# PM STATION STATISTICS
pm_station_stats <- pm_weekday_data %>%
  group_by(start_code) %>%
  summarise(
    n_days = n(),
    mean_rides = if(n() > 0) mean(count, na.rm = TRUE) else NA_real_,
    sd_rides = if(n() > 1) sd(count, na.rm = TRUE) else NA_real_,
    median_rides = if(n() > 0) median(count, na.rm = TRUE) else NA_real_,
    min_rides = if(n() > 0 && sum(!is.na(count)) > 0) min(count, na.rm = TRUE) else NA_real_,
    max_rides = if(n() > 0 && sum(!is.na(count)) > 0) max(count, na.rm = TRUE) else NA_real_,
    total_rides = if(n() > 0) sum(count, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(n_days >= 30, !is.na(mean_rides), !is.na(sd_rides)) %>%
  mutate(
    UCL = mean_rides + 3 * sd_rides,
    LCL = pmax(0, mean_rides - 3 * sd_rides),
    CV = ifelse(mean_rides > 0, (sd_rides / mean_rides) * 100, NA_real_),
    rush_period = "PM"
  ) %>%
  filter(!is.na(CV)) %>%
  arrange(desc(mean_rides))

cat("\nAM stations analyzed:", nrow(am_station_stats), "\n")
cat("PM stations analyzed:", nrow(pm_station_stats), "\n")

################################################################################
# STEP 5: Identify Out-of-Control Stations (AM and PM)
################################################################################

# AM HIGH VARIABILITY
am_high_variability <- am_station_stats %>%
  filter(CV > 50) %>%
  arrange(desc(CV))

# PM HIGH VARIABILITY
pm_high_variability <- pm_station_stats %>%
  filter(CV > 50) %>%
  arrange(desc(CV))

cat("\nAM stations with CV > 50%:", nrow(am_high_variability), "\n")
cat("PM stations with CV > 50%:", nrow(pm_high_variability), "\n")

# AM OUT-OF-CONTROL EVENTS
am_out_of_control <- am_weekday_data %>%
  left_join(am_station_stats %>% dplyr::select(start_code, mean_rides, UCL, LCL), 
            by = "start_code") %>%
  filter(count > UCL | count < LCL) %>%
  mutate(
    violation_type = ifelse(count > UCL, "Above UCL", "Below LCL"),
    rush_period = "AM"
  )

# PM OUT-OF-CONTROL EVENTS
pm_out_of_control <- pm_weekday_data %>%
  left_join(pm_station_stats %>% dplyr::select(start_code, mean_rides, UCL, LCL), 
            by = "start_code") %>%
  filter(count > UCL | count < LCL) %>%
  mutate(
    violation_type = ifelse(count > UCL, "Above UCL", "Below LCL"),
    rush_period = "PM"
  )

cat("\nAM out-of-control events:", nrow(am_out_of_control), "\n")
cat("PM out-of-control events:", nrow(pm_out_of_control), "\n")

# AM VIOLATIONS BY STATION
am_violations_by_station <- am_out_of_control %>%
  group_by(start_code) %>%
  summarise(
    n_violations = n(),
    n_above_ucl = sum(violation_type == "Above UCL"),
    n_below_lcl = sum(violation_type == "Below LCL"),
    .groups = "drop"
  ) %>%
  left_join(am_station_stats %>% dplyr::select(start_code, n_days, mean_rides, CV), 
            by = "start_code") %>%
  mutate(
    violation_rate = (n_violations / n_days) * 100,
    rush_period = "AM"
  ) %>%
  arrange(desc(violation_rate))

# PM VIOLATIONS BY STATION
pm_violations_by_station <- pm_out_of_control %>%
  group_by(start_code) %>%
  summarise(
    n_violations = n(),
    n_above_ucl = sum(violation_type == "Above UCL"),
    n_below_lcl = sum(violation_type == "Below LCL"),
    .groups = "drop"
  ) %>%
  left_join(pm_station_stats %>% dplyr::select(start_code, n_days, mean_rides, CV), 
            by = "start_code") %>%
  mutate(
    violation_rate = (n_violations / n_days) * 100,
    rush_period = "PM"
  ) %>%
  arrange(desc(violation_rate))

################################################################################
# STEP 6: COMPREHENSIVE VISUALIZATIONS (To Support Six Sigma Analysis)
################################################################################

# PLOT 1: AM vs PM Comparison - Average Rides by Top 15 Stations
# Only plot if we have data
if (nrow(am_station_stats) > 0 && nrow(pm_station_stats) > 0) {
  # Get unique top stations from either AM or PM
  top_stations_am <- am_station_stats %>% head(15) %>% pull(start_code)
  top_stations_pm <- pm_station_stats %>% head(15) %>% pull(start_code)
  top_stations_combined <- unique(c(top_stations_am, top_stations_pm))
  
  if (length(top_stations_combined) > 0) {
    combined_top_stations <- bind_rows(
      am_station_stats %>% filter(start_code %in% top_stations_combined),
      pm_station_stats %>% filter(start_code %in% top_stations_combined)
    )
    
    if (nrow(combined_top_stations) > 0) {
      plot1 <- ggplot(combined_top_stations, 
                      aes(x = reorder(start_code, mean_rides), 
                          y = mean_rides, fill = rush_period)) +
        geom_col(position = "dodge", alpha = 0.8) +
        coord_flip() +
        scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                          name = "Rush Period") +
        labs(
          title = "AM vs PM Rush Hour: Top Stations Comparison",
          subtitle = "Average daily rides (weekdays, non-winter months)",
          x = "Station Code",
          y = "Average Daily Rides"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
      
      print(plot1)
    }
  }
}

# PLOT 2: Process Capability - CV Distribution Comparison (AM vs PM)
# Only plot if we have data
if (nrow(am_station_stats) > 0 && nrow(pm_station_stats) > 0) {
  combined_cv <- bind_rows(
    am_station_stats %>% dplyr::select(start_code, CV, rush_period),
    pm_station_stats %>% dplyr::select(start_code, CV, rush_period)
  )
  
  if (nrow(combined_cv) > 0 && sum(!is.na(combined_cv$CV)) > 0) {
    plot2 <- ggplot(combined_cv, aes(x = CV, fill = rush_period)) +
      geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
      geom_vline(xintercept = 50, color = "red", linetype = "dashed", size = 1) +
      scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                        name = "Rush Period") +
      annotate("text", x = 55, y = Inf, label = "High Variability\nThreshold", 
               vjust = 1.5, color = "red", size = 3) +
      labs(
        title = "Process Variability: AM vs PM Rush Hours",
        subtitle = "Distribution of Coefficient of Variation (CV) across all stations",
        x = "Coefficient of Variation (%)",
        y = "Number of Stations"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
    
    print(plot2)
  }
}

# PLOT 3: Control Chart for Highest Volume Station (AM vs PM)
# COMMENTED OUT - plots too many individual data points, slows down system
# top_am_station_code <- am_station_stats$start_code[1]
# 
# am_time_series <- am_weekday_data %>%
#   filter(start_code == top_am_station_code) %>%
#   left_join(am_station_stats %>% dplyr::select(start_code, mean_rides, UCL, LCL), 
#             by = "start_code") %>%
#   mutate(rush_period = "AM")
# 
# pm_time_series <- pm_weekday_data %>%
#   filter(start_code == top_am_station_code) %>%
#   left_join(pm_station_stats %>% dplyr::select(start_code, mean_rides, UCL, LCL), 
#             by = "start_code") %>%
#   mutate(rush_period = "PM")
# 
# combined_time_series <- bind_rows(am_time_series, pm_time_series)
# 
# plot3 <- ggplot(combined_time_series, aes(x = day, y = count, color = rush_period)) +
#   geom_line(alpha = 0.5) +
#   geom_point(aes(shape = count > UCL | count < LCL), size = 1.5, alpha = 0.7) +
#   geom_hline(data = combined_time_series %>% distinct(rush_period, mean_rides),
#              aes(yintercept = mean_rides, color = rush_period), 
#              linetype = "dashed", size = 0.8) +
#   scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
#                      name = "Rush Period") +
#   scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
#                      labels = c("In Control", "Out of Control"),
#                      name = "Status") +
#   facet_wrap(~rush_period, ncol = 1) +
#   labs(
#     title = paste("Control Chart: Station", top_am_station_code),
#     subtitle = "AM vs PM comparison over time (dashed line = mean)",
#     x = "Date",
#     y = "Number of Rides"
#   ) +
#   theme_minimal(base_size = 11)
# 
# print(plot3)

# PLOT 4: Violation Rates - AM vs PM Comparison (Top 15 from each)
# Only plot if we have violation data
if (nrow(am_violations_by_station) > 0 || nrow(pm_violations_by_station) > 0) {
  # Get unique top stations from either AM or PM violation lists
  top_violations_am <- if(nrow(am_violations_by_station) > 0) am_violations_by_station %>% head(15) %>% pull(start_code) else character(0)
  top_violations_pm <- if(nrow(pm_violations_by_station) > 0) pm_violations_by_station %>% head(15) %>% pull(start_code) else character(0)
  top_violations_combined <- unique(c(top_violations_am, top_violations_pm))
  
  if (length(top_violations_combined) > 0) {
    combined_violations <- bind_rows(
      if(nrow(am_violations_by_station) > 0) am_violations_by_station %>% filter(start_code %in% top_violations_combined) else tibble(),
      if(nrow(pm_violations_by_station) > 0) pm_violations_by_station %>% filter(start_code %in% top_violations_combined) else tibble()
    )
    
    if (nrow(combined_violations) > 0) {
      plot4 <- ggplot(combined_violations, 
                      aes(x = reorder(start_code, violation_rate), 
                          y = violation_rate, fill = rush_period)) +
        geom_col(position = "dodge", alpha = 0.8) +
        coord_flip() +
        scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                          name = "Rush Period") +
        labs(
          title = "Stations with Highest Control Violations: AM vs PM",
          subtitle = "% of days outside ±3σ control limits",
          x = "Station Code",
          y = "Violation Rate (%)"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
      
      print(plot4)
    }
  }
}

# PLOT 5: Pareto Chart - Stations Contributing Most to Variability (AM)
# Only plot if we have violation data
if (nrow(am_violations_by_station) > 0 && sum(am_violations_by_station$n_violations, na.rm = TRUE) > 0) {
  am_pareto_data <- am_violations_by_station %>%
    head(20) %>%
    mutate(
      cumulative_violations = cumsum(n_violations),
      cumulative_pct = (cumulative_violations / sum(n_violations)) * 100
    )
  
  if (nrow(am_pareto_data) > 0 && max(am_pareto_data$n_violations, na.rm = TRUE) > 0) {
    plot5 <- ggplot(am_pareto_data, aes(x = reorder(start_code, -n_violations))) +
      geom_col(aes(y = n_violations), fill = "#E69F00", alpha = 0.8) +
      geom_line(aes(y = cumulative_pct * max(n_violations, na.rm = TRUE) / 100, group = 1), 
                color = "red", size = 1.2) +
      geom_point(aes(y = cumulative_pct * max(n_violations, na.rm = TRUE) / 100), 
                 color = "red", size = 3) +
      scale_y_continuous(
        name = "Number of Violations",
        sec.axis = sec_axis(~./max(am_pareto_data$n_violations, na.rm = TRUE) * 100, 
                            name = "Cumulative %")
      ) +
      labs(
        title = "Pareto Analysis: AM Rush Hour Violations by Station",
        subtitle = "Top 20 problem stations (80/20 rule)",
        x = "Station Code"
      ) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot5)
  }
}

# PLOT 6: Time Series of Overall System Performance (AM vs PM)
# COMMENTED OUT - plots too many individual data points, slows down system
# am_daily_summary <- am_weekday_data %>%
#   group_by(day) %>%
#   summarise(
#     total_rides = sum(count),
#     avg_rides = mean(count),
#     .groups = "drop"
#   ) %>%
#   mutate(rush_period = "AM")
# 
# pm_daily_summary <- pm_weekday_data %>%
#   group_by(day) %>%
#   summarise(
#     total_rides = sum(count),
#     avg_rides = mean(count),
#     .groups = "drop"
#   ) %>%
#   mutate(rush_period = "PM")
# 
# combined_daily <- bind_rows(am_daily_summary, pm_daily_summary)
# 
# plot6 <- ggplot(combined_daily, aes(x = day, y = total_rides, color = rush_period)) +
#   geom_line(alpha = 0.7, size = 0.8) +
#   geom_smooth(se = TRUE, span = 0.2, alpha = 0.2) +
#   scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
#                      name = "Rush Period") +
#   labs(
#     title = "System-Level Performance Over Time",
#     subtitle = "Total daily rides across all stations (with smoothed trend)",
#     x = "Date",
#     y = "Total Rides"
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(legend.position = "top")
# 
# print(plot6)

# PLOT 7: Box Plot - Variability Comparison by Season (AM vs PM)
# COMMENTED OUT - uses large dataset with many individual data points, slows down system
# combined_seasonal <- bind_rows(
#   am_weekday_data %>% mutate(rush_period = "AM"),
#   pm_weekday_data %>% mutate(rush_period = "PM")
# )
# 
# plot7 <- ggplot(combined_seasonal, aes(x = season, y = count, fill = rush_period)) +
#   geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
#   scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
#                     name = "Rush Period") +
#   labs(
#     title = "Seasonal Variability in Rush Hour Ridership",
#     subtitle = "Distribution of rides by season (excluding winter)",
#     x = "Season",
#     y = "Number of Rides per Station per Day"
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(legend.position = "top")
# 
# print(plot7)

# PLOT 8: Scatter Plot - Mean vs CV (Process Capability Analysis)
# Only plot if we have data
if (nrow(am_station_stats) > 0 && nrow(pm_station_stats) > 0) {
  combined_mean_cv <- bind_rows(
    am_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period),
    pm_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period)
  )
  
  if (nrow(combined_mean_cv) > 0 && sum(!is.na(combined_mean_cv$mean_rides)) > 0 && sum(!is.na(combined_mean_cv$CV)) > 0) {
    plot8 <- ggplot(combined_mean_cv, aes(x = mean_rides, y = CV, color = rush_period)) +
      geom_point(alpha = 0.6, size = 2.5) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                         name = "Rush Period") +
      scale_x_log10() +
      annotate("text", x = max(combined_mean_cv$mean_rides, na.rm = TRUE), y = 55, 
               label = "High Variability Zone", color = "red", hjust = 1) +
      labs(
        title = "Process Capability: Mean vs Variability",
        subtitle = "Higher volume doesn't always mean more stability",
        x = "Average Daily Rides (log scale)",
        y = "Coefficient of Variation (%)"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
    
    print(plot8)
  }
}

################################################################################
# STEP 6A: COEFFICIENT OF VARIATION (CV) VISUALIZATIONS
################################################################################

cat("\nCreating Coefficient of Variation charts...\n")

# CV PLOT 1: Top Stations with Highest CV (AM vs PM Side-by-Side)
# Get unique top CV stations from either AM or PM
top_cv_am <- am_station_stats %>% arrange(desc(CV)) %>% head(20) %>% pull(start_code)
top_cv_pm <- pm_station_stats %>% arrange(desc(CV)) %>% head(20) %>% pull(start_code)
top_cv_stations_list <- unique(c(top_cv_am, top_cv_pm))

combined_cv_top <- bind_rows(
  am_station_stats %>% filter(start_code %in% top_cv_stations_list) %>% arrange(desc(CV)),
  pm_station_stats %>% filter(start_code %in% top_cv_stations_list) %>% arrange(desc(CV))
)

cv_plot1 <- ggplot(combined_cv_top, 
                   aes(x = reorder(start_code, CV), y = CV, fill = rush_period)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.8) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1.2) +
  coord_flip() +
  scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                    name = "Rush Period") +
  labs(
    title = "Stations with Highest Process Variability",
    subtitle = "Top stations by Coefficient of Variation (AM vs PM comparison)",
    x = "Station Code",
    y = "Coefficient of Variation (%)",
    caption = "Red line indicates 50% CV threshold | Higher CV = Less predictable demand"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9)
  )

print(cv_plot1)

# CV PLOT 2: Top 20 Stations with Lowest CV (Most Stable)
combined_cv_stable <- bind_rows(
  am_station_stats %>% arrange(CV) %>% head(20) %>% mutate(type = "Lowest CV"),
  pm_station_stats %>% arrange(CV) %>% head(20) %>% mutate(type = "Lowest CV")
) %>%
  group_by(start_code) %>%
  filter(n() == 2) %>%
  ungroup()

cv_plot2 <- ggplot(combined_cv_stable, 
                   aes(x = reorder(start_code, -CV), y = CV, fill = rush_period)) +
  geom_col(position = "dodge", alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("AM" = "#009E73", "PM" = "#0072B2"),
                    name = "Rush Period") +
  labs(
    title = "Most Stable Stations (Lowest Variability)",
    subtitle = "Top 20 stations with lowest Coefficient of Variation",
    x = "Station Code",
    y = "Coefficient of Variation (%)",
    caption = "Lower CV indicates more predictable, stable demand"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(cv_plot2)

# CV PLOT 3: REMOVED (per user request)
# combined_cv_volume <- bind_rows(
#   am_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period),
#   pm_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period)
# ) %>%
#   mutate(
#     volume_category = case_when(
#       mean_rides < 5 ~ "Low Volume\n(<5 rides/day)",
#       mean_rides < 15 ~ "Medium Volume\n(5-15 rides/day)",
#       mean_rides < 30 ~ "High Volume\n(15-30 rides/day)",
#       TRUE ~ "Very High Volume\n(>30 rides/day)"
#     ),
#     volume_category = factor(volume_category, 
#                             levels = c("Low Volume\n(<5 rides/day)", 
#                                      "Medium Volume\n(5-15 rides/day)",
#                                      "High Volume\n(15-30 rides/day)", 
#                                      "Very High Volume\n(>30 rides/day)"))
#   )
# 
# cv_plot3 <- ggplot(combined_cv_volume, aes(x = volume_category, y = CV, fill = rush_period)) +
#   geom_violin(alpha = 0.6, position = position_dodge(0.9)) +
#   geom_boxplot(width = 0.2, alpha = 0.8, position = position_dodge(0.9),
#                outlier.alpha = 0.3, outlier.size = 1) +
#   geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 0.8) +
#   scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
#                     name = "Rush Period") +
#   labs(
#     title = "CV Distribution by Station Volume Category",
#     subtitle = "Does higher volume lead to more stability?",
#     x = "Station Volume Category",
#     y = "Coefficient of Variation (%)",
#     caption = "Violin plots show distribution; boxes show median and quartiles"
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     legend.position = "top",
#     plot.title = element_text(face = "bold", size = 14),
#     plot.subtitle = element_text(size = 10, color = "gray40"),
#     panel.grid.major.x = element_blank()
#   )
# 
# print(cv_plot3)

# CV PLOT 4: REMOVED (per user request)
# cv_categories <- bind_rows(
#   am_station_stats %>% dplyr::select(start_code, CV, rush_period),
#   pm_station_stats %>% dplyr::select(start_code, CV, rush_period)
# ) %>%
#   mutate(
#     cv_category = case_when(
#       CV < 25 ~ "Low Variability\n(CV < 25%)",
#       CV < 50 ~ "Moderate Variability\n(CV 25-50%)",
#       CV < 75 ~ "High Variability\n(CV 50-75%)",
#       TRUE ~ "Very High Variability\n(CV > 75%)"
#     ),
#     cv_category = factor(cv_category, 
#                         levels = c("Low Variability\n(CV < 25%)",
#                                  "Moderate Variability\n(CV 25-50%)",
#                                  "High Variability\n(CV 50-75%)",
#                                  "Very High Variability\n(CV > 75%)"))
#   ) %>%
#   group_by(rush_period, cv_category) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   group_by(rush_period) %>%
#   mutate(
#     total = sum(count),
#     percentage = (count / total) * 100,
#     label = sprintf("%.1f%%", percentage)
#   )
# 
# cv_plot4 <- ggplot(cv_categories, aes(x = rush_period, y = count, fill = cv_category)) +
#   geom_col(position = "fill", alpha = 0.9) +
#   geom_text(aes(label = label), position = position_fill(vjust = 0.5), 
#             color = "white", fontface = "bold", size = 4) +
#   scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Variability Level") +
#   scale_y_continuous(labels = scales::percent) +
#   labs(
#     title = "Process Capability Distribution: AM vs PM",
#     subtitle = "Percentage of stations in each variability category",
#     x = "Rush Period",
#     y = "Percentage of Stations",
#     caption = "Target: Maximize green (low variability) zones"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     legend.position = "right",
#     plot.title = element_text(face = "bold", size = 14),
#     plot.subtitle = element_text(size = 10, color = "gray40"),
#     panel.grid = element_blank()
#   )
# 
# print(cv_plot4)

# CV PLOT 5: CV Trend Over Time (Monthly Average)
am_monthly_cv <- am_weekday_data %>%
  mutate(year_month = format(day, "%Y-%m")) %>%
  group_by(year_month, start_code) %>%
  summarise(
    mean_rides = mean(count, na.rm = TRUE),
    sd_rides = sd(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(sd_rides), mean_rides > 0) %>%
  mutate(CV = (sd_rides / mean_rides) * 100) %>%
  group_by(year_month) %>%
  summarise(
    avg_cv = mean(CV, na.rm = TRUE),
    median_cv = median(CV, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    date = as.Date(paste0(year_month, "-01")),
    rush_period = "AM"
  )

pm_monthly_cv <- pm_weekday_data %>%
  mutate(year_month = format(day, "%Y-%m")) %>%
  group_by(year_month, start_code) %>%
  summarise(
    mean_rides = mean(count, na.rm = TRUE),
    sd_rides = sd(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(sd_rides), mean_rides > 0) %>%
  mutate(CV = (sd_rides / mean_rides) * 100) %>%
  group_by(year_month) %>%
  summarise(
    avg_cv = mean(CV, na.rm = TRUE),
    median_cv = median(CV, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    date = as.Date(paste0(year_month, "-01")),
    rush_period = "PM"
  )

combined_monthly_cv <- bind_rows(am_monthly_cv, pm_monthly_cv)

cv_plot5 <- ggplot(combined_monthly_cv, aes(x = date, y = avg_cv, color = rush_period)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(se = TRUE, span = 0.3, alpha = 0.2, size = 0.8) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 0.8) +
  scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                     name = "Rush Period") +
  labs(
    title = "System-Wide CV Trends Over Time",
    subtitle = "Average Coefficient of Variation across all stations by month",
    x = "Date",
    y = "Average CV (%)",
    caption = "Smoothed trend line shows overall pattern; red line = 50% threshold"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank()
  )

print(cv_plot5)

# CV PLOT 6: CV vs Volume Scatter with Density Contours
cv_plot6 <- ggplot(combined_mean_cv, aes(x = mean_rides, y = CV, color = rush_period)) +
  geom_density_2d(alpha = 0.5, size = 0.8) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, size = 1) +
  scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                     name = "Rush Period") +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Process Stability Analysis: Volume vs Variability",
    subtitle = "Relationship between station volume and coefficient of variation",
    x = "Average Daily Rides (log scale)",
    y = "Coefficient of Variation (%)",
    caption = "Density contours show concentration of stations; trend lines show relationship"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank()
  )

print(cv_plot6)

# CV PLOT 7: Ridgeline Plot of CV Distribution by Year
if(!require(ggridges)) install.packages("ggridges")
library(ggridges)

combined_cv_yearly <- bind_rows(
  am_station_stats %>% dplyr::select(start_code, CV) %>% mutate(rush_period = "AM"),
  pm_station_stats %>% dplyr::select(start_code, CV) %>% mutate(rush_period = "PM")
)

cv_plot7 <- ggplot(combined_cv_yearly, aes(x = CV, y = rush_period, fill = rush_period)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9, color = "white", size = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                    name = "Rush Period") +
  labs(
    title = "CV Distribution Comparison: AM vs PM Rush Hours",
    subtitle = "Ridgeline plot showing full distribution of process variability",
    x = "Coefficient of Variation (%)",
    y = "Rush Period",
    caption = "Red line = 50% CV threshold | Higher peaks indicate more stations at that CV level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.grid.major.y = element_blank()
  )

print(cv_plot7)

# CV PLOT 8: Top Stations with Highest CV - Single Clean Bar Chart
# Get top 15 stations across both AM and PM periods (combined average)
top_cv_overall <- bind_rows(
  am_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period),
  pm_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period)
) %>%
  group_by(start_code) %>%
  summarise(
    avg_cv = mean(CV, na.rm = TRUE),
    avg_rides = mean(mean_rides, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_cv)) %>%
  head(15)

cv_plot8 <- ggplot(top_cv_overall, 
                   aes(x = reorder(start_code, avg_cv), y = avg_cv, fill = avg_cv)) +
  # Add the bars with gradient color
  geom_col(alpha = 0.95, color = "white", size = 1.2, width = 0.8) +
  # Add percentage labels on bars
  geom_text(aes(label = paste0(round(avg_cv, 1), "%")), 
            hjust = -0.15, size = 4, fontface = "bold", color = "gray20") +
  # Add a reference line at 50% CV
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1.2, alpha = 0.7) +
  annotate("text", x = 14, y = 52, label = "High Variability Threshold (50%)", 
           color = "red", fontface = "bold", size = 3.5, hjust = 0) +
  # Flip coordinates for horizontal bars
  coord_flip(ylim = c(0, max(top_cv_overall$avg_cv) * 1.15)) +
  # Color gradient from green (low) to red (high)
  scale_fill_gradient2(
    low = "#2E7D32",      # Dark green
    mid = "#FFC107",       # Amber
    high = "#C62828",      # Dark red
    midpoint = 50,
    guide = "none"
  ) +
  # Labels and titles
  labs(
    title = "Top 15 Stations with Highest Process Variability",
    subtitle = "Average Coefficient of Variation (CV) across AM and PM Rush Hours",
    x = "Station Code",
    y = "Coefficient of Variation (%)",
    caption = "CV measures variability relative to the mean | Higher CV = less predictable demand"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Title styling
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, 
                             margin = margin(b = 8)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5,
                               margin = margin(t = 15)),
    # Axis styling
    axis.text.y = element_text(size = 10, color = "gray20", face = "bold"),
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.title = element_text(face = "bold", size = 11),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    # Grid styling
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", size = 0.5),
    # Background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.margin = margin(20, 30, 20, 20)
  )

print(cv_plot8)

cat("\nCoefficient of Variation charts complete!\n")

################################################################################
# STEP 6B: PROCESS OVERVIEW CHARTS (Boxplot + Histogram Style)
################################################################################

cat("\nCreating process overview charts...\n")

# Load ggpubr for ggarrange
if(!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

# Load moments package for skewness and kurtosis
if(!require(moments)) install.packages("moments")
library(moments)

# Set a consistent theme for all quality control plots
theme_set(
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey"),
    plot.margin = margin(r = 0)
  )
)

# Helper function to calculate descriptive statistics (matching onsen example)
describe = function(x){
  tibble(x) %>%
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      skew = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE)
    ) %>%
    mutate(
      caption = paste(
        "Process Mean: ", mean %>% round(2), " | ",
        "SD: ", sd %>% round(2), " | ",
        "Skewness: ", skew %>% round(2), " | ",
        "Kurtosis: ", kurtosis %>% round(2),
        sep = "")
    ) %>%
    return()
}

# For 2016 analysis - get FULL YEAR directly from database (no filtering)
# Pull data fresh from database for complete year

cat("\nPulling complete 2016 data from database...\n")

# Get ALL 2016 AM data directly from database
am_2016_full <- tbl(con, "tally_rush_edges") %>%
  filter(rush == "am") %>%
  collect() %>%
  mutate(day = as.Date(day)) %>%
  mutate(
    weekday = weekdays(day),
    is_weekend = weekday %in% c("Saturday", "Sunday"),
    month = as.numeric(format(day, "%m")),
    year = as.numeric(format(day, "%Y"))
  ) %>%
  filter(year == 2016, !is_weekend) %>%  # Only 2016 weekdays, NO season filtering
  mutate(
    month_name = format(day, "%B"),  # Full month name
    month_factor = factor(month_name, levels = c("January", "February", "March", 
                                                  "April", "May", "June",
                                                  "July", "August", "September",
                                                  "October", "November", "December"))
  )

# Get ALL 2016 PM data directly from database
pm_2016_full <- tbl(con, "tally_rush_edges") %>%
  filter(rush == "pm") %>%
  collect() %>%
  mutate(day = as.Date(day)) %>%
  mutate(
    weekday = weekdays(day),
    is_weekend = weekday %in% c("Saturday", "Sunday"),
    month = as.numeric(format(day, "%m")),
    year = as.numeric(format(day, "%Y"))
  ) %>%
  filter(year == 2016, !is_weekend) %>%  # Only 2016 weekdays, NO season filtering
  mutate(
    month_name = format(day, "%B"),  # Full month name
    month_factor = factor(month_name, levels = c("January", "February", "March", 
                                                  "April", "May", "June",
                                                  "July", "August", "September",
                                                  "October", "November", "December"))
  )

# Debug: Check what months we actually have
cat("\nMonths available in 2016 AM data:\n")
print(table(am_2016_full$month_name))
cat("\nMonths available in 2016 PM data:\n")
print(table(pm_2016_full$month_name))
cat("\nDate range in 2016 AM data:", as.character(min(am_2016_full$day)), "to", as.character(max(am_2016_full$day)), "\n")

# General process overview function (matching the ggprocess pattern)
ggprocess = function(x, y, xlab = "Subgroup", ylab = "Metric"){
  
  # Get descriptive statistics
  tab = describe(y)
  
  # Make the initial boxplot...
  g1 = ggplot(mapping = aes(x = x, y = y, group = x)) +
    # Plot grand mean
    geom_hline(mapping = aes(yintercept = mean(y)), color = "lightgrey", size = 3) +
    # Plot points and boxplots 
    geom_jitter(height = 0, width = 0.25) +
    geom_boxplot() +
    labs(x = xlab, y = ylab,
         subtitle = "Process Overview",
         # Add our descriptive stats in the caption!
         caption = tab$caption)
  
  # Make the histogram, but tilt it on its side
  g2 = ggplot(mapping = aes(x = y)) +
    geom_histogram(bins = 15, color = "white", fill = "grey") +
    theme_void() +   # Clear the theme
    coord_flip()   # tilt on its side
  
  # Then bind them together into 1 plot, 'h'orizontally aligned.
  p1 = ggarrange(g1, g2, widths = c(5, 1), align = "h")
  
  return(p1)
}

# Create 2016 AM Process Overview (Full Year)
cat("\n=== 2016 AM RUSH HOUR PROCESS OVERVIEW (FULL YEAR) ===\n")
am_2016_overview <- ggprocess(
  x = am_2016_full$month_factor, 
  y = am_2016_full$count,
  xlab = "Month",
  ylab = "Daily Rides per Station"
)
# Add title
am_2016_overview_titled <- annotate_figure(am_2016_overview,
  top = text_grob("2016 AM Rush Hour - Process Overview (Full Year)", 
                  face = "bold", size = 16))
print(am_2016_overview_titled)

# Create 2016 PM Process Overview (Full Year)
cat("\n=== 2016 PM RUSH HOUR PROCESS OVERVIEW (FULL YEAR) ===\n")
pm_2016_overview <- ggprocess(
  x = pm_2016_full$month_factor, 
  y = pm_2016_full$count,
  xlab = "Month",
  ylab = "Daily Rides per Station"
)
# Add title
pm_2016_overview_titled <- annotate_figure(pm_2016_overview,
  top = text_grob("2016 PM Rush Hour - Process Overview (Full Year)", 
                  face = "bold", size = 16))
print(pm_2016_overview_titled)

# Show summary of the 2016 data
cat("\n2016 AM Data Summary:\n")
cat("  Total days:", nrow(am_2016_full), "\n")
cat("  Date range:", min(am_2016_full$day), "to", max(am_2016_full$day), "\n")
cat("  Unique months:", length(unique(am_2016_full$month_name)), "\n")
cat("  Months included:", paste(sort(unique(am_2016_full$month_name)), collapse = ", "), "\n")

cat("\n2016 PM Data Summary:\n")
cat("  Total days:", nrow(pm_2016_full), "\n")
cat("  Date range:", min(pm_2016_full$day), "to", max(pm_2016_full$day), "\n")
cat("  Unique months:", length(unique(pm_2016_full$month_name)), "\n")
cat("  Months included:", paste(sort(unique(pm_2016_full$month_name)), collapse = ", "), "\n")

# Reset theme to default
theme_set(theme_grey())

cat("\nProcess overview charts complete!\n")

################################################################################
# STEP 7: Summary Report
################################################################################

cat("\n")
cat("================================================================================\n")
cat("                    SIX SIGMA PROCESS ANALYSIS SUMMARY                         \n")
cat("================================================================================\n")
if (nrow(am_weekday_data) > 0) {
  cat("Date Range:", as.character(min(am_weekday_data$day)), "to", as.character(max(am_weekday_data$day)), "\n")
} else {
  cat("Date Range: No data available\n")
}
cat("Analysis Scope: Weekdays only, excluding winter months (Dec-Feb)\n")
cat("\n")

cat("======================== AM RUSH HOUR STATISTICS ========================\n")
cat("\n")
cat("OVERALL STATISTICS (AM):\n")
cat("  Total stations analyzed:", nrow(am_station_stats), "\n")
if (nrow(am_station_stats) > 0) {
  cat("  Average rides per station per day:", round(mean(am_station_stats$mean_rides, na.rm = TRUE), 2), "\n")
  cat("  Median CV across stations:", round(median(am_station_stats$CV, na.rm = TRUE), 2), "%\n")
} else {
  cat("  No stations to analyze\n")
}
cat("\n")
cat("PROCESS CAPABILITY (AM):\n")
if (nrow(am_station_stats) > 0) {
  cat("  Stations with HIGH variability (CV > 50%):", nrow(am_high_variability), 
      sprintf("(%.1f%%)\n", 100 * nrow(am_high_variability) / nrow(am_station_stats)))
  cat("  Stations with MODERATE variability (CV 25-50%):", 
      nrow(am_station_stats %>% filter(CV >= 25 & CV <= 50)),
      sprintf("(%.1f%%)\n", 100 * nrow(am_station_stats %>% filter(CV >= 25 & CV <= 50)) / nrow(am_station_stats)))
  cat("  Stations with LOW variability (CV < 25%):", 
      nrow(am_station_stats %>% filter(CV < 25)),
      sprintf("(%.1f%%)\n", 100 * nrow(am_station_stats %>% filter(CV < 25)) / nrow(am_station_stats)))
} else {
  cat("  No stations to analyze\n")
}
cat("\n")
cat("OUT-OF-CONTROL EVENTS (AM):\n")
cat("  Total violations:", nrow(am_out_of_control), "\n")
cat("  Stations with at least one violation:", nrow(am_violations_by_station), "\n")
cat("\n")
cat("TOP 5 MOST VARIABLE AM STATIONS (by CV):\n")
am_high_variability %>%
  head(5) %>%
  dplyr::select(start_code, mean_rides, CV) %>%
  print()
cat("\n")
cat("TOP 5 AM STATIONS BY VIOLATION RATE:\n")
am_violations_by_station %>%
  head(5) %>%
  dplyr::select(start_code, violation_rate, n_violations, n_days) %>%
  print()
cat("\n")

cat("======================== PM RUSH HOUR STATISTICS ========================\n")
cat("\n")
cat("OVERALL STATISTICS (PM):\n")
cat("  Total stations analyzed:", nrow(pm_station_stats), "\n")
if (nrow(pm_station_stats) > 0) {
  cat("  Average rides per station per day:", round(mean(pm_station_stats$mean_rides, na.rm = TRUE), 2), "\n")
  cat("  Median CV across stations:", round(median(pm_station_stats$CV, na.rm = TRUE), 2), "%\n")
} else {
  cat("  No stations to analyze\n")
}
cat("\n")
cat("PROCESS CAPABILITY (PM):\n")
if (nrow(pm_station_stats) > 0) {
  cat("  Stations with HIGH variability (CV > 50%):", nrow(pm_high_variability), 
      sprintf("(%.1f%%)\n", 100 * nrow(pm_high_variability) / nrow(pm_station_stats)))
  cat("  Stations with MODERATE variability (CV 25-50%):", 
      nrow(pm_station_stats %>% filter(CV >= 25 & CV <= 50)),
      sprintf("(%.1f%%)\n", 100 * nrow(pm_station_stats %>% filter(CV >= 25 & CV <= 50)) / nrow(pm_station_stats)))
  cat("  Stations with LOW variability (CV < 25%):", 
      nrow(pm_station_stats %>% filter(CV < 25)),
      sprintf("(%.1f%%)\n", 100 * nrow(pm_station_stats %>% filter(CV < 25)) / nrow(pm_station_stats)))
} else {
  cat("  No stations to analyze\n")
}
cat("\n")
cat("OUT-OF-CONTROL EVENTS (PM):\n")
cat("  Total violations:", nrow(pm_out_of_control), "\n")
cat("  Stations with at least one violation:", nrow(pm_violations_by_station), "\n")
cat("\n")
cat("TOP 5 MOST VARIABLE PM STATIONS (by CV):\n")
pm_high_variability %>%
  head(5) %>%
  dplyr::select(start_code, mean_rides, CV) %>%
  print()
cat("\n")
cat("TOP 5 PM STATIONS BY VIOLATION RATE:\n")
pm_violations_by_station %>%
  head(5) %>%
  dplyr::select(start_code, violation_rate, n_violations, n_days) %>%
  print()
cat("\n")
cat("================================================================================\n")

# Save results for further analysis (both AM and PM)
saveRDS(am_station_stats, "am_station_statistics.rds")
saveRDS(pm_station_stats, "pm_station_statistics.rds")
saveRDS(am_violations_by_station, "am_violations_by_station.rds")
saveRDS(pm_violations_by_station, "pm_violations_by_station.rds")

cat("\nResults saved to:\n")
cat("  - am_station_statistics.rds\n")
cat("  - pm_station_statistics.rds\n")
cat("  - am_violations_by_station.rds\n")
cat("  - pm_violations_by_station.rds\n")

################################################################################
# END OF SIX SIGMA ANALYSIS
################################################################################

# OLD EXPLORATORY CODE - COMMENTED OUT
# bgdata <- readRDS("bgdataset.rds")
# dates <- readRDS("dates.rds")
# stationbg <- readRDS("stationbg_dataset.rds")
# 
# glimpse(bgdata)
# glimpse(dates)
# glimpse(stationbg)
# 
# con <- dbConnect(RSQLite::SQLite(), dbname = "bluebikes.sqlite")
# dbListTables(con)
# dbListFields(con, "dates")
# dbListFields(con, "stationbg_dataset")
# dbListFields(con, "tally_rush")
# dbListFields(con, "tally_rush_edges")
# 
# rush_edges <- tbl(con, "tally_rush")
# 
# con %>% tbl("stations") %>% filter(geoid = "32012") %>% group_by(geoid) %>%

# 
# random_day <- rush_edges %>%
#   distinct(day) %>%    # get unique days
#   collect() %>%        # bring them into R memory
#   sample_n(1) %>%      # randomly pick one
#   pull(day)            # extract the value
# 
# result <- rush_edges %>%
#   filter(day == random_day) %>%
#   summarise(
#     total_count = sum(count),
#     num_rush_entries = n()
#   ) %>%
#   collect()
# 
# print(result)
# 
# rush_edges %>%
#   distinct(day) %>%    # get unique days
#   collect() %>%        # bring them into R memory
#   sample_n(50) %>%      # randomly pick one
#   pull(day)            # extract the value
# 
# result <- rush_edges %>%
#   filter(day == random_day) %>%
#   collect () %>%
#   summarise(
#     total_count = sum(count),
#     num_rush_entries = n()
#   ) %>%
#   collect()
# 
# print(result)
# ## 
# 
# rush_edges <- tbl(con, "tally_rush")
# random_row <- rush_edges %>%
#   dplyr::slice_sample(n = 1) %>%  
#   collect()
# 
# cat("\n--- Random row (day, rush, count) ---\n")
# print(random_row)
# 
# random_day <- random_row$day
# 
# cat("\nRandom day selected: ", random_day, "\n", sep = "")
# 
# result_day <- rush_edges %>%
#   filter(day == random_day) %>%
#   summarise(
#     total_count = sum(count),
#     num_rush_entries = n()
#   ) %>%
#   collect() %>%
#   mutate(day = random_day) %>%
#   select(day, total_count, num_rush_entries)
# 
# cat("\n--- Summary for the selected day ---\n")




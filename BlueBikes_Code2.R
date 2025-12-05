# Install all required packages (run once)
#install.packages(c("ggplot2", "dplyr", "MASS", "RSQLite", "readr", "sf", 
#                   "stringr", "DBI", "lubridate", "viridis", "ggpubr", "moments"))
library(ggplot2); library(dplyr); library(MASS); library(RSQLite); library(readr); library(sf); 
library(stringr); library(DBI); library(lubridate); library(viridis)

# unzip("data/bluebikes/bluebikes.zip")

################################################################################
# SIX SIGMA ANALYSIS: BlueBikes AM Rush Hour by Station (Past 4 Years)
################################################################################

# Connect to database
con <- dbConnect(RSQLite::SQLite(), dbname = "bluebikes.sqlite")

# Load supplementary datasets
dates <- readRDS("dates.rds")
stationbg <- readRDS("stationbg_dataset.rds")

# Get current date and calculate 4 years ago
current_date <- Sys.Date()
four_years_ago <- current_date - (4 * 365)

cat("Analyzing data from", as.character(four_years_ago), "to", as.character(current_date), "\n")

################################################################################
# STEP 1: Extract AM and PM Rush Hour Data by Station for Past 4 Years
################################################################################

# Query the tally_rush_edges table which has station-level data
# Note: tally_rush_edges uses 'start_code' and 'end_code', not station_id
# Create SEPARATE TIBBLES for AM and PM

# First, check what columns are available
cat("\nColumns in tally_rush_edges table:\n")
print(dbListFields(con, "tally_rush_edges"))

# AM RUSH HOUR TIBBLE
am_rush_data <- tbl(con, "tally_rush_edges") %>%
  filter(rush == "am") %>%
  filter(day >= !!as.character(four_years_ago)) %>%
  collect() %>%
  mutate(day = as.Date(day))

cat("\nAM rush hour records collected:", nrow(am_rush_data), "\n")

# PM RUSH HOUR TIBBLE
pm_rush_data <- tbl(con, "tally_rush_edges") %>%
  filter(rush == "pm") %>%
  filter(day >= !!as.character(four_years_ago)) %>%
  collect() %>%
  mutate(day = as.Date(day))

cat("PM rush hour records collected:", nrow(pm_rush_data), "\n")

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

################################################################################
# STEP 4: Calculate Process Statistics by Station (AM and PM Separately)
################################################################################

# AM STATION STATISTICS
# Note: using 'start_code' which is the column name in tally_rush_edges
am_station_stats <- am_weekday_data %>%
  group_by(start_code) %>%
  summarise(
    n_days = n(),
    mean_rides = mean(count, na.rm = TRUE),
    sd_rides = sd(count, na.rm = TRUE),
    median_rides = median(count, na.rm = TRUE),
    min_rides = min(count, na.rm = TRUE),
    max_rides = max(count, na.rm = TRUE),
    total_rides = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_days >= 30) %>%
  mutate(
    UCL = mean_rides + 3 * sd_rides,
    LCL = pmax(0, mean_rides - 3 * sd_rides),
    CV = (sd_rides / mean_rides) * 100,
    rush_period = "AM"
  ) %>%
  arrange(desc(mean_rides))

# PM STATION STATISTICS
pm_station_stats <- pm_weekday_data %>%
  group_by(start_code) %>%
  summarise(
    n_days = n(),
    mean_rides = mean(count, na.rm = TRUE),
    sd_rides = sd(count, na.rm = TRUE),
    median_rides = median(count, na.rm = TRUE),
    min_rides = min(count, na.rm = TRUE),
    max_rides = max(count, na.rm = TRUE),
    total_rides = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_days >= 30) %>%
  mutate(
    UCL = mean_rides + 3 * sd_rides,
    LCL = pmax(0, mean_rides - 3 * sd_rides),
    CV = (sd_rides / mean_rides) * 100,
    rush_period = "PM"
  ) %>%
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
combined_top_stations <- bind_rows(
  am_station_stats %>% head(15),
  pm_station_stats %>% head(15)
) %>%
  group_by(start_code) %>%
  filter(n() == 2) %>%  # Only stations in both top 15
  ungroup()

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

# PLOT 2: Process Capability - CV Distribution Comparison (AM vs PM)
combined_cv <- bind_rows(
  am_station_stats %>% dplyr::select(start_code, CV, rush_period),
  pm_station_stats %>% dplyr::select(start_code, CV, rush_period)
)

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

# PLOT 3: Control Chart for Highest Volume Station (AM vs PM)
top_am_station_code <- am_station_stats$start_code[1]

am_time_series <- am_weekday_data %>%
  filter(start_code == top_am_station_code) %>%
  left_join(am_station_stats %>% dplyr::select(start_code, mean_rides, UCL, LCL), 
            by = "start_code") %>%
  mutate(rush_period = "AM")

pm_time_series <- pm_weekday_data %>%
  filter(start_code == top_am_station_code) %>%
  left_join(pm_station_stats %>% dplyr::select(start_code, mean_rides, UCL, LCL), 
            by = "start_code") %>%
  mutate(rush_period = "PM")

combined_time_series <- bind_rows(am_time_series, pm_time_series)

plot3 <- ggplot(combined_time_series, aes(x = day, y = count, color = rush_period)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(shape = count > UCL | count < LCL), size = 1.5, alpha = 0.7) +
  geom_hline(data = combined_time_series %>% distinct(rush_period, mean_rides),
             aes(yintercept = mean_rides, color = rush_period), 
             linetype = "dashed", size = 0.8) +
  scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                     name = "Rush Period") +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                     labels = c("In Control", "Out of Control"),
                     name = "Status") +
  facet_wrap(~rush_period, ncol = 1) +
  labs(
    title = paste("Control Chart: Station", top_am_station_code),
    subtitle = "AM vs PM comparison over time (dashed line = mean)",
    x = "Date",
    y = "Number of Rides"
  ) +
  theme_minimal(base_size = 11)

print(plot3)

# PLOT 4: Violation Rates - AM vs PM Comparison (Top 15 from each)
combined_violations <- bind_rows(
  am_violations_by_station %>% head(15),
  pm_violations_by_station %>% head(15)
) %>%
  group_by(start_code) %>%
  filter(n() == 2) %>%  # Only stations in both top 15
  ungroup()

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

# PLOT 5: Pareto Chart - Stations Contributing Most to Variability (AM)
am_pareto_data <- am_violations_by_station %>%
  head(20) %>%
  mutate(
    cumulative_violations = cumsum(n_violations),
    cumulative_pct = (cumulative_violations / sum(n_violations)) * 100
  )

plot5 <- ggplot(am_pareto_data, aes(x = reorder(start_code, -n_violations))) +
  geom_col(aes(y = n_violations), fill = "#E69F00", alpha = 0.8) +
  geom_line(aes(y = cumulative_pct * max(n_violations) / 100, group = 1), 
            color = "red", size = 1.2) +
  geom_point(aes(y = cumulative_pct * max(n_violations) / 100), 
             color = "red", size = 3) +
  scale_y_continuous(
    name = "Number of Violations",
    sec.axis = sec_axis(~./max(am_pareto_data$n_violations) * 100, 
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

# PLOT 6: Time Series of Overall System Performance (AM vs PM)
am_daily_summary <- am_weekday_data %>%
  group_by(day) %>%
  summarise(
    total_rides = sum(count),
    avg_rides = mean(count),
    .groups = "drop"
  ) %>%
  mutate(rush_period = "AM")

pm_daily_summary <- pm_weekday_data %>%
  group_by(day) %>%
  summarise(
    total_rides = sum(count),
    avg_rides = mean(count),
    .groups = "drop"
  ) %>%
  mutate(rush_period = "PM")

combined_daily <- bind_rows(am_daily_summary, pm_daily_summary)

plot6 <- ggplot(combined_daily, aes(x = day, y = total_rides, color = rush_period)) +
  geom_line(alpha = 0.7, size = 0.8) +
  geom_smooth(se = TRUE, span = 0.2, alpha = 0.2) +
  scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                     name = "Rush Period") +
  labs(
    title = "System-Level Performance Over Time",
    subtitle = "Total daily rides across all stations (with smoothed trend)",
    x = "Date",
    y = "Total Rides"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

print(plot6)

# PLOT 7: Box Plot - Variability Comparison by Season (AM vs PM)
combined_seasonal <- bind_rows(
  am_weekday_data %>% mutate(rush_period = "AM"),
  pm_weekday_data %>% mutate(rush_period = "PM")
)

plot7 <- ggplot(combined_seasonal, aes(x = season, y = count, fill = rush_period)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                    name = "Rush Period") +
  labs(
    title = "Seasonal Variability in Rush Hour Ridership",
    subtitle = "Distribution of rides by season (excluding winter)",
    x = "Season",
    y = "Number of Rides per Station per Day"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

print(plot7)

# PLOT 8: Scatter Plot - Mean vs CV (Process Capability Analysis)
combined_mean_cv <- bind_rows(
  am_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period),
  pm_station_stats %>% dplyr::select(start_code, mean_rides, CV, rush_period)
)

plot8 <- ggplot(combined_mean_cv, aes(x = mean_rides, y = CV, color = rush_period)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                     name = "Rush Period") +
  scale_x_log10() +
  annotate("text", x = max(combined_mean_cv$mean_rides), y = 55, 
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
  
  # Calculate statistics for each subgroup (month)
  subgroup_stats <- data.frame(x = x, y = y) %>%
    group_by(x) %>%
    summarise(
      mean_val = mean(y, na.rm = TRUE),
      sd_val = sd(y, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Make the initial boxplot...
  g1 = ggplot(mapping = aes(x = x, y = y, group = x)) +
    # Plot grand mean
    geom_hline(mapping = aes(yintercept = mean(y)), color = "lightgrey", size = 3) +
    # Plot points and boxplots 
    geom_jitter(height = 0, width = 0.25) +
    geom_boxplot() +
    # Add text annotations for mean and SD
    geom_text(data = subgroup_stats,
              aes(x = x, y = Inf, label = paste0("μ=", round(mean_val, 1))),
              inherit.aes = FALSE, vjust = -0.5, hjust = 0.5, size = 2.8, color = "blue") +
    geom_text(data = subgroup_stats,
              aes(x = x, y = Inf, label = paste0("σ=", round(sd_val, 1))),
              inherit.aes = FALSE, vjust = 0.5, hjust = 0.5, size = 2.8, color = "red") +
    scale_y_continuous(trans = "sqrt") +
    labs(x = xlab, y = ylab,
         subtitle = "Process Overview",
         # Add our descriptive stats in the caption!
         caption = tab$caption)
  
  # Make the histogram, but tilt it on its side
  g2 = ggplot(mapping = aes(x = y)) +
    geom_histogram(bins = 15, color = "white", fill = "grey") +
    scale_y_continuous(trans = "sqrt") +
    theme_void() +   # Clear the theme
    coord_flip()   # tilt on its side
  
  # Then bind them together into 1 plot, 'h'orizontally aligned.
  p1 = ggarrange(g1, g2, widths = c(5, 1), align = "h")
  
  return(p1)
}



d = am_2016_full %>%
  group_by(month_factor) %>%
  summarize(lower = quantile(count, prob = 0.025),
            upper = quantile(count, prob = 0.975),
            median = quantile(count, prob = 0.50))

d

ggplot() +
  geom_linerange(data = d, mapping = aes(x = month_factor, ymin = lower, ymax = upper)) +
  geom_point(data = d, mapping = aes(x = month_factor, y = median))
# crossbar


# Create 2016 AM Process Overview (Full Year)
cat("\n=== 2016 AM RUSH HOUR PROCESS OVERVIEW (FULL YEAR) ===\n")

# Calculate monthly statistics for text output
am_monthly_stats <- am_2016_full %>%
  group_by(month_factor) %>%
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    sd_count = sd(count, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

cat("\nMonthly Statistics (AM):\n")
print(am_monthly_stats %>% arrange(month_factor))

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

# Calculate monthly statistics for text output
pm_monthly_stats <- pm_2016_full %>%
  group_by(month_factor) %>%
  summarise(
    mean_count = mean(count, na.rm = TRUE),
    sd_count = sd(count, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

cat("\nMonthly Statistics (PM):\n")
print(pm_monthly_stats %>% arrange(month_factor))

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

# Save me!
# pm_2016_overview_titled %>% saveRDS("data.rds")
# pm_2016_overview_titled = readr::read_rds("data.rds")


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
# STEP 6C: CHAPTER 15 X̄–S SPC ANALYSIS
################################################################################

# Input: am_weekday_data and pm_weekday_data (already filtered to weekdays and non-winter)

# Function to implement X̄–S SPC approach
implement_xbar_s_spc <- function(data) {
  
  # Define week = format(day - wday(day) %% 7, "%Y-%U")
  # Keep only Mon-Fri inside each subgroup
  # Require >= 4 days inside a subgroup or drop that subgroup
  
  data_with_week <- data %>%
    mutate(
      weekday_num = lubridate::wday(day, week_start = 1),  # Monday = 1
      is_weekday = weekday_num <= 5  # Mon-Fri only
    ) %>%
    filter(is_weekday) %>%  # Keep only Mon-Fri
    mutate(
      # Calculate week: move back to Monday, then format as YYYY-WW
      week = format(day - (weekday_num - 1), "%Y-%U")
    )
  
  # For each (start_code, week) compute: xbar, s, nw
  # Filter to require >= 4 days in subgroup
  weekly_stats <- data_with_week %>%
    group_by(start_code, week) %>%
    summarise(
      xbar = mean(count),
      s = sd(count),
      nw = n(),
      .groups = "drop"
    ) %>%
    filter(nw >= 4)  # Require >= 4 days
  
  # For each station separately, estimate sigma_short
  # sigma_short = sqrt( sum((nw-1)*s^2) / sum(nw-1) )
  station_sigma <- weekly_stats %>%
    group_by(start_code) %>%
    summarise(
      sigma_short = sqrt(sum((nw - 1) * s^2, na.rm = TRUE) / sum(nw - 1, na.rm = TRUE)),
      grand_mean = mean(xbar, na.rm = TRUE),
      typical_nw = median(nw, na.rm = TRUE),
      n_weeks = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(sigma_short) & sigma_short > 0)  # Remove invalid estimates
  
  # Build X̄–chart data with control limits
  # UCL = grand_mean + 3 * sigma_short / sqrt(typical_nw)
  # LCL = grand_mean - 3 * sigma_short / sqrt(typical_nw)
  
  # Select only needed columns from station_sigma
  station_params <- station_sigma %>%
    dplyr::select(start_code, sigma_short, grand_mean, typical_nw)
  
  xbar_chart_data <- weekly_stats %>%
    left_join(station_params, by = "start_code") %>%
    mutate(
      UCL = grand_mean + 3 * sigma_short / sqrt(typical_nw),
      LCL = grand_mean - 3 * sigma_short / sqrt(typical_nw),
      is_violation = (xbar > UCL | xbar < LCL)
    )
  
  return(list(
    weekly_stats = xbar_chart_data,
    station_params = station_sigma
  ))
}

# Apply to AM data
cat("\n=== Implementing X̄–S SPC Analysis for AM ===\n")
am_spc_results <- implement_xbar_s_spc(am_weekday_data)
am_spc_data <- am_spc_results$weekly_stats
am_spc_params <- am_spc_results$station_params

# Find the station with largest #weeks for the example
top_am_station <- am_spc_params %>%
  arrange(desc(n_weeks)) %>%
  slice_head(n = 1)

cat("Example AM station:", top_am_station$start_code, 
    "with", top_am_station$n_weeks, "weeks\n")

# Extract data for the top station
top_am_data <- am_spc_data %>%
  filter(start_code == top_am_station$start_code) %>%
  arrange(week)

# X̄–chart for the example station
cat("\nCreating X̄–chart...\n")
xbar_chart <- ggplot(top_am_data, aes(x = seq_along(xbar), y = xbar)) +
  geom_hline(yintercept = top_am_station$grand_mean, 
             color = "blue", linetype = "solid", linewidth = 1) +
  geom_hline(yintercept = top_am_station$grand_mean + 3 * top_am_station$sigma_short / sqrt(top_am_station$typical_nw), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = top_am_station$grand_mean - 3 * top_am_station$sigma_short / sqrt(top_am_station$typical_nw), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(aes(color = is_violation), size = 2, alpha = 0.7) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     name = "Violation",
                     labels = c("FALSE" = "In Control", "TRUE" = "Out of Control")) +
  labs(
    title = paste("X̄–Chart: Station", top_am_station$start_code, "AM Rush Hour"),
    subtitle = paste("Control limits: UCL/LCL = mean ± 3σ/√n"),
    x = "Week Number",
    y = "Sample Mean (x̄)",
    caption = paste("Grand mean:", round(top_am_station$grand_mean, 2),
                   "| σ_short:", round(top_am_station$sigma_short, 2),
                   "| Typical n:", round(top_am_station$typical_nw, 1))
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(xbar_chart)

# S-chart for the same station
cat("Creating S-chart...\n")

# Get typical subgroup size for proper S-chart constants
typical_n <- round(top_am_station$typical_nw)

# S-chart constants based on subgroup size
# For n around 5: c4 ≈ 0.94, B4 ≈ 2.089, B3 = 0
# Using approximate values
c4_values <- c(1, 0.798, 0.886, 0.921, 0.940, 0.952, 0.959, 0.965)
B4_values <- c(0, 0, 0, 0, 2.089, 2.011, 1.966, 1.928)

n_index <- min(max(typical_n, 1), 8)
c4 <- c4_values[n_index]
B4 <- B4_values[n_index]

center_line_S <- c4 * top_am_station$sigma_short
UCL_S <- if (B4 > 0) B4 * top_am_station$sigma_short else center_line_S + 3 * top_am_station$sigma_short
LCL_S <- 0

s_chart <- ggplot(top_am_data, aes(x = seq_along(s), y = s)) +
  geom_hline(yintercept = center_line_S, 
             color = "blue", linetype = "solid", linewidth = 1) +
  geom_hline(yintercept = UCL_S, 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = LCL_S, 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(aes(color = (s > UCL_S | s < LCL_S)), size = 2, alpha = 0.7) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     name = "Violation",
                     labels = c("FALSE" = "In Control", "TRUE" = "Out of Control")) +
  labs(
    title = paste("S-Chart: Station", top_am_station$start_code, "AM Rush Hour"),
    subtitle = "Within-subgroup standard deviations",
    x = "Week Number",
    y = "Sample Standard Deviation (s)",
    caption = paste("Center: ", round(center_line_S, 2),
                   "| UCL_S:", round(UCL_S, 2),
                   "| Subgroup n:", typical_n)
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(s_chart)

cat("\nX̄–S SPC analysis complete for AM rush hour!\n")

################################################################################
# STEP 7: Summary Report
################################################################################

cat("\n")
cat("================================================================================\n")
cat("                    SIX SIGMA PROCESS ANALYSIS SUMMARY                         \n")
cat("================================================================================\n")
cat("Date Range:", as.character(min(am_weekday_data$day)), "to", as.character(max(am_weekday_data$day)), "\n")
cat("Analysis Scope: Weekdays only, excluding winter months (Dec-Feb)\n")
cat("\n")

cat("======================== AM RUSH HOUR STATISTICS ========================\n")
cat("\n")
cat("OVERALL STATISTICS (AM):\n")
cat("  Total stations analyzed:", nrow(am_station_stats), "\n")
cat("  Average rides per station per day:", round(mean(am_station_stats$mean_rides), 2), "\n")
cat("  Median CV across stations:", round(median(am_station_stats$CV), 2), "%\n")
cat("\n")
cat("PROCESS CAPABILITY (AM):\n")
cat("  Stations with HIGH variability (CV > 50%):", nrow(am_high_variability), 
    sprintf("(%.1f%%)\n", 100 * nrow(am_high_variability) / nrow(am_station_stats)))
cat("  Stations with MODERATE variability (CV 25-50%):", 
    nrow(am_station_stats %>% filter(CV >= 25 & CV <= 50)),
    sprintf("(%.1f%%)\n", 100 * nrow(am_station_stats %>% filter(CV >= 25 & CV <= 50)) / nrow(am_station_stats)))
cat("  Stations with LOW variability (CV < 25%):", 
    nrow(am_station_stats %>% filter(CV < 25)),
    sprintf("(%.1f%%)\n", 100 * nrow(am_station_stats %>% filter(CV < 25)) / nrow(am_station_stats)))
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
cat("  Average rides per station per day:", round(mean(pm_station_stats$mean_rides), 2), "\n")
cat("  Median CV across stations:", round(median(pm_station_stats$CV), 2), "%\n")
cat("\n")
cat("PROCESS CAPABILITY (PM):\n")
cat("  Stations with HIGH variability (CV > 50%):", nrow(pm_high_variability), 
    sprintf("(%.1f%%)\n", 100 * nrow(pm_high_variability) / nrow(pm_station_stats)))
cat("  Stations with MODERATE variability (CV 25-50%):", 
    nrow(pm_station_stats %>% filter(CV >= 25 & CV <= 50)),
    sprintf("(%.1f%%)\n", 100 * nrow(pm_station_stats %>% filter(CV >= 25 & CV <= 50)) / nrow(pm_station_stats)))
cat("  Stations with LOW variability (CV < 25%):", 
    nrow(pm_station_stats %>% filter(CV < 25)),
    sprintf("(%.1f%%)\n", 100 * nrow(pm_station_stats %>% filter(CV < 25)) / nrow(pm_station_stats)))
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




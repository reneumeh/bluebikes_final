# Blue Bikes Data Export to CSV (Shortened Version)
# This script extracts important data from the BlueBikes database
# and exports it to CSV files, one per year, for easy and fast data access
# This version limits each year to the first 1000 rows

# Load required libraries
library(dplyr)
library(RSQLite)
library(DBI)
library(lubridate)
library(readr)

# Connect to database
cat("Connecting to database...\n")
con <- dbConnect(RSQLite::SQLite(), dbname = "bluebikes.sqlite")

# Check available date range
cat("\nChecking available date range in database...\n")
date_range <- dplyr::tbl(con, "tally_rush_edges") %>%
  dplyr::summarise(
    min_date = min(day, na.rm = TRUE),
    max_date = max(day, na.rm = TRUE)
  ) %>%
  dplyr::collect()

cat("Available date range:", as.character(date_range$min_date), "to", as.character(date_range$max_date), "\n")

################################################################################
# STEP 1: Extract AM and PM Rush Hour Data
################################################################################

cat("\nExtracting rush hour data from database...\n")

# Get all AM rush hour data
am_rush_data <- dplyr::tbl(con, "tally_rush_edges") %>%
  dplyr::filter(rush == "am") %>%
  dplyr::collect() %>%
  dplyr::mutate(day = as.Date(day))

cat("AM rush hour records collected:", nrow(am_rush_data), "\n")

# Get all PM rush hour data
pm_rush_data <- dplyr::tbl(con, "tally_rush_edges") %>%
  dplyr::filter(rush == "pm") %>%
  dplyr::collect() %>%
  dplyr::mutate(day = as.Date(day))

cat("PM rush hour records collected:", nrow(pm_rush_data), "\n")

# Check if we have data
if (nrow(am_rush_data) == 0 && nrow(pm_rush_data) == 0) {
  stop("ERROR: No data found in database. Please check database connection and data availability.")
}

################################################################################
# STEP 2: Combine AM and PM Data and Add Metadata
################################################################################

cat("\nCombining AM and PM data and adding metadata...\n")

# Combine AM and PM data
combined_data <- dplyr::bind_rows(am_rush_data, pm_rush_data)

# Add comprehensive metadata - do this in steps to avoid column reference issues
combined_data <- combined_data %>%
  dplyr::mutate(
    # Date information first
    Date = day,
    Year = as.numeric(format(day, "%Y")),
    Month = as.numeric(format(day, "%m")),
    Month_Name = format(day, "%B"),
    Day_of_Week = weekdays(day),
    Day_of_Month = as.numeric(format(day, "%d"))
  ) %>%
  dplyr::mutate(
    # Rush period
    Rush_Period = dplyr::case_when(
      rush == "am" ~ "AM",
      rush == "pm" ~ "PM",
      TRUE ~ as.character(rush)
    )
  ) %>%
  dplyr::mutate(
    # Time information (uses Day_of_Week created above)
    Is_Weekend = Day_of_Week %in% c("Saturday", "Sunday")
  ) %>%
  dplyr::mutate(
    # Season (uses Month created above)
    Season = dplyr::case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  dplyr::mutate(
    # Number of rides and station information
    Number_of_Rides = count,
    Start_Station_Code = start_code,
    End_Station_Code = end_code
  )

# Now select and reorder columns for the CSV
combined_data <- combined_data %>%
  dplyr::select(
    Year,
    Month,
    Month_Name,
    Date,
    Day_of_Week,
    Day_of_Month,
    Season,
    Rush_Period,
    Is_Weekend,
    Start_Station_Code,
    End_Station_Code,
    Number_of_Rides
  ) %>%
  dplyr::arrange(Year, Month, Date, Rush_Period, Start_Station_Code)

cat("Total records after combining:", nrow(combined_data), "\n")

################################################################################
# STEP 3: Create ShortenedCSV Folder
################################################################################

cat("\nCreating ShortenedCSV folder...\n")
if (!dir.exists("ShortenedCSV")) {
  dir.create("ShortenedCSV")
  cat("Created ShortenedCSV folder.\n")
} else {
  cat("ShortenedCSV folder already exists.\n")
}

################################################################################
# STEP 4: Split Data by Year and Export to CSV
################################################################################

cat("\nSplitting data by year and exporting to CSV files...\n")

# Get unique years
years <- sort(unique(combined_data$Year))
cat("Years found:", paste(years, collapse = ", "), "\n")

# Export CSV for each year (limited to first 1000 rows per year)
for (year in years) {
  cat("\nProcessing year", year, "...\n")
  
  # Filter data for this year
  year_data <- combined_data %>%
    dplyr::filter(Year == year)
  
  original_count <- nrow(year_data)
  cat("  Original records for", year, ":", original_count, "\n")
  
  # Limit to first 1000 rows
  year_data <- year_data %>%
    dplyr::slice_head(n = 1000)
  
  cat("  Limited to first", nrow(year_data), "records\n")
  
  # Create filename
  filename <- paste0("ShortenedCSV/BlueBikes_", year, ".csv")
  
  # Write to CSV
  write_csv(year_data, filename)
  
  cat("  Exported to:", filename, "\n")
  
  # Print summary statistics
  year_summary <- year_data %>%
    dplyr::group_by(Rush_Period, Month_Name) %>%
    dplyr::summarise(
      Total_Rides = sum(Number_of_Rides, na.rm = TRUE),
      Avg_Rides_Per_Day = mean(Number_of_Rides, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("  Summary for", year, ":\n")
  print(year_summary)
}

################################################################################
# STEP 5: Create Summary Files (based on shortened data)
################################################################################

cat("\nCreating summary files based on shortened data...\n")

# Create shortened combined data (first 1000 rows per year) for summaries
shortened_combined_data <- combined_data %>%
  dplyr::group_by(Year) %>%
  dplyr::slice_head(n = 1000) %>%
  dplyr::ungroup()

cat("Shortened data for summaries:", nrow(shortened_combined_data), "rows\n")

# Overall summary by year
summary_by_year <- shortened_combined_data %>%
  dplyr::group_by(Year, Rush_Period) %>%
  dplyr::summarise(
    Total_Rides = sum(Number_of_Rides, na.rm = TRUE),
    Avg_Rides_Per_Day = mean(Number_of_Rides, na.rm = TRUE),
    Median_Rides_Per_Day = median(Number_of_Rides, na.rm = TRUE),
    Min_Rides_Per_Day = min(Number_of_Rides, na.rm = TRUE),
    Max_Rides_Per_Day = max(Number_of_Rides, na.rm = TRUE),
    Total_Days = dplyr::n_distinct(Date),
    Total_Stations = dplyr::n_distinct(Start_Station_Code),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Year, Rush_Period)

write_csv(summary_by_year, "ShortenedCSV/Summary_By_Year.csv")
cat("Created summary file: ShortenedCSV/Summary_By_Year.csv\n")

# Summary by year and month
summary_by_year_month <- shortened_combined_data %>%
  dplyr::group_by(Year, Month, Month_Name, Rush_Period) %>%
  dplyr::summarise(
    Total_Rides = sum(Number_of_Rides, na.rm = TRUE),
    Avg_Rides_Per_Day = mean(Number_of_Rides, na.rm = TRUE),
    Total_Days = dplyr::n_distinct(Date),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Year, Month, Rush_Period)

write_csv(summary_by_year_month, "ShortenedCSV/Summary_By_Year_Month.csv")
cat("Created summary file: ShortenedCSV/Summary_By_Year_Month.csv\n")

# Summary by station
summary_by_station <- shortened_combined_data %>%
  dplyr::group_by(Start_Station_Code, Rush_Period) %>%
  dplyr::summarise(
    Total_Rides = sum(Number_of_Rides, na.rm = TRUE),
    Avg_Rides_Per_Day = mean(Number_of_Rides, na.rm = TRUE),
    Total_Days = dplyr::n_distinct(Date),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Start_Station_Code, Rush_Period)

write_csv(summary_by_station, "ShortenedCSV/Summary_By_Station.csv")
cat("Created summary file: ShortenedCSV/Summary_By_Station.csv\n")

################################################################################
# STEP 6: Final Summary
################################################################################

cat("\n" , rep("=", 70), "\n", sep = "")
cat("EXPORT COMPLETE!\n")
cat(rep("=", 70), "\n", sep = "")
cat("\nFiles created in ShortenedCSV folder:\n")
cat("\n")

# List all CSV files created
csv_files <- list.files("ShortenedCSV", pattern = "\\.csv$", full.names = TRUE)
for (file in csv_files) {
  file_size <- file.info(file)$size / 1024  # Size in KB
  cat("  -", basename(file), sprintf("(%.1f KB)\n", file_size))
}

cat("\n")
cat("Total years exported:", length(years), "\n")
cat("Total records exported (limited to 1000 per year):", nrow(shortened_combined_data), "\n")
cat("\n")
cat("To load data for a specific year, use:\n")
cat("  data_2016 <- read_csv('ShortenedCSV/BlueBikes_2016.csv')\n")
cat("\n")
cat(rep("=", 70), "\n", sep = "")

# Close database connection
dbDisconnect(con)

cat("\nDatabase connection closed.\n")


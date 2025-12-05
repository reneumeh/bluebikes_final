# Blue Bikes Boston - Six Sigma Analysis Dashboard

A comprehensive Shiny dashboard application for analyzing and visualizing Blue Bikes Boston station optimization using Six Sigma methodology.

## Overview

This project presents a Six Sigma analysis of Blue Bikes Boston station operations, focusing on rush hour optimization and capacity management. The dashboard provides interactive visualizations and detailed analysis of station performance, violations, and financial impacts.

## Project Team

- Nevin Motto
- Rene Umeh
- Daniel Carlson
- Evelyne Morisseau
- Charlie Gagliardo

## Features

- **Project Overview**: Team information, project charter, goals, scope, and timeline
- **Research Question**: Concise research question and objectives
- **Data**: Brief bullets on data sources and processing
- **Method**: Detailed SPC framework explanation with SIPOC and Process Map
- **Results**: Key findings with interactive visualizations and quantities of interest
- **Discussion**: Implications and recommendations
- **Financial Impacts**: Financial context and projected revenue impacts ($543,000 - $814,000 annually)
- **References**: Complete bibliography of sources

## Key Findings

- **Primary Quantity of Interest**: 2.51 rides gained per optimized station per day
- **95% Confidence Interval**: [1.74, 3.28] rides per station per day
- **Financial Impact**: $543,000 - $814,000 annual additional revenue
- **Average Ridership**: AM (2.18) and PM (2.52) per station

## Installation

### Required R Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard", 
  "DT",
  "ggplot2",
  "dplyr",
  "RSQLite",
  "DBI",
  "lubridate",
  "viridis",
  "plotly",
  "readr",
  "data.table",
  "geosphere",
  "zoo",
  "qcc",
  "boot"
))
```

## Running the App

### Option 1: RStudio
Open `AppDisplay.R` in RStudio and click the "Run App" button.

### Option 2: Command Line
```r
shiny::runApp("AppDisplay.R")
```

## Project Structure

```
BlueBicycles/
├── AppDisplay.R                    # Main Shiny application
├── BlueBikes_Code2.R               # Analysis code
├── DataCSV/                        # CSV data files (2011-2021)
│   ├── BlueBikes_2011.csv
│   ├── BlueBikes_2012.csv
│   ├── ...
│   └── Summary_By_Station.csv
├── Images/                         # Image files for dashboard
│   ├── SIPOC.png
│   ├── ProcessMap.png
│   └── VOC.png
├── am_station_statistics.rds       # Morning station statistics
├── pm_station_statistics.rds       # Evening station statistics
├── am_violations_by_station.rds    # Morning violations data
├── pm_violations_by_station.rds    # Evening violations data
├── stationbg_dataset.rds            # Station background dataset
├── dates.rds                        # Date references
├── SHINY_APP_README.md             # Detailed app documentation
├── APP_FEATURES.md                 # Features summary
├── APP_UPDATES_SUMMARY.md          # Updates summary
└── README.md                        # This file
```

## Visualizations

The dashboard includes:

1. **Process Variability Distribution**: CV distribution across stations (AM vs PM)
2. **Bootstrap Distribution**: Rides gained per optimized station with confidence intervals
3. **SIPOC Diagram**: Process mapping visualization
4. **Process Map**: Detailed process flow
5. **Voice of the Customer (VOC)**: Customer feedback analysis

## Data Sources

- Blue Bikes Boston trip data (2011-2021)
- Station-level statistics and violations
- Rush hour analysis (AM and PM periods)

## Methodology

The project uses Statistical Process Control (SPC) framework to:
- Analyze station variability using Coefficient of Variation (CV)
- Identify stations with high variability (>50% CV)
- Calculate control limits and process capability
- Perform bootstrap analysis for confidence intervals
- Project financial impacts based on optimization

## License

MIT License - see [LICENSE](LICENSE) file for details

## Documentation

For more detailed information, see:
- [SHINY_APP_README.md](SHINY_APP_README.md) - Complete app documentation
- [APP_FEATURES.md](APP_FEATURES.md) - Features summary
- [APP_UPDATES_SUMMARY.md](APP_UPDATES_SUMMARY.md) - Updates and implementation details

## Support

For issues or questions about the app functionality, refer to:
- Shiny documentation: https://shiny.rstudio.com/
- Shinydashboard documentation: https://rstudio.github.io/shinydashboard/

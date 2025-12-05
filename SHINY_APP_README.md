# Blue Bikes Boston - Six Sigma Analysis Dashboard

## Overview
This Shiny dashboard application displays the comprehensive Six Sigma analysis project for Blue Bikes Boston station optimization. The app is organized into multiple sections covering all aspects of the project from problem statement to financial impacts.

## Features
- **Project Overview**: Team information, project charter, goals, scope, and timeline
- **Problem Statement & Business Case**: Detailed analysis of current issues and business justification
- **Methodology**: Research design, SPC framework, and analytical methods
- **Results & Analysis**: Preliminary findings from the Six Sigma analysis
- **Financial Impacts**: Financial context and projected revenue impacts
- **References**: Complete bibliography of sources

## Installation

### Required Packages
The app requires the following R packages. They will be automatically installed if not already present:

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
  "plotly"
))
```

Or simply run the app - it will check and install missing packages automatically.

## Running the App

### Option 1: Direct Run
Simply open `AppDisplay.R` in RStudio and click the "Run App" button, or run:

```r
source("AppDisplay.R")
```

### Option 2: Command Line
```r
shiny::runApp("AppDisplay.R")
```

## App Structure

### Tab 1: Project Overview
- Project charter information
- Team member roles
- Project goals and objectives
- Scope definition (in-scope vs. out-of-scope)
- Project timeline with planned vs. actual completion dates

### Tab 2: Problem Statement & Business Case
- Detailed problem statement
- Key issues during rush hours
- Business case and strategic goals
- Expected benefits
- Consequences of inaction
- Alignment with business initiatives

### Tab 3: Methodology
- Research design template
- Outcome variables and research questions
- Measures and data sources
- Statistical Process Control (SPC) framework
- Analysis approach
- Specific methods used

### Tab 4: Results & Analysis
- Preliminary results summary
- Morning (AM) rush hour findings
- Evening (PM) rush hour findings
- Discussion and recommendations
- Visualization placeholders (ready for integration with analysis code)

### Tab 5: Financial Impacts
- Current BlueBikes financial situation
- Bootstrap analysis results
- Projected revenue impacts ($543,000 - $814,000 annually)
- Strategic expansion context
- Contract renewal considerations

### Tab 6: References
- Complete bibliography with clickable links
- All citations from the project document

## Customization

### Adding Visualizations
To integrate actual visualizations from your analysis code (`BlueBikes_Code2.R`):

1. Save your plots as R objects or image files
2. Add plot outputs in the Results tab section
3. Use `renderPlot()` or `renderPlotly()` for interactive charts

Example:
```r
output$my_plot <- renderPlotly({
  # Your plot code here
  plot_ly(data, x = ~x, y = ~y, type = 'scatter')
})
```

### Adding Data Tables
To display actual analysis results as data tables:

1. Load your saved data files (e.g., `am_station_statistics.rds`)
2. Add `DT::renderDataTable()` outputs in the server function
3. Reference them in the UI with `DT::dataTableOutput()`

## Data Files
If you want to integrate actual analysis data, ensure the following files are accessible:
- `am_station_statistics.rds`
- `pm_station_statistics.rds`
- `am_violations_by_station.rds`
- `pm_violations_by_station.rds`
- `bluebikes.sqlite` (database file)

## Notes
- The app is designed to work independently without requiring the database connection
- All project information is embedded in the app structure
- The visualization section includes placeholders ready for integration with your analysis results
- All styling is consistent with a professional dashboard appearance

## Troubleshooting

### Package Installation Issues
If you encounter package installation errors, try installing packages individually:
```r
install.packages("shinydashboard")
install.packages("DT")
# etc.
```

### Port Conflicts
If the default port (3838) is busy, RStudio will automatically use another port. You can also specify a port:
```r
shiny::runApp("AppDisplay.R", port = 8080)
```

## Support
For issues or questions about the app functionality, refer to:
- Shiny documentation: https://shiny.rstudio.com/
- Shinydashboard documentation: https://rstudio.github.io/shinydashboard/


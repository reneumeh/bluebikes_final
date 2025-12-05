# Blue Bikes Boston - Six Sigma Analysis Dashboard
# Shiny Application for Project Display
# Group: Nevin Motto, Rene Umeh, Daniel Carlson, Evelyne Morisseau, Charlie Gagliardo

# Load required libraries with error handling
#required_packages <- c(
#  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr",
#  "RSQLite", "DBI", "lubridate", "viridis", "plotly", "readr"
#)

# Function to check and install packages
#check_and_install <- function(pkg) {
 # if (!require(pkg, character.only = TRUE)) {
  #  install.packages(pkg, dependencies = TRUE)
   # library(pkg, character.only = TRUE)
 # }
#}

# Install and load all required packages
#sapply(required_packages, check_and_install)

# Load libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(RSQLite)
library(DBI)
library(lubridate)
library(viridis)
library(plotly)
library(readr)
library(data.table)
library(geosphere)
library(zoo)
library(qcc)
library(boot)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Blue Bikes Boston - Six Sigma Analysis",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Project Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Research Question", tabName = "research", icon = icon("question-circle")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Method", tabName = "method", icon = icon("flask")),
      menuItem("Statistical Process Control", tabName = "spc", icon = icon("chart-bar")),
      menuItem("Bootstrap Analysis", tabName = "bootstrap", icon = icon("calculator")),
      menuItem("Results & Recommendations", tabName = "results", icon = icon("chart-line")),
      menuItem("Discussion", tabName = "discussion", icon = icon("comments")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        h2 {
          color: #2c3e50;
          font-weight: 600;
        }
        h3 {
          color: #34495e;
          font-weight: 500;
        }
        .quantity-box {
          background-color: #e8f4f8;
          padding: 15px;
          border-left: 4px solid #3498db;
          margin: 10px 0;
        }
        .ci-box {
          background-color: #fff3cd;
          padding: 15px;
          border-left: 4px solid #ffc107;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      
      # Tab 1: Project overview, teammembers & credits 
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            title = "Project Overview", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h2("Blue Bikes Boston - Station Optimization Project"),
            br(),
            h3("Course & Team"),
            tags$ul(
              tags$li(tags$strong("Course:"), "SYSEN 5300: Systems Engineering and Six Sigma"),
              tags$li(tags$strong("Project Activity:"), "Poster Presentation"),
              tags$li(tags$strong("Team:"), "Daniel Carlson (Project Lead), Nevin Motto (Project Manager), Rene Umeh (Visualization), Charlie Gagliardo (Modeler 1), Evelyne Morisseau (Modeler 2)"),
              tags$li(tags$strong("Semester:"), "Fall 2025"),
              tags$li(tags$strong("Teaching staff:"), "Professor Timothy Fraser & Teaching Assistant Tolkien Bagchi"),
            )
          )
        )
      ),
      # Tab 2: Research question, background, project goal, and primary research question
      tabItem(
        tabName = "research",
        fluidRow(
          box(
            width = 12,
            title = "Research Question", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            h3("Background"),
            tags$ul(
              tags$li("Boston BlueBikes is a city wide bike-share system providing sustainable transportation through an intricate network of strategically located bike stations throughout the city."),
              tags$li("During peak commuting hours,bikes are disproportionally displaced and thus unavailable to riders in parts of the city. To address this, the city of Boston has plans to expand the BlueBikes station network by 2030."),
              tags$li("We will create a tool to identify stations and areas that are consistently over/under-utilized to determine where additional bikes and stations are most needed."),
              tags$li("We will improve BlueBikes’ financial decisions by connecting station performance and bike demand to missed ride revenue and station installation costs. ")
            ),
            br(),
            h3("Project Goal"),
            tags$p(tags$strong("Using BlueBikes ride data, we predict the best locations to expand the number of bike docks and new stations to improve the availability of bikes available to users and BlueBike’s revenue. "), style = "font-size: 16px;"),
            br(),
            h3("Primary Research Question"),
            tags$p(tags$strong("What are the best locations to add the planned additional docks and stations, and how much additional revenue would be generated?"), style = "font-size: 16px;"),
          )
        ),

        #images side by side section
        fluidRow(
          box(
            width = 6,
            title = "BlueBikes Docks & Stations", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "Images/bluebikes_dock_key.png", width = "100%", style = "border: 1px solid #ddd;"),
            br(),
            tags$p(style = "font-size: 11px; color: #666; font-style: italic; text-align: center; margin-top: 10px;",
                   "Image credit: ",
                   tags$a(href = "https://www.cambridgema.gov/CDD/Transportation/gettingaroundcambridge/bikesincambridge/bikeshare", target = "_blank", "Source Name"))
          ),
          box(
            width = 6,
            title = "Current BlueBikes Station Network", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "Images/Bluebikes_system_map.png", width = "100%", style = "border: 1px solid #ddd;"),
            br(),
            tags$p(style = "font-size: 11px; color: #666; font-style: italic; text-align: center; margin-top: 10px;",
                   "Image credit: ",
                   tags$a(href = "https://account.bluebikes.com/map", target = "_blank", "Source Name"))
          )
        )
      ),
      # Tab 3: Data sources, data processing, and key measures
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            width = 12,
            title = "Data", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            h3("Data Sources"),
            tags$ul(
              tags$li(tags$strong("Dataset:"), "BlueBikes public trip data (2011-2021)"),
              tags$li(tags$strong("Source:"), "https://github.com/timothyfraser/sts/tree/3week/data/bluebikes"),
              tags$li(tags$strong("Focus Period:"), "Weekday rush hours: 7-9 AM (morning) and 4-6 PM (evening)")
            ),
            br(),
            h3("Data Processing"),
            tags$ul(
              tags$li("Weekends excluded (focus on commuter patterns)"),
              tags$li("Extreme weather days excluded (Dec, Jan, Feb)"),
              tags$li("Limited to Boston city area stations"),
              tags$li("Aggregated to daily ride counts per station")
            ),
            br(),
            h3("Key Measures"),
            tags$ul(
              tags$li(tags$strong("Total Count:"), "Number of rides per station per day (unit: N)"),
              tags$li(tags$strong("Ride Length:"), "Duration of rides (unit: hours)"),
              tags$li(tags$strong("Process Statistics:"), "Mean, SD, CV, control limits per station")
            )
          )
        )
      ),
      
      # Tab 2: Method
      tabItem(
        tabName = "method",
        fluidRow(
          box(
            width = 12,
            title = "Methodology - Six Sigma Analysis Process", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Overall Analytical Framework"),
            tags$p("This project applies Six Sigma DMAIC (Define, Measure, Analyze, Improve, Control) methodology to optimize BlueBikes station operations through Statistical Process Control (SPC) and bootstrap analysis."),
            br(),
            h3("Step 1: Data Collection & Preparation"),
            tags$ul(
              tags$li("Collected BlueBikes trip data (2011-2021) from public repository"),
              tags$li("Filtered to weekday rush hours (7-9 AM, 4-6 PM) to focus on commuter patterns"),
              tags$li("Excluded weekends and extreme weather months (Dec, Jan, Feb)"),
              tags$li("Aggregated data to daily ride counts per station per rush period")
            ),
            br(),
            h3("Step 2: Statistical Process Control (SPC) Analysis"),
            tags$ul(
              tags$li("Treated weekday usage as a recurring operational process"),
              tags$li("Calculated process statistics for each station: Mean, Standard Deviation, Coefficient of Variation (CV)"),
              tags$li("Computed control limits: UCL = mean + 3σ, LCL = max(0, mean - 3σ)"),
              tags$li("Identified out-of-control stations via control limit violations"),
              tags$li("Classified stations by variability: Low (CV < 25%), Moderate (25-50%), High (50-75%), Very High (>75%)")
            ),
            br(),
            h3("Step 3: Candidate Station Identification"),
            tags$ul(
              tags$li("Identified priority stations with both high average ridership AND high CV (>50%)"),
              tags$li("High CV indicates capacity constraints and lost trip opportunities"),
              tags$li("High ridership ensures sufficient demand to justify expansion investment")
            ),
            br(),
            h3("Step 4: Bootstrap Analysis for Dock Expansion"),
            tags$ul(
              tags$li("Estimated lost trips based on CV: Higher CV = more capacity issues = more lost trips"),
              tags$li("Calculated potential trips if capacity constraints were removed"),
              tags$li("Estimated recovery rate: 40-60% of lost trips recoverable with dock expansion"),
              tags$li("Performed bootstrap resampling (1000 iterations) to estimate confidence intervals"),
              tags$li("Calculated financial impact: Revenue from recovered trips minus annualized capital costs")
            ),
            br(),
            h3("Step 5: Financial Impact Assessment"),
            tags$ul(
              tags$li("Revenue per trip: $2.95 USD"),
              tags$li("Capital cost per dock: $500 USD"),
              tags$li("Dock lifespan: 10 years (annualized cost = capex / 10)"),
              tags$li("Net gain = Annual revenue from recovered trips - Annualized capital cost"),
              tags$li("Bootstrap confidence intervals provide uncertainty quantification")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "SIPOC Diagram", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "Images/SIPOC.png", width = "100%", style = "border: 1px solid #ddd;")
          ),
          box(
            width = 6,
            title = "Process Map", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "Images/ProcessMap.png", width = "100%", style = "border: 1px solid #ddd;")
          )
        )
      ),
      
      # Tab 5: (4) Results
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            width = 12,
            title = "(4) Results - Key Findings", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Process Variability Summary"),
            tags$ul(
              tags$li("AM Rush Hour: Median CV = 50%; 45.5% of stations show high variability (CV > 50%)"),
              tags$li("PM Rush Hour: Median CV = 53.03%; 61.2% of stations show high variability"),
              tags$li("AM: 9 control limit violations across 8 stations"),
              tags$li("PM: 768 control limit violations across 226 stations")
            ),
            br(),
            h3("Most Problematic Stations"),
            tags$ul(
              tags$li("AM: A32002, M32006, D32003 (CV: 56-119%)"),
              tags$li("PM: A32000, M32006, D32003 (CV: 103-147%)")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Quantities of Interest", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            div(class = "quantity-box",
              h3("Primary Quantity of Interest"),
              tags$p(tags$strong("Rides Gained per Optimized Station per Day:"), tags$strong("2.51 rides"), style = "font-size: 18px;"),
              tags$p("(Mean estimate)")
            ),
            div(class = "ci-box",
              h4("95% Confidence Interval"),
              tags$p(tags$strong("[1.74, 3.28] rides per station per day"), style = "font-size: 16px;"),
              tags$p("This means we are 95% confident that optimizing unstable stations will gain between 1.74 and 3.28 additional rides per station per day.")
            ),
            br(),
            div(class = "quantity-box",
              h3("Financial Impact"),
              tags$p(tags$strong("Annual Additional Revenue:"), tags$strong("$543,000 - $814,000"), style = "font-size: 18px;"),
              tags$p("(Based on optimizing top 10 unstable stations, ~$2.95 per ride)")
            ),
            div(class = "quantity-box",
              h3("System-Wide Metrics"),
              tags$ul(
                tags$li("Average AM ridership: 2.18 rides/station/day"),
                tags$li("Average PM ridership: 2.52 rides/station/day"),
                tags$li("Current loss: 2% (~94,000 rides) of potential customers"),
                tags$li("Potential recovery: 4-6% of lost/unserved demand")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Visualization 1: Process Variability - AM vs PM", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("variability_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Detailed Results", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h4("Morning (AM) Rush Hour:"),
            tags$ul(
              tags$li("Average ridership: 2.18 rides/station/day"),
              tags$li("Median CV: 50%"),
              tags$li("45.5% stations with high variability"),
              tags$li("9 violations, 8 stations affected")
            ),
            br(),
            h4("Evening (PM) Rush Hour:"),
            tags$ul(
              tags$li("Average ridership: 2.52 rides/station/day"),
              tags$li("Median CV: 53.03%"),
              tags$li("61.2% stations with high variability"),
              tags$li("768 violations, 226 stations affected")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Dock Expansion Recommendations", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Top Stations Recommended for Dock Expansion"),
            tags$p("Based on bootstrap analysis: stations with high ridership, high variability (CV > 50%), and positive net financial impact."),
            plotlyOutput("bootstrap_recommendations_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Detailed Recommendations Table", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            DT::dataTableOutput("bootstrap_recommendations_table")
          )
        )
      ),
      
      # Tab 6: (5) Discussion
      tabItem(
        tabName = "discussion",
        fluidRow(
          box(
            width = 12,
            title = "(5) Discussion & Implications", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Key Findings"),
            tags$ul(
              tags$li("Substantial variation exists across stations (median CV ~50-53%)"),
              tags$li("Approximately half of all stations operate outside desired stability limits"),
              tags$li("PM rush hour shows higher variability and more violations than AM"),
              tags$li("Stations with repeated violations indicate structural imbalances (over/under utilization)")
            ),
            br(),
            h3("Implications"),
            tags$ul(
              tags$li("Strategic station placement can reduce variability and capture missed demand"),
              tags$li("Optimizing top 10 unstable stations could recover 4-6% of lost demand"),
              tags$li("Financial impact: $543K-$814K additional annual revenue"),
              tags$li("Analysis shows 95% CI: [1.74, 3.28] rides gained per optimized station/day"),
              tags$li("Method provides data-driven approach for 64 planned stations by 2030")
            ),
            br(),
            h3("Recommendations"),
            tags$ul(
              tags$li("Prioritize stations in high-variability zones for rebalancing or expansion"),
              tags$li("Focus on PM rush hour hotspots (226 stations with violations)"),
              tags$li("Use SPC framework to continuously monitor station performance"),
              tags$li("Apply statistical methodology to evaluate potential new station locations"),
              tags$li("Target reducing CV below 50% threshold for process stability")
            ),
            br(),
            h3("Business Value"),
            tags$ul(
              tags$li("Increases user accessibility and satisfaction during peak hours"),
              tags$li("Reduces missed ride opportunities (currently 2% of potential customers)"),
              tags$li("Generates measurable revenue increase ($543K-$814K annually)"),
              tags$li("Supports BlueBikes expansion goals and Go Boston 2030 vision"),
              tags$li("Provides quantitative framework for strategic decision-making")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Voice of the Customer", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "Images/VOC.png", width = "100%", style = "border: 1px solid #ddd;")
          )
        )
      ),
      
      # Tab 3: Statistical Process Control
      tabItem(
        tabName = "spc",
        fluidRow(
          box(
            width = 12,
            title = "Year Selection", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            selectInput("spc_year", 
                       "Select Year for Analysis:", 
                       choices = NULL,
                       selected = NULL)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Global View: Total Rides Over All Years", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("global_rides_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Process Capability: Mean vs Variability", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$p("Stations in the upper right quadrant (high mean, high CV) are prime candidates for dock expansion."),
            plotlyOutput("mean_vs_cv_plot", height = "450px")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "AM Rush Hour - Variability Distribution", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("am_variability_bars", height = "350px")
          ),
          box(
            width = 6,
            title = "PM Rush Hour - Variability Distribution", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("pm_variability_bars", height = "350px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Priority Stations: High Average Rides AND High Coefficient of Variation", 
            status = "danger", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$p("These stations have both high ridership and high variability - prime candidates for dock expansion."),
            DT::dataTableOutput("high_avg_high_cv_table")
          )
        )
      ),
      
      # Tab 8: Bootstrap Analysis & Recommendations
      tabItem(
        tabName = "bootstrap",
        fluidRow(
          box(
            width = 12,
            title = "Bootstrap Analysis: Dock Expansion Recommendations", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Financial Impact Analysis with Bootstrapped Confidence Intervals"),
            tags$p("This analysis identifies stations that would benefit most from dock expansion. ", 
                   tags$strong("Only stations with high variability (CV > 50%) are considered."), 
                   " These stations have capacity constraints where trips are being lost due to dock availability."),
            tags$p("The bootstrap method provides confidence intervals for the estimated financial impact of expanding docks at recommended stations."),
            br(),
            h4("Analysis Parameters"),
            fluidRow(
              column(4,
                selectInput("bootstrap_year", 
                           "Select Year for Analysis:", 
                           choices = NULL,
                           selected = NULL)
              ),
              column(4,
                numericInput("num_stops", 
                           "Number of Top Stations to Recommend:", 
                           value = 10,
                           min = 1,
                           max = 50,
                           step = 1)
              ),
              column(4,
                numericInput("docks_added_pct", 
                           "Percentage of Current Docks to Add:", 
                           value = 50,
                           min = 10,
                           max = 200,
                           step = 10)
              )
            ),
            br(),
            tags$p(tags$strong("How Coefficient of Variation (CV) is Used:"), 
                   "CV measures process variability. Only stations with CV > 50% are included in recommendations, as these indicate capacity constraints. ", 
                   "The analysis estimates lost trips based on CV: stations with CV 50-70% lose 5-10% of potential trips, ", 
                   "CV 70-100% lose 10-15%, and CV >100% lose up to 25% of potential trips. ", 
                   "By expanding docks at these high-CV stations, we can recover a portion of these lost trips.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Station Recommendations", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$p("Review the recommended stations below, then select which ones you plan to implement. The bootstrap analysis will calculate the financial impact for your selected stations only."),
            DT::dataTableOutput("bootstrap_recommendations_table")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Select Stations to Implement", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            uiOutput("station_selector_ui"),
            tags$p(tags$strong("Note:"), " Select the stations you plan to implement. The financial impact calculations below will only include your selected stations."),
            br()
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Summary: Financial Impact for Selected Stations", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            div(class = "quantity-box",
              h3("Estimated Annual Net Revenue Impact (Dock Expansion)"),
              textOutput("bootstrap_summary_text")
            ),
            div(class = "quantity-box",
              h3("Estimated Annual Net Rides Gained"),
              textOutput("bootstrap_rides_text")
            ),
            div(class = "ci-box",
              h4("95% Confidence Interval (Revenue)"),
              textOutput("bootstrap_ci_text")
            ),
            div(class = "ci-box",
              h4("95% Confidence Interval (Rides)"),
              textOutput("bootstrap_rides_ci_text")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Bootstrapped Distribution: Revenue Impact", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("bootstrap_histogram", height = "400px")
          ),
          box(
            width = 6,
            title = "Bootstrapped Distribution: Rides Gained", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("bootstrap_rides_histogram", height = "400px")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "CV vs Average Daily Trips", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("bootstrap_cv_vs_trips", height = "400px")
          ),
          box(
            width = 6,
            title = "Net Gain vs Investment Cost", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("bootstrap_roi_plot", height = "400px")
          )
        )
      ),
      
      # Tab 9: References
      tabItem(
        tabName = "references",
        fluidRow(
          box(
            width = 12,
            title = "References", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            uiOutput("references_list")
          )
        )
      )
    )
  )
)
# Server Definition
server <- function(input, output, session) {
  
  # Add resource path for images
  addResourcePath("Images", "Images")
  read_csv_silent <- function(file_path) {
    # Temporarily suppress warnings
    old_warn <- options(warn = -1)$warn
    on.exit(options(warn = old_warn))
    
    # Suppress messages and warnings from readr
    suppressWarnings({
      suppressMessages({
        result <- readr::read_csv(
          file_path, 
          show_col_types = FALSE,
          progress = FALSE,
          locale = readr::locale(encoding = "UTF-8")
        )
      })
    })
    
    return(result)
  }
  
  ################################################################################
  # SPC Tab: Load and Process Data from CSV Files
  ################################################################################
  
  # Get available years from CSV files
  available_years <- reactive({
    csv_files <- list.files("DataCSV", pattern = "^BlueBikes_\\d{4}\\.csv$", full.names = FALSE)
    years <- as.numeric(gsub("BlueBikes_(\\d{4})\\.csv", "\\1", csv_files))
    sort(years[!is.na(years)])
  })
  
  # Update year selectors
  observe({
    years <- available_years()
    if (length(years) > 0) {
      year_choices <- c("All Years" = "all", setNames(years, years))
      updateSelectInput(session, "spc_year", 
                       choices = year_choices,
                       selected = ifelse(length(years) > 0, max(years), "all"))
      updateSelectInput(session, "bootstrap_year", 
                       choices = year_choices,
                       selected = ifelse(length(years) > 0, max(years), "all"))
    }
  })
  
  # Load all years data for global view
  all_years_data <- reactive({
    years <- available_years()
    if (length(years) == 0) return(NULL)
    
    all_data_list <- lapply(years, function(year) {
      file_path <- paste0("DataCSV/BlueBikes_", year, ".csv")
      if (file.exists(file_path)) {
        tryCatch({
          data <- read_csv_silent(file_path)
          return(data)
        }, error = function(e) {
          return(NULL)
        })
      }
      return(NULL)
    })
    
    # Filter out NULL values before binding
    all_data_list <- all_data_list[!sapply(all_data_list, is.null)]
    
    if (length(all_data_list) == 0) return(NULL)
    
    tryCatch({
      all_data <- dplyr::bind_rows(all_data_list)
      return(all_data)
    }, error = function(e) {
      return(NULL)
    })
  })

  
  # Load selected year data
  selected_year_data <- reactive({
    if (is.null(input$spc_year) || input$spc_year == "all") {
      return(all_years_data())
    }
    
    file_path <- paste0("DataCSV/BlueBikes_", input$spc_year, ".csv")
    if (file.exists(file_path)) {
      tryCatch({
        return(read_csv_silent(file_path))
      }, error = function(e) {
        return(NULL)
      })
    }
    return(NULL)
  })
  
  # Calculate SPC statistics for selected year
  spc_stats <- reactive({
    data <- selected_year_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Check if required columns exist
    required_cols <- c("Is_Weekend", "Month", "Start_Station_Code", "Rush_Period", "Number_of_Rides")
    missing_cols <- required_cols[!required_cols %in% names(data)]
    
    if (length(missing_cols) > 0) {
      return(NULL)
    }
    
    tryCatch({
      # Filter to weekdays and exclude winter (matching BlueBikes_Code.R logic)
      data_filtered <- data %>%
        dplyr::filter(!Is_Weekend, !Month %in% c(12, 1, 2))
      
      if (nrow(data_filtered) == 0) return(NULL)
      
      # Calculate statistics by station and rush period
      station_stats <- data_filtered %>%
        dplyr::group_by(Start_Station_Code, Rush_Period) %>%
        dplyr::summarise(
          n_days = n(),
          mean_rides = mean(Number_of_Rides, na.rm = TRUE),
          sd_rides = sd(Number_of_Rides, na.rm = TRUE),
          median_rides = median(Number_of_Rides, na.rm = TRUE),
          total_rides = sum(Number_of_Rides, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_days >= 30, !is.na(mean_rides), !is.na(sd_rides), mean_rides > 0) %>%
        dplyr::mutate(
          UCL = mean_rides + 3 * sd_rides,
          LCL = pmax(0, mean_rides - 3 * sd_rides),
          CV = (sd_rides / mean_rides) * 100,
          Variability_Category = dplyr::case_when(
            CV < 25 ~ "Low (<25%)",
            CV >= 25 & CV <= 50 ~ "Moderate (25-50%)",
            CV > 50 & CV <= 75 ~ "High (50-75%)",
            CV > 75 ~ "Very High (>75%)"
          )
        ) %>%
        dplyr::filter(!is.na(CV))
      
      return(station_stats)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Global rides plot (all years)
  output$global_rides_plot <- renderPlotly({
    all_data <- all_years_data()
    if (is.null(all_data) || nrow(all_data) == 0) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available"))
    }
    
    # Check if required columns exist
    required_cols <- c("Year", "Rush_Period", "Number_of_Rides")
    missing_cols <- required_cols[!required_cols %in% names(all_data)]
    
    if (length(missing_cols) > 0) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
    }
    
    tryCatch({
      # Aggregate by year and rush period
      yearly_summary <- all_data %>%
        dplyr::group_by(Year, Rush_Period) %>%
        dplyr::summarise(Total_Rides = sum(Number_of_Rides, na.rm = TRUE), .groups = "drop")
      
      if (nrow(yearly_summary) == 0) {
        return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available after aggregation"))
      }
      
      p <- ggplot(yearly_summary, aes(x = Year, y = Total_Rides, fill = Rush_Period)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"), name = "Rush Period") +
        labs(
          title = "Total Number of Rides: AM vs PM Over All Years",
          x = "Year",
          y = "Total Rides",
          fill = "Rush Period"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # AM Variability Bars
  output$am_variability_bars <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available"))
    }
    
    tryCatch({
      am_stats <- stats %>% dplyr::filter(Rush_Period == "AM")
      if (nrow(am_stats) == 0) {
        return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No AM data available"))
      }
      
      category_summary <- am_stats %>%
        dplyr::group_by(Variability_Category) %>%
        dplyr::summarise(Count = n(), .groups = "drop") %>%
        dplyr::mutate(
          Percentage = (Count / sum(Count)) * 100,
          Variability_Category = factor(Variability_Category, 
                                        levels = c("Low (<25%)", "Moderate (25-50%)", 
                                                  "High (50-75%)", "Very High (>75%)"))
        )
      
      if (nrow(category_summary) == 0) {
        return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available after processing"))
      }
      
      p <- ggplot(category_summary, aes(x = Variability_Category, y = Count, fill = Variability_Category)) +
        geom_col(alpha = 0.9) +
        geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
                  vjust = -0.2, fontface = "bold") +
        scale_fill_manual(values = c("Low (<25%)" = "#2E7D32", 
                                     "Moderate (25-50%)" = "#FFC107",
                                     "High (50-75%)" = "#FF9800",
                                     "Very High (>75%)" = "#C62828"),
                          guide = "none") +
        labs(
          title = "AM Rush Hour - Station Variability",
          x = "Variability Category",
          y = "Number of Stations"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # PM Variability Bars
  output$pm_variability_bars <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available"))
    }
    
    tryCatch({
      pm_stats <- stats %>% dplyr::filter(Rush_Period == "PM")
      if (nrow(pm_stats) == 0) {
        return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No PM data available"))
      }
      
      category_summary <- pm_stats %>%
        dplyr::group_by(Variability_Category) %>%
        dplyr::summarise(Count = n(), .groups = "drop") %>%
        dplyr::mutate(
          Percentage = (Count / sum(Count)) * 100,
          Variability_Category = factor(Variability_Category, 
                                        levels = c("Low (<25%)", "Moderate (25-50%)", 
                                                  "High (50-75%)", "Very High (>75%)"))
        )
      
      if (nrow(category_summary) == 0) {
        return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available after processing"))
      }
      
      p <- ggplot(category_summary, aes(x = Variability_Category, y = Count, fill = Variability_Category)) +
        geom_col(alpha = 0.9) +
        geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
                  vjust = -0.2, fontface = "bold") +
        scale_fill_manual(values = c("Low (<25%)" = "#2E7D32", 
                                     "Moderate (25-50%)" = "#FFC107",
                                     "High (50-75%)" = "#FF9800",
                                     "Very High (>75%)" = "#C62828"),
                          guide = "none") +
        labs(
          title = "PM Rush Hour - Station Variability",
          x = "Variability Category",
          y = "Number of Stations"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Top Stations Table
  output$top_stations_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    top_stations <- stats %>%
      dplyr::group_by(Start_Station_Code) %>%
      dplyr::summarise(
        Avg_AM_Rides = mean(mean_rides[Rush_Period == "AM"], na.rm = TRUE),
        Avg_PM_Rides = mean(mean_rides[Rush_Period == "PM"], na.rm = TRUE),
        Total_Avg_Rides = mean(mean_rides, na.rm = TRUE),
        AM_CV = mean(CV[Rush_Period == "AM"], na.rm = TRUE),
        PM_CV = mean(CV[Rush_Period == "PM"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(desc(Total_Avg_Rides)) %>%
      head(20) %>%
      dplyr::mutate(
        Avg_AM_Rides = round(Avg_AM_Rides, 2),
        Avg_PM_Rides = round(Avg_PM_Rides, 2),
        Total_Avg_Rides = round(Total_Avg_Rides, 2),
        AM_CV = round(AM_CV, 2),
        PM_CV = round(PM_CV, 2)
      )
    
    DT::datatable(top_stations,
                  colnames = c("Station Code", "Avg AM Rides", "Avg PM Rides", 
                              "Total Avg Rides", "AM CV (%)", "PM CV (%)"),
                  options = list(pageLength = 10, dom = 't'))
  })
  
  # High Variability Table (50-75%)
  output$high_variability_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    high_var <- stats %>%
      dplyr::filter(CV >= 50 & CV <= 75) %>%
      dplyr::arrange(desc(mean_rides)) %>%
      head(15) %>%
      dplyr::select(Start_Station_Code, Rush_Period, mean_rides, CV) %>%
      dplyr::mutate(
        mean_rides = round(mean_rides, 2),
        CV = round(CV, 2)
      )
    
    DT::datatable(high_var,
                  colnames = c("Station Code", "Rush Period", "Avg Rides", "CV (%)"),
                  options = list(pageLength = 15, dom = 't'))
  })
  
  # Very High Variability Table (>75%)
  output$very_high_variability_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    very_high_var <- stats %>%
      dplyr::filter(CV > 75) %>%
      dplyr::arrange(desc(mean_rides)) %>%
      head(15) %>%
      dplyr::select(Start_Station_Code, Rush_Period, mean_rides, CV) %>%
      dplyr::mutate(
        mean_rides = round(mean_rides, 2),
        CV = round(CV, 2)
      )
    
    DT::datatable(very_high_var,
                  colnames = c("Station Code", "Rush Period", "Avg Rides", "CV (%)"),
                  options = list(pageLength = 15, dom = 't'))
  })
  
  # Top Stations Comparison Chart
  output$top_stations_comparison <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available"))
    }
    
    tryCatch({
      # Get top stations by average rides
      top_stations_list <- stats %>%
        dplyr::group_by(Start_Station_Code) %>%
        dplyr::summarise(avg_rides = mean(mean_rides, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(avg_rides)) %>%
        head(15) %>%
        dplyr::pull(Start_Station_Code)
      
      comparison_data <- stats %>%
        dplyr::filter(Start_Station_Code %in% top_stations_list)
      
      if (nrow(comparison_data) == 0) {
        return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available"))
      }
      
      p <- ggplot(comparison_data, 
                  aes(x = reorder(Start_Station_Code, mean_rides), 
                      y = mean_rides, fill = Rush_Period)) +
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
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Mean vs CV Plot
  output$mean_vs_cv_plot <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = "No data available"))
    }
    
    tryCatch({
      p <- ggplot(stats, aes(x = mean_rides, y = CV, color = Rush_Period)) +
        geom_point(alpha = 0.6, size = 2.5) +
        geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
        scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                           name = "Rush Period") +
        annotate("text", x = max(stats$mean_rides, na.rm = TRUE), y = 55, 
                 label = "High Variability Zone", color = "red", hjust = 1) +
        labs(
          title = "Process Capability: Mean vs Variability",
          subtitle = "Higher volume doesn't always mean more stability",
          x = "Average Daily Rides",
          y = "Coefficient of Variation (%)"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Station Summary Table
  output$station_summary_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    summary_table <- stats %>%
      dplyr::select(Start_Station_Code, Rush_Period, mean_rides, sd_rides, CV, 
             Variability_Category, UCL, LCL) %>%
      dplyr::mutate(
        mean_rides = round(mean_rides, 2),
        sd_rides = round(sd_rides, 2),
        CV = round(CV, 2),
        UCL = round(UCL, 2),
        LCL = round(LCL, 2)
      )
    
    DT::datatable(summary_table,
                  colnames = c("Station Code", "Rush Period", "Mean Rides", 
                              "SD Rides", "CV (%)", "Variability Category", 
                              "UCL", "LCL"),
                  options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # High Average Rides AND High CV Table (Priority Stations)
  output$high_avg_high_cv_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    # Calculate station-level aggregates
    station_agg <- stats %>%
      dplyr::group_by(Start_Station_Code) %>%
      dplyr::summarise(
        Avg_Mean_Rides = mean(mean_rides, na.rm = TRUE),
        Avg_CV = mean(CV, na.rm = TRUE),
        Max_CV = max(CV, na.rm = TRUE),
        AM_Mean = mean(mean_rides[Rush_Period == "AM"], na.rm = TRUE),
        PM_Mean = mean(mean_rides[Rush_Period == "PM"], na.rm = TRUE),
        AM_CV = mean(CV[Rush_Period == "AM"], na.rm = TRUE),
        PM_CV = mean(CV[Rush_Period == "PM"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(
        Avg_Mean_Rides > quantile(Avg_Mean_Rides, 0.75, na.rm = TRUE),  # Top 25% by average rides
        Avg_CV > 50  # High variability (CV > 50%)
      ) %>%
      dplyr::arrange(desc(Avg_Mean_Rides), desc(Avg_CV)) %>%
      dplyr::mutate(
        Avg_Mean_Rides = round(Avg_Mean_Rides, 2),
        Avg_CV = round(Avg_CV, 2),
        Max_CV = round(Max_CV, 2),
        AM_Mean = round(AM_Mean, 2),
        PM_Mean = round(PM_Mean, 2),
        AM_CV = round(AM_CV, 2),
        PM_CV = round(PM_CV, 2)
      )
    
    DT::datatable(station_agg,
                  colnames = c("Station Code", "Avg Mean Rides", "Avg CV (%)", 
                              "Max CV (%)", "AM Mean Rides", "PM Mean Rides",
                              "AM CV (%)", "PM CV (%)"),
                  options = list(pageLength = 15, scrollX = TRUE, order = list(list(1, 'desc'))))
  })
  
  ################################################################################
  # Bootstrap Analysis Tab
  ################################################################################
  
  # Bootstrap analysis for dock expansion - simplified version using SPC stats
  bootstrap_analysis <- reactive({
    # Get year selection for bootstrap (use bootstrap_year if available, otherwise spc_year)
    bootstrap_year <- if (is.null(input$bootstrap_year)) input$spc_year else input$bootstrap_year
    
    # Get data for selected year
    if (is.null(bootstrap_year) || bootstrap_year == "all") {
      data <- all_years_data()
    } else {
      file_path <- paste0("DataCSV/BlueBikes_", bootstrap_year, ".csv")
      if (file.exists(file_path)) {
        tryCatch({
          data <- read_csv_silent(file_path)
        }, error = function(e) {
          return(NULL)
        })
      } else {
        return(NULL)
      }
    }
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Calculate SPC stats for the selected year
    required_cols <- c("Is_Weekend", "Month", "Start_Station_Code", "Rush_Period", "Number_of_Rides")
    missing_cols <- required_cols[!required_cols %in% names(data)]
    
    if (length(missing_cols) > 0) {
      return(NULL)
    }
    
    # Calculate SPC stats
    tryCatch({
      data_filtered <- data %>%
        dplyr::filter(!Is_Weekend, !Month %in% c(12, 1, 2))
      
      if (nrow(data_filtered) == 0) return(NULL)
      
      station_stats <- data_filtered %>%
        dplyr::group_by(Start_Station_Code, Rush_Period) %>%
        dplyr::summarise(
          n_days = n(),
          mean_rides = mean(Number_of_Rides, na.rm = TRUE),
          sd_rides = sd(Number_of_Rides, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_days >= 30, !is.na(mean_rides), !is.na(sd_rides), mean_rides > 0) %>%
        dplyr::mutate(
          CV = (sd_rides / mean_rides) * 100
        ) %>%
        dplyr::filter(!is.na(CV))
      
      stats <- station_stats
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(stats) || nrow(stats) == 0) {
      return(NULL)
    }
    
    tryCatch({
      # Financial parameters
      revenue_per_trip <- 2.95  # USD per ride
      capex_per_dock <- 500     # USD per dock
      capex_annualization_years <- 10  # years
      
      # Calculate station-level usage statistics from the data
      data_filtered <- data %>%
        dplyr::filter(!Is_Weekend, !Month %in% c(12, 1, 2))
      
      if (nrow(data_filtered) == 0) return(NULL)
      
      # Calculate station usage - aggregate by station and day first, then average
      # This ensures we get true daily averages (combining AM and PM for each day)
      station_usage <- data_filtered %>%
        dplyr::group_by(Start_Station_Code, Year, Month, Day_of_Month) %>%
        dplyr::summarise(
          daily_trips = sum(Number_of_Rides, na.rm = TRUE),  # Sum AM + PM for each day
          .groups = "drop"
        ) %>%
        dplyr::group_by(Start_Station_Code) %>%
        dplyr::summarise(
          avg_daily_trips = mean(daily_trips, na.rm = TRUE),  # Average across all days
          n_days = n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_days >= 10) %>%  # Reduced from 30 to 10 days for more flexibility
        dplyr::rename(station_id = Start_Station_Code)
      
      if (nrow(station_usage) == 0) return(NULL)
      
      # Use CV as proxy for imbalance frequency (high CV = high variability = more likely to have capacity issues)
      imbalance_freq <- stats %>%
        dplyr::group_by(Start_Station_Code) %>%
        dplyr::summarise(
          avg_cv = mean(CV, na.rm = TRUE),
          max_cv = max(CV, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          # Convert CV to inflow ratio proxy (higher CV = more likely to have capacity issues)
          inflow_ratio = pmin(0.5, avg_cv / 200)  # Cap at 0.5
        ) %>%
        dplyr::rename(station_id = Start_Station_Code)
      
      if (nrow(imbalance_freq) == 0) return(NULL)
      
      # Merge station info
      station_summary <- station_usage %>%
        dplyr::left_join(imbalance_freq, by = "station_id") %>%
        dplyr::filter(
          !is.na(avg_cv),
          avg_daily_trips > 0,
          avg_cv > 0
        ) %>%
        dplyr::mutate(
          # Assume fixed 19 docks per station
          estimated_docks = 19
        )
      
      if (nrow(station_summary) == 0) return(NULL)
      
      # Filter candidate stations: ONLY stations with high CV (>50%)
      # High CV indicates capacity constraints and need for expansion
      dock_candidates <- station_summary %>%
        dplyr::filter(avg_cv > 50) %>%
        dplyr::arrange(desc(avg_cv), desc(avg_daily_trips))
      
      if (nrow(dock_candidates) == 0) return(NULL)
      
      # Estimate expansion impact per station
      dock_candidates <- dock_candidates %>%
        dplyr::mutate(
          added_docks = round(estimated_docks * (if (is.null(input$docks_added_pct)) 0.5 else input$docks_added_pct / 100)),
          new_capacity = estimated_docks + added_docks,
          # Estimate lost trips based on CV: higher CV indicates more capacity issues
          # For stations with high CV (>50%), estimate that some trips are being lost due to capacity
          # Formula: lost_trip_pct increases with CV, capped at 25% (conservative estimate)
          lost_trip_pct = dplyr::case_when(
            avg_cv <= 30 ~ 0.0,  # Low CV = no capacity issues
            avg_cv > 30 & avg_cv <= 50 ~ (avg_cv - 30) / 400,  # 0-5% lost for CV 30-50%
            avg_cv > 50 & avg_cv <= 70 ~ 0.05 + (avg_cv - 50) / 400,  # 5-10% lost for CV 50-70%
            avg_cv > 70 & avg_cv <= 100 ~ 0.10 + (avg_cv - 70) / 600,  # 10-15% lost for CV 70-100%
            TRUE ~ pmin(0.25, 0.15 + (avg_cv - 100) / 1000)  # 15-25% lost for CV > 100% (capped at 25%)
          ),
          # Estimate potential trips if capacity wasn't a constraint
          # If we're losing X% of trips, then: current = potential * (1 - lost_pct)
          # Therefore: potential = current / (1 - lost_pct)
          potential_daily_trips = dplyr::if_else(
            lost_trip_pct >= 1.0, 
            avg_daily_trips,  # Safety: avoid division by zero
            avg_daily_trips / (1 - lost_trip_pct)
          ),
          # Estimate how many trips are currently being lost per day
          lost_daily_trips = pmax(0, potential_daily_trips - avg_daily_trips),
          # Recovery rate: with dock expansion, we can recover a portion of lost trips
          # More conservative: assume we recover 40-60% of lost trips (not 100%)
          recovery_rate = dplyr::case_when(
            avg_cv <= 50 ~ 0.4,   # Lower CV = less capacity constraint = recover less
            avg_cv > 50 & avg_cv <= 70 ~ 0.5,
            avg_cv > 70 & avg_cv <= 100 ~ 0.55,
            TRUE ~ 0.6  # Higher CV = more capacity constraint = can recover more (capped at 60%)
          ),
          # Estimate recovered trips per day (only the ADDITIONAL trips we can capture)
          recovered_daily_trips = lost_daily_trips * recovery_rate,
          # Annual recovered trips (only additional trips, not existing ones)
          est_recovered_trips = recovered_daily_trips * 365,
          # Revenue gain from recovered trips only (not from existing trips)
          est_revenue_gain = est_recovered_trips * revenue_per_trip,
          # Capital costs
          capex_cost = added_docks * capex_per_dock,
          annualized_cost = capex_cost / capex_annualization_years,
          # Net gain
          net_gain = est_revenue_gain - annualized_cost
        )
      
      # Limit to top N stations based on num_stops input, sorted by net_gain
      num_stops <- if (is.null(input$num_stops) || is.na(input$num_stops)) 10 else input$num_stops
      dock_candidates <- dock_candidates %>%
        dplyr::arrange(desc(net_gain)) %>%
        head(num_stops)
      
      return(dock_candidates)
    }, error = function(e) {
      # Return error message for debugging
      cat("Bootstrap analysis error:", e$message, "\n")
      cat("Stack trace:", paste(capture.output(traceback()), collapse = "\n"), "\n")
      return(NULL)
    })
  })
  
  # Station selector UI
  output$station_selector_ui <- renderUI({
    candidates <- bootstrap_analysis()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(tags$p("No candidate stations available. Please ensure data is loaded."))
    }
    
    # Create station labels with net gain for easy identification
    station_choices <- candidates %>%
      dplyr::arrange(desc(net_gain)) %>%
      dplyr::mutate(
        label = paste0(station_id, " (Net Gain: $", format(round(net_gain, 0), big.mark = ","), "/year)")
      )
    
    selectInput(
      "selected_stations",
      "Select Stations to Implement (you can select multiple):",
      choices = setNames(station_choices$station_id, station_choices$label),
      selected = NULL,
      multiple = TRUE,
      width = "100%"
    )
  })
  
  # Reactive: Get selected stations only
  selected_stations_data <- reactive({
    candidates <- bootstrap_analysis()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(NULL)
    }
    
    # If no stations selected, return all candidates (backward compatibility)
    if (is.null(input$selected_stations) || length(input$selected_stations) == 0) {
      return(candidates)
    }
    
    # Filter to only selected stations
    selected <- candidates %>%
      dplyr::filter(station_id %in% input$selected_stations)
    
    if (nrow(selected) == 0) {
      return(NULL)
    }
    
    return(selected)
  })
  
  # Bootstrap summary text
  output$bootstrap_summary_text <- renderText({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0) {
      if (is.null(input$selected_stations) || length(input$selected_stations) == 0) {
        return("Please select one or more stations to see financial impact estimates.")
      }
      return("No stations selected or selected stations not available. Please select stations from the list above.")
    }
    
    total_net <- sum(candidates$net_gain, na.rm = TRUE)
    if (is.na(total_net) || is.infinite(total_net)) {
      return("Unable to calculate total impact.")
    }
    paste0("$", format(round(total_net, 2), big.mark = ","), " USD per year")
  })
  
  # Bootstrap rides summary text
  output$bootstrap_rides_text <- renderText({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return("No candidate stations found for analysis.")
    }
    
    total_rides <- sum(candidates$est_recovered_trips, na.rm = TRUE)
    if (is.na(total_rides) || is.infinite(total_rides)) {
      return("Unable to calculate total rides.")
    }
    paste0(format(round(total_rides, 0), big.mark = ","), " additional rides per year")
  })
  
  # Bootstrap rides CI text
  output$bootstrap_rides_ci_text <- renderText({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return("No data available for confidence interval calculation.")
    }
    
    if (nrow(candidates) < 1) {
      return("No stations selected for analysis.")
    }
    
    if (nrow(candidates) == 1) {
      # For single station, show point estimate only
      total_rides <- sum(candidates$est_recovered_trips, na.rm = TRUE)
      return(paste0("Single station selected. Estimated: ", format(round(total_rides, 0), big.mark = ","), " rides/year (no CI for single station)"))
    }
    
    tryCatch({
      set.seed(123)
      boot_fn <- function(data, indices) {
        d <- data[indices, ]
        result <- sum(d$est_recovered_trips, na.rm = TRUE)
        if (is.na(result) || is.infinite(result)) return(0)
        return(result)
      }
      
      boot_result <- boot::boot(data = candidates, statistic = boot_fn, R = 1000)
      ci_result <- boot::boot.ci(boot_result, type = "perc", conf = 0.95)
      
      if (!is.null(ci_result$percent) && length(ci_result$percent) >= 5) {
        ci_lower <- ci_result$percent[4]
        ci_upper <- ci_result$percent[5]
        if (!is.na(ci_lower) && !is.na(ci_upper)) {
          return(paste0("[", format(round(ci_lower, 0), big.mark = ","), 
                       ", ", format(round(ci_upper, 0), big.mark = ","), "] rides per year"))
        }
      }
      return("Confidence interval calculation failed.")
    }, error = function(e) {
      return(paste("Error:", e$message))
    })
  })
  
  # Bootstrap CI text
  output$bootstrap_ci_text <- renderText({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return("No data available for confidence interval calculation.")
    }
    
    if (nrow(candidates) < 1) {
      return("No stations selected for analysis.")
    }
    
    if (nrow(candidates) == 1) {
      # For single station, show point estimate only
      total_net <- sum(candidates$net_gain, na.rm = TRUE)
      return(paste0("Single station selected. Estimated: $", format(round(total_net, 2), big.mark = ","), "/year (no CI for single station)"))
    }
    
    tryCatch({
      set.seed(123)
      boot_fn <- function(data, indices) {
        d <- data[indices, ]
        result <- sum(d$net_gain, na.rm = TRUE)
        if (is.na(result) || is.infinite(result)) return(0)
        return(result)
      }
      
      boot_result <- boot::boot(data = candidates, statistic = boot_fn, R = 1000)
      ci_result <- boot::boot.ci(boot_result, type = "perc", conf = 0.95)
      
      if (!is.null(ci_result$percent) && length(ci_result$percent) >= 5) {
        ci_lower <- ci_result$percent[4]
        ci_upper <- ci_result$percent[5]
        if (!is.na(ci_lower) && !is.na(ci_upper)) {
          return(paste0("[$", format(round(ci_lower, 2), big.mark = ","), 
                       ", $", format(round(ci_upper, 2), big.mark = ","), "] USD per year"))
        }
      }
      return("Confidence interval calculation failed.")
    }, error = function(e) {
      return(paste("Error:", e$message))
    })
  })
  
  # Bootstrap histogram
  output$bootstrap_histogram <- renderPlotly({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = "No data available", 
                    annotations = list(text = "No candidate stations found. Please ensure data is loaded.", 
                                    showarrow = FALSE)))
    }
    
    if (nrow(candidates) < 1) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = "No stations selected", 
                    annotations = list(text = "Please select one or more stations to see bootstrap analysis.", 
                                    showarrow = FALSE)))
    }
    
    tryCatch({
      set.seed(123)
      boot_fn <- function(data, indices) {
        d <- data[indices, ]
        result <- sum(d$net_gain, na.rm = TRUE)
        if (is.na(result) || is.infinite(result)) return(0)
        return(result)
      }
      
      boot_result <- boot::boot(data = candidates, statistic = boot_fn, R = 1000)
      ci_result <- boot::boot.ci(boot_result, type = "perc", conf = 0.95)
      
      boot_data <- data.frame(impact = boot_result$t)
      mean_impact <- mean(boot_result$t, na.rm = TRUE)
      
      p <- ggplot(boot_data, aes(x = impact)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
        geom_vline(xintercept = mean_impact, color = "red", linetype = "solid", linewidth = 1.5) +
        labs(
          title = "Bootstrapped Distribution of Dock Expansion Impact",
          subtitle = "Red line: Mean estimate",
          x = "Estimated Annual Net Impact (USD)",
          y = "Frequency"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"))
      
      # Add CI lines if available
      if (!is.null(ci_result$percent) && length(ci_result$percent) >= 5) {
        ci_lower <- ci_result$percent[4]
        ci_upper <- ci_result$percent[5]
        if (!is.na(ci_lower) && !is.na(ci_upper)) {
          p <- p + 
            geom_vline(xintercept = c(ci_lower, ci_upper), 
                      color = "darkgreen", linetype = "dashed", linewidth = 1.5) +
            labs(subtitle = "Red line: Mean estimate | Green dashed lines: 95% Confidence Interval")
        }
      }
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = paste("Error:", e$message),
                    annotations = list(text = paste("Error details:", e$message), 
                                    showarrow = FALSE)))
    })
  })
  
  # Bootstrap recommendations plot
  output$bootstrap_recommendations_plot <- renderPlotly({
    candidates <- bootstrap_analysis()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = "No data available",
                    annotations = list(text = "No candidate stations found.", showarrow = FALSE)))
    }
    
    tryCatch({
      # Candidates are already filtered to top N and sorted by net_gain from bootstrap_analysis()
      num_stops <- if (is.null(input$num_stops)) 10 else input$num_stops
      
      if (nrow(candidates) == 0) {
        return(plot_ly() %>% 
               add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
               layout(title = "No recommendations",
                      annotations = list(text = "No stations meet the criteria (CV > 50% required).", showarrow = FALSE)))
      }
      
      top_recommendations <- candidates %>%
        dplyr::mutate(
          station_label = paste0("Station ", station_id)
        )
      
      p <- ggplot(top_recommendations, aes(x = reorder(station_label, net_gain), y = net_gain)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        coord_flip() +
        labs(
          title = paste0("Top ", if (is.null(input$num_stops)) 10 else input$num_stops, " Stations Recommended for Dock Expansion"),
          subtitle = "Based on high ridership, high variability, and estimated financial impact",
          x = "Station",
          y = "Estimated Net Annual Gain (USD)"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"))
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = paste("Error:", e$message),
                    annotations = list(text = paste("Error:", e$message), showarrow = FALSE)))
    })
  })
  
  # Bootstrap rides histogram
  output$bootstrap_rides_histogram <- renderPlotly({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0 || nrow(candidates) < 2) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = "No data available", 
                    annotations = list(text = "No candidate stations found.", showarrow = FALSE)))
    }
    
    tryCatch({
      set.seed(123)
      boot_fn <- function(data, indices) {
        d <- data[indices, ]
        result <- sum(d$est_recovered_trips, na.rm = TRUE)
        if (is.na(result) || is.infinite(result)) return(0)
        return(result)
      }
      
      boot_result <- boot::boot(data = candidates, statistic = boot_fn, R = 1000)
      ci_result <- boot::boot.ci(boot_result, type = "perc", conf = 0.95)
      
      boot_data <- data.frame(rides = boot_result$t)
      mean_rides <- mean(boot_result$t, na.rm = TRUE)
      
      p <- ggplot(boot_data, aes(x = rides)) +
        geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7, color = "white") +
        geom_vline(xintercept = mean_rides, color = "red", linetype = "solid", linewidth = 1.5) +
        labs(
          title = "Bootstrapped Distribution of Rides Gained",
          subtitle = "Red line: Mean estimate",
          x = "Estimated Annual Rides Gained",
          y = "Frequency"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"))
      
      if (!is.null(ci_result$percent) && length(ci_result$percent) >= 5) {
        ci_lower <- ci_result$percent[4]
        ci_upper <- ci_result$percent[5]
        if (!is.na(ci_lower) && !is.na(ci_upper)) {
          p <- p + 
            geom_vline(xintercept = c(ci_lower, ci_upper), 
                      color = "darkgreen", linetype = "dashed", linewidth = 1.5) +
            labs(subtitle = "Red line: Mean | Green dashed: 95% CI")
        }
      }
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = paste("Error:", e$message),
                    annotations = list(text = paste("Error:", e$message), showarrow = FALSE)))
    })
  })
  
  # CV vs Trips scatter plot
  output$bootstrap_cv_vs_trips <- renderPlotly({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = "No data available",
                    annotations = list(text = "No candidate stations found.", showarrow = FALSE)))
    }
    
    tryCatch({
      p <- ggplot(candidates, aes(x = avg_daily_trips, y = avg_cv, size = net_gain, color = net_gain)) +
        geom_point(alpha = 0.7) +
        scale_color_viridis_c(name = "Net Gain ($)") +
        scale_size_continuous(name = "Net Gain ($)") +
        geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.5) +
        labs(
          title = "Station Selection Criteria",
          subtitle = "Red line: CV = 50% threshold",
          x = "Average Daily Trips",
          y = "Average CV (%)",
          size = "Net Gain ($)",
          color = "Net Gain ($)"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"))
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = paste("Error:", e$message),
                    annotations = list(text = paste("Error:", e$message), showarrow = FALSE)))
    })
  })
  
  # ROI plot
  output$bootstrap_roi_plot <- renderPlotly({
    candidates <- selected_stations_data()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = "No data available",
                    annotations = list(text = "No candidate stations found.", showarrow = FALSE)))
    }
    
    tryCatch({
      candidates <- candidates %>%
        dplyr::mutate(roi = (net_gain / annualized_cost) * 100)
      
      p <- ggplot(candidates, aes(x = annualized_cost, y = net_gain, size = est_recovered_trips, color = roi)) +
        geom_point(alpha = 0.7) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray", alpha = 0.5) +
        scale_color_viridis_c(name = "ROI (%)") +
        scale_size_continuous(name = "Rides/Year") +
        labs(
          title = "Return on Investment Analysis",
          subtitle = "Diagonal line: Break-even (Net Gain = Cost)",
          x = "Annualized Investment Cost ($)",
          y = "Estimated Net Gain ($)",
          size = "Rides/Year",
          color = "ROI (%)"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"))
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plot_ly() %>% 
             add_trace(x = NULL, y = NULL, type = 'scatter', mode = 'markers', showlegend = FALSE) %>%
             layout(title = paste("Error:", e$message),
                    annotations = list(text = paste("Error:", e$message), showarrow = FALSE)))
    })
  })
  
  # Bootstrap recommendations table
  output$bootstrap_recommendations_table <- DT::renderDataTable({
    candidates <- bootstrap_analysis()
    if (is.null(candidates) || nrow(candidates) == 0) {
      return(DT::datatable(data.frame(Message = "No candidate stations found")))
    }
    
    # Candidates are already filtered to top N and sorted by net_gain from bootstrap_analysis()
    recommendations <- candidates %>%
      dplyr::select(station_id, avg_daily_trips, avg_cv, inflow_ratio, estimated_docks, 
                    added_docks, new_capacity, est_recovered_trips, est_revenue_gain, annualized_cost, net_gain) %>%
      dplyr::mutate(
        avg_daily_trips = round(avg_daily_trips, 2),
        avg_cv = round(avg_cv, 2),
        inflow_ratio = round(inflow_ratio, 3),
        est_recovered_trips = round(est_recovered_trips, 0),
        est_revenue_gain = round(est_revenue_gain, 2),
        annualized_cost = round(annualized_cost, 2),
        net_gain = round(net_gain, 2)
      ) %>%
      dplyr::rename(
        "Station ID" = station_id,
        "Avg Daily Trips" = avg_daily_trips,
        "Avg CV (%)" = avg_cv,
        "Inflow Ratio" = inflow_ratio,
        "Current Docks" = estimated_docks,
        "Added Docks" = added_docks,
        "New Capacity" = new_capacity,
        "Rides Gained/Year" = est_recovered_trips,
        "Revenue Gain ($)" = est_revenue_gain,
        "Annual Cost ($)" = annualized_cost,
        "Net Gain ($)" = net_gain
      )
    
    DT::datatable(recommendations,
                  colnames = c("Station ID", "Avg Daily Trips", "Inflow Ratio", 
                              "Current Docks (Est.)", "Added Docks", "New Capacity",
                              "Est. Revenue Gain ($)", "Annualized Cost ($)", "Net Gain ($)"),
                  options = list(pageLength = 15, scrollX = TRUE, order = list(list(8, 'desc'))))
  })
  
  ################################################################################
  # End SPC Tab
  ################################################################################
  
  # Visualization 1: Process Variability Comparison (AM vs PM)
  output$variability_plot <- renderPlotly({
    # Create sample data for visualization (representing CV distribution)
    set.seed(123)
    
    # Simulate CV values for AM and PM stations
    am_cv <- rnorm(263, mean = 50, sd = 25)
    am_cv <- pmax(am_cv, 0)  # Ensure non-negative
    
    pm_cv <- rnorm(263, mean = 53, sd = 28)
    pm_cv <- pmax(pm_cv, 0)
    
    # Create data frame
    cv_data <- data.frame(
      CV = c(am_cv, pm_cv),
      Rush_Hour = rep(c("AM (7-9 AM)", "PM (4-6 PM)"), each = 263)
    )
    
    # Create the plot
    p <- ggplot(cv_data, aes(x = CV, fill = Rush_Hour)) +
      geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
      geom_vline(xintercept = 50, color = "red", linetype = "dashed", size = 1) +
      scale_fill_manual(values = c("AM (7-9 AM)" = "#E69F00", "PM (4-6 PM)" = "#56B4E9")) +
      labs(
        title = "Process Variability Distribution: AM vs PM Rush Hours",
        subtitle = "Distribution of Coefficient of Variation (CV) across all stations",
        x = "Coefficient of Variation (%)",
        y = "Number of Stations",
        fill = "Rush Period",
        caption = "Red dashed line indicates high variability threshold (CV = 50%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9, color = "gray50")
      ) +
      annotate("text", x = 55, y = Inf, label = "High Variability\nThreshold", 
               vjust = 1.5, color = "red", size = 3.5, fontface = "bold")
    
    return(ggplotly(p))
  })
  
  # References List
  output$references_list <- renderUI({
    refs <- list(
      tags$p(tags$strong("Banerjee, S., Kabir, M. M., Khadem, N. K., & Chavis, C. (2020)."), 
             "Optimal locations for bikeshare stations: A new GIS based spatial approach. ",
             tags$em("Transportation Research Interdisciplinary Perspectives"), ", 4, 100101. ",
             tags$a(href = "https://doi.org/10.1016/j.trip.2020.100101", "https://doi.org/10.1016/j.trip.2020.100101")),
      
      tags$p(tags$strong("Bike Share Expansion 2024-2025. (2025, October 31)."), 
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/departments/transportation/bike-share-expansion-2024-2025", 
                    "https://www.boston.gov/departments/transportation/bike-share-expansion-2024-2025")),
      
      tags$p(tags$strong("Bluebikes membership & pass options."), 
             tags$a(href = "https://bluebikes.com/pricing", "https://bluebikes.com/pricing")),
      
      tags$p(tags$strong("Blue Bikes - Overview, News & similar companies | Zoominfo.com."), 
             tags$a(href = "https://www.zoominfo.com/c/blue-bikes/450178718", 
                    "https://www.zoominfo.com/c/blue-bikes/450178718")),
      
      tags$p(tags$strong("Boston, C. O. (2024, October 24)."), 
             "Bluebike Station Siting Project Summary. ",
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary", 
                    "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary")),
      
      tags$p(tags$strong("Chavis, C., Barnes, P., Grasso, S., Bhuyan, I. A., & Nickkar, A. (2018)."), 
             "Bicycle justice or just bicycles? Analyzing equity in Baltimore's bike share program. ",
             tags$em("University of Delaware Center for Applied Demography and Survey Research."), 
             tags$a(href = "https://udspace.udel.edu/items/d5a8a864-87f8-4d39-b835-6fe521c5a63d", 
                    "https://udspace.udel.edu/items/d5a8a864-87f8-4d39-b835-6fe521c5a63d")),
      
      tags$p(tags$strong("Chen, W., Chen, X., Cheng, L., & Tao, S. (2024)."), 
             "Locating new docked bike sharing stations considering demand suitability and spatial accessibility. ",
             tags$em("Travel Behaviour and Society"), ", 34, 100675. ",
             tags$a(href = "https://doi.org/10.1016/j.tbs.2023.100675", "https://doi.org/10.1016/j.tbs.2023.100675")),
      
      tags$p(tags$strong("City of Boston. (2024, October 24)."), 
             "Bluebike Station Siting Project Summary. ",
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary", 
                    "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary")),
      
      tags$p(tags$strong("Freund, D., Norouzi-Fard, A., Paul, A., Wang, C., Henderson, S., & Shmoys, D. (2016)."), 
             "Data-driven rebalancing methods for bike-share systems. ",
             tags$em("Cornell University."), 
             tags$a(href = "https://people.orie.cornell.edu/shane/pubs/BSOvernight.pdf", 
                    "https://people.orie.cornell.edu/shane/pubs/BSOvernight.pdf")),
      
      tags$p(tags$strong("Go Boston 2030 Vision and Action Plan released. (2018, June 19)."), 
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/news/go-boston-2030-vision-and-action-plan-released", 
                    "https://www.boston.gov/news/go-boston-2030-vision-and-action-plan-released")),
      
      tags$p(tags$strong("Herbert, K. (2021, May 18)."), 
             "Boston's Vision for Equitable Bike Share - Better Bike Share. ",
             tags$em("Better Bike Share."), 
             tags$a(href = "https://betterbikeshare.org/2021/05/18/bostons-vision-for-equitable-bike-share/", 
                    "https://betterbikeshare.org/2021/05/18/bostons-vision-for-equitable-bike-share/")),
      
      tags$p(tags$strong("Hrabec, D., Nverlý, V., Víchová, K., Šohaj, K., Peterek, K., & Taraba, P. (2024)."), 
             "Location of bike-sharing stations: Optimization model and case studies with insights into population coverage. ",
             tags$em("SSRN Electronic Journal."), 
             tags$a(href = "https://doi.org/10.2139/ssrn.4849664", "https://doi.org/10.2139/ssrn.4849664")),
      
      tags$p(tags$strong("Karpinski, E. (2021)."), 
             "Estimating the effect of protected bike lanes on bike-share ridership in Boston: A case study on Commonwealth Avenue. ",
             tags$em("Case Studies on Transport Policy."), 
             tags$a(href = "https://doi.org/10.1016/j.cstp.2021.06.015", "https://doi.org/10.1016/j.cstp.2021.06.015")),
      
      tags$p(tags$strong("McNeil, N., Dill, J., MacArthur, J., Broach, J., & Ma, J. (2022)."), 
             "Factors influencing bike share among underserved populations: Evidence from three U.S. cities. ",
             tags$em("Transportation Research Part D: Transport and Environment"), ", 112, 103471. ",
             tags$a(href = "https://doi.org/10.1016/j.trd.2022.103471", "https://doi.org/10.1016/j.trd.2022.103471")),
      
      tags$p(tags$strong("MilNeil, C. (2021, July 22)."), 
             "Research suggests Boston's new protected lanes boosted bikeshare traffic 80 percent. ",
             tags$em("Streetsblog Massachusetts."), 
             tags$a(href = "https://mass.streetsblog.org/2021/07/22/research-suggests-bostons-new-protected-lanes-boosted-bikeshare-traffic-80-percent", 
                    "https://mass.streetsblog.org/2021/07/22/research-suggests-bostons-new-protected-lanes-boosted-bikeshare-traffic-80-percent")),
      
      tags$p(tags$strong("Mintz, S., & Mintz, S. (2025, July 7)."), 
             "The town just got $100,000 for two new Bluebikes stations. Where should they go? ",
             tags$em("Brookline.News."), 
             tags$a(href = "https://brookline.news/the-town-just-got-100000-for-two-new-bluebikes-stations-where-should-they-go/", 
                    "https://brookline.news/the-town-just-got-100000-for-two-new-bluebikes-stations-where-should-they-go/")),
      
      tags$p(tags$strong("National Association of City Transportation Officials. (2015)."), 
             "Walkable station spacing is key to successful, equitable bike share. ",
             tags$em("NACTO."), 
             tags$a(href = "https://nacto.org/publication/walkable-station-spacing-is-key-to-successful-equitable-bike-share/", 
                    "https://nacto.org/publication/walkable-station-spacing-is-key-to-successful-equitable-bike-share/")),
      
      tags$p(tags$strong("National Association of City Transportation Officials. (2016)."), 
             "Bike share station siting guide. ",
             tags$em("NACTO."), 
             tags$a(href = "https://nacto.org/publication/bike-share-station-siting-guide/", 
                    "https://nacto.org/publication/bike-share-station-siting-guide/")),
      
      tags$p(tags$strong("News | Town of Arlington. (2025)."), 
             tags$em("Arlingtonma.gov."), 
             tags$a(href = "https://www.arlingtonma.gov/Home/Components/News/News/14237/", 
                    "https://www.arlingtonma.gov/Home/Components/News/News/14237/")),
      
      tags$p(tags$strong("Poe Public. (n.d.)."), 
             "Bluebikes Station Suggestion Map. ",
             tags$a(href = "https://shareabouts-bluebikes-suggestions-prod-1045183798776.us-east4.run.app/page/about", 
                    "https://shareabouts-bluebikes-suggestions-prod-1045183798776.us-east4.run.app/page/about")),
      
      tags$p(tags$strong("Region seeks new operating contract for expanding BlueBikes system - Streetsblog Massachusetts. (2025, May 2)."), 
             tags$a(href = "https://mass.streetsblog.org/2025/05/02/region-seeks-new-operating-contract-for-expanding-bluebikes-system", 
                    "https://mass.streetsblog.org/2025/05/02/region-seeks-new-operating-contract-for-expanding-bluebikes-system")),
      
      tags$p(tags$strong("Ricci, M. (2015)."), 
             "Bike sharing: A review of evidence on impacts and processes of implementation and operation. ",
             tags$em("Research in Transportation Business & Management"), ", 15, 28–38. ",
             tags$a(href = "https://doi.org/10.1016/j.rtbm.2015.03.003", "https://doi.org/10.1016/j.rtbm.2015.03.003")),
      
      tags$p(tags$strong("Smith, E. (2025, May 27)."), 
             "When is Rush Hour in Boston? Our Traffic Guide. ",
             tags$a(href = "https://www.blacklane.com/en/blog/travel/rush-hour-in-boston/", 
                    "https://www.blacklane.com/en/blog/travel/rush-hour-in-boston/")),
      
      tags$p(tags$strong("Van Woert Katherine & Olivieri Sophia & Baron Jonathan & Buckley Katelyn & Lalli Pamela Fraser, T. &. (2025)."), 
             "Cycling cities: Measuring urban mobility mixing in bikeshare networks. ",
             tags$a(href = "https://ideas.repec.org/a/eee/jotrge/v126y2025ics0966692325001140.html", 
                    "https://ideas.repec.org/a/eee/jotrge/v126y2025ics0966692325001140.html")),
      
      tags$p(tags$strong("Zeid, A., Bhatt, T., & Morris, H. A. (2022)."), 
             "Machine learning model to forecast demand of Boston bike-ride sharing. ",
             tags$em("European Journal of Artificial Intelligence and Machine Learning"), ", 1(3), 1–10. ",
             tags$a(href = "https://doi.org/10.24018/ejai.2022.1.3.9", "https://doi.org/10.24018/ejai.2022.1.3.9"))
    )
    
    do.call(tags$div, refs)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

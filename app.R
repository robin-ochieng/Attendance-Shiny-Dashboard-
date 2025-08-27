# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(DT) 
library(scales)
library(bs4Dash)
library(bslib)
library(fresh)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(hms)
library(writexl) # for Excel downloads
library(shinyWidgets) # stable date range picker

# Define a custom theme using bslib
my_theme <- bs_theme(
  bootswatch = 'minty',
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  
  navbar_fg = "#ffffff"  
)

source("modules/data_processing.R")
source("modules/download_utils.R")
source("modules/metrics.R")
source("modules/earlySignInsData.R")
source("modules/lateSignInsData.R")
source("modules/earlySignOutsData.R")
source("modules/lateSignOutsData.R")
source("modules/customValueBox.R")
#source("modules/attendance_data.R")


# Here's a simplified example of how you might set up the dashboard UI to display these metrics:
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  dashboardHeader(
    tags$li(
      class = "text-center header-title-container",  # Added a new class for more specific styling
      tags$h4("Attendance Dashboard", class = "header-title")
    ),
  titleWidth = 400,
  controlbarIcon = NULL,
  sidebarIcon = NULL,
  fixed = TRUE,
  tags$div(class = "control-bar", actionButton("toggleControlbar", "Filters", class = "btn btn-primary control-button"))
  ),
  dashboardSidebar(
    fixed = TRUE,
    collapsed = FALSE,
    minified = FALSE,
    width = 150,
    sidebarMenu(
      menuItem("Dashboard", tabName = "metrics", icon = icon("chart-line")),
      menuItem("Early Sign Ins", tabName = "data2View", icon = icon("sign-in-alt")),
      menuItem("Late Sign Ins", tabName = "data3View", icon = icon("user-clock")),
      menuItem("Early Sign Outs", tabName = "data4View", icon = icon("sign-out-alt")),
      menuItem("Late Sign Outs", tabName = "data5View", icon = icon("user-clock"))
    ),
    div(class = "sidebar-footer",
        img(src = "images/kenbright2.png")
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon"),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet")
    ),
    tabItems(
      tabItem(tabName = "metrics", metricsUI("metricsMod")),
      #tabItem(tabName = "dataView", attendanceDataUI("attendanceDataMod")),
      tabItem(tabName = "data2View", earlySignInDataUI("earlySignInDataMod")),
      tabItem(tabName = "data3View", lateSignInDataUI("lateSignInDataMod")),
      tabItem(tabName = "data4View", earlySignOutsDataUI("earlySignOutsDataMod")),
      tabItem(tabName = "data5View", lateSignOutsDataUI("lateSignOutsDataMod"))
    ),
    div(class = "body-footer", "© 2025 Attendance Dashboard") 
  ),
  title = "Attendance Dashboard",
  skin = "blue",
  controlbar = dashboardControlbar(  
    skin = "info",
    title = "Filter Settings", 
    id = "dashboardControlbar",
  width = 300,
  collapsed = TRUE,
    bs4Card(
      width = 12,
      title = "Filters",
      background = "white",
      class = "bs4-card-custom",
      radioButtons(
        inputId = "filter_type", 
        label = "Filter by:",
        choices = c("Monthly Report", "All Time Report", "Custom Date Range"), 
        selected = "Monthly Report"
      ),
      # Show Month/Year only when not custom range
      conditionalPanel(
        condition = "input.filter_type !== 'Custom Date Range'",
        selectInput(inputId = "attendance_month", label = "Select Month", choices = NULL),
        selectInput(inputId = "attendance_year", label = "Select Year", choices = NULL)
      ),
      # Custom date range picker (stable positioning)
      conditionalPanel(
        condition = "input.filter_type === 'Custom Date Range'",
        airDatepickerInput(
          inputId = "attendance_daterange",
          label = "Select Date Range",
          value = NULL,
          range = TRUE,
          autoClose = TRUE,
          position = "left bottom",
          update_on = "close",
          dateFormat = "yyyy-mm-dd",
          view = "days",
          monthsField = "monthsShort"
        )
      )
    )
  )
)

server <- function(input, output, session) {

    # Set the time zone globally
  Sys.setenv(TZ = "Africa/Nairobi")

  observeEvent(input$toggleControlbar, {
    bs4Dash::updateControlbar(
      session = session,
      id = "dashboardControlbar"
    )
  })
  
  # Load and process data
  Data <- read_and_process_data("data/Data.xlsx")



  observe({
    req(Data)
    attendance_data <- Data %>%
      mutate(Month = as.character(Month),  
             Month = trimws(Month),
             Year = as.numeric(as.character(Year)))
    # Filter out NA values if present
    month_choices <- attendance_data$Month[!is.na(attendance_data$Month)] %>% unique()
    # Add "All" option for months
    month_choices <- c("All" = "All", month_choices)
    year_choices <- attendance_data$Year[!is.na(attendance_data$Year)] %>% unique()
    # Add "All" option for years
    year_choices <- c("All" = "All", year_choices)
    # Update the select Input for months and years
    updateSelectInput(session, "attendance_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "attendance_year", choices = year_choices, selected = 2024)

    # Initialize date range to the last 30 days within available data (or full range if smaller)
    if (!all(is.na(attendance_data$`Date/Time`))) {
      min_date <- as.Date(min(attendance_data$`Date/Time`, na.rm = TRUE))
      max_date <- as.Date(max(attendance_data$`Date/Time`, na.rm = TRUE))
      start_default <- max(min_date, max_date - lubridate::days(30))
      shinyWidgets::updateAirDateInput(
        session = session,
        inputId = "attendance_daterange",
        value = c(format(start_default, "%Y-%m-%d"), format(max_date, "%Y-%m-%d"))
      )
    }
  })
  

  # Reactive expression to filter the data based on selected month and year
  filtered_data__attendance <- reactive({
    req(Data)
    attendance_data <- Data %>%
      mutate(
        Month = as.character(Month),
        Month = trimws(Month),
        Year  = as.numeric(as.character(Year))
      )

    if (input$filter_type == "Monthly Report") {
      # Last 30 days based on max date in `Date/Time`
      max_dt <- max(attendance_data$`Date/Time`, na.rm = TRUE)
      cutoff_dt <- max_dt - days(30)
      attendance_data <- attendance_data %>%
        filter(`Date/Time` >= cutoff_dt)

    } else if (input$filter_type == "Custom Date Range") {
      # Use the selected date range (inclusive) from airDatepickerInput
      dr <- input$attendance_daterange
      # dr is typically a length-2 vector of "YYYY-MM-DD" strings; be defensive
      if (is.null(dr)) return(attendance_data)
      if (length(dr) == 1 && is.character(dr) && grepl(" to | - ", dr)) {
        # Handle combined string values just in case
        parts <- strsplit(dr, " to | - ")[[1]]
        dr <- parts[1:2]
      }
      req(length(dr) >= 2)
      start_date <- as.Date(dr[1])
      end_date <- as.Date(dr[2])
      attendance_data <- attendance_data %>%
        filter(as.Date(`Date/Time`) >= start_date & as.Date(`Date/Time`) <= end_date)
    } else {
      # Month & Year filter
      if (input$attendance_month == "All") {
        attendance_data <- attendance_data %>%
          filter(Year == as.numeric(input$attendance_year))
      } else {
        attendance_data <- attendance_data %>%
          filter(Month == input$attendance_month, Year == as.numeric(input$attendance_year))
      }
    }

    attendance_data
  })
  
  
  # Recalculate metrics based on filtered data
  attendance_metrics <- reactive({
    calculate_metrics(filtered_data__attendance())
  })
  
  # Recalculate early sign-ins based on filtered data
  EarlySignInDetails <- reactive({
    calculate_early_sign_ins(filtered_data__attendance())
  })
  
  # Recalculate late sign-ins based on filtered data
  LateSignInDetails <- reactive({
    calculate_late_sign_ins(filtered_data__attendance())
  })
  # Recalculate early sign-outs based on filtered data
  EarlySignOutsDetails <- reactive({
    calculate_early_sign_outs(filtered_data__attendance())
  })
  
  # Recalculate late sign-outs based on filtered data
  LateSignOutsDetails <- reactive({
    calculate_late_sign_outs(filtered_data__attendance())
  })
  # Call the attendance metrics module
  metricsServer("metricsMod", filtered_data__attendance, attendance_metrics)
  
  # Call the attendance data module
  #attendanceDataServer("attendanceDataMod", filtered_data__attendance)
  
  #call the early sign-in data module
  earlySignInDataServer("earlySignInDataMod", EarlySignInDetails)
  
  #call the late sign-in data module
  lateSignInDataServer("lateSignInDataMod", LateSignInDetails)
  
  #call the early sign-out data module
  earlySignOutsDataServer("earlySignOutsDataMod", EarlySignOutsDetails)
  
  #call the late sign-in data module
  lateSignOutsDataServer("lateSignOutsDataMod", LateSignOutsDetails)
}

shinyApp(ui, server)




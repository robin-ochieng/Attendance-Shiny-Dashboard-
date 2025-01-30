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
source("modules/metrics.R")
source("modules/earlySignInsData.R")
source("modules/lateSignInsData.R")
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
      menuItem("Dashboard", tabName = "metrics", icon = icon("tachometer-alt")),
      #menuItem("Data View", tabName = "dataView", icon = icon("table")),
      menuItem("Early Sign Ins", tabName = "data2View", icon = icon("clock")),
      menuItem("Late Sign Ins", tabName = "data3View", icon = icon("user-clock"))
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
      tabItem(tabName = "data3View", lateSignInDataUI("lateSignInDataMod"))
    ),
    div(class = "body-footer", "© 2024 Attendance Dashboard") 
  ),
  title = "Attendance Dashboard",
  skin = "blue",
  controlbar = dashboardControlbar(  
    skin = "info",
    title = "Filter Settings", 
    id = "dashboardControlbar",
    width = 300,
    bs4Card(
      width = 12,
      title = "Filters",
      background = "white",
      class = "bs4-card-custom",
      radioButtons(
        inputId = "filter_type", 
        label = "Filter by:",
        choices = c("Weekly Report", "All Time Report"), 
        selected = "Weekly Report"
      ),
      selectInput(inputId = "attendance_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "attendance_year", label = "Select Year", choices = NULL)
    )
  )
)

server <- function(input, output, session) {

    # Set the time zone globally
  Sys.setenv(TZ = "Africa/Nairobi")

  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("dashboardControlbar")
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

    # If user selects "Last 7 days", filter based on max date in `Date/Time` column
    if (input$filter_type == "Weekly Report") {
      # Ensure your `Date/Time` column is properly parsed as a Date or POSIX date-time
      # Adjust the column name below if it differs
      max_date <- max(attendance_data$`Date/Time`, na.rm = TRUE)
      cutoff_date <- max_date - days(7)
      
      attendance_data <- attendance_data %>%
        filter(`Date/Time` >= cutoff_date)
      
    } else {
      # Else user chooses "Month & Year", revert to current month-year filter logic
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
  
  # Call the attendance metrics module
  metricsServer("metricsMod", filtered_data__attendance, attendance_metrics)
  
  # Call the attendance data module
  #attendanceDataServer("attendanceDataMod", filtered_data__attendance)
  
  #call the early sign-in data module
  earlySignInDataServer("earlySignInDataMod", EarlySignInDetails)
  
  #call the late sign-in data module
  lateSignInDataServer("lateSignInDataMod", LateSignInDetails)
}

shinyApp(ui, server)




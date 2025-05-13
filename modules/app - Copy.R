# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(DT) 
library(scales)
library(bs4Dash)
library(bslib)
library(fresh)
library(shinycssloaders)

# Define a custom theme using bslib
my_theme <- bs_theme(
  bootswatch = 'cerulean',
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)

# Read the data
Data <- read_excel("data/Data.xlsx", col_types = c("text", 
                                                   "text", "date", "text", "text", "text", 
                                                   "text", "text"))

# Convert 'Time' column to POSIXct format for easier comparison
Data <- Data %>%
  mutate(Time = as.numeric(Time),  # Ensure Time is numeric
         Time = seconds_to_period(Time * 86400),  # Convert to period
         Hour = hour(Time),  # Extract hour component
         Time_of_Day = if_else(hour(Time) > 6 & hour(Time) < 19 |
                                 (hour(Time) == 6 & minute(Time) > 0) |
                                 (hour(Time) == 19 & minute(Time) == 0),
                               "Day", "Night"),  # Determine Time of Day
         Time_Period = case_when(
           Hour >= 4 & Hour < 10 ~ "Morning (6-10)",
           Hour >= 10 & Hour < 12 ~ "Mid-Morning (10-12)",
           Hour >= 12 & Hour < 13 ~ "Noon (12-13)",
           Hour >= 13 & Hour < 16 ~ "Afternoon (13-16)",
           Hour >= 16 & Hour < 19 ~ "Evening (16-19)",
           Hour >= 19 | Hour < 4 ~ "Night (19-00)",  # Adjusted for logical OR as midnight spans from 19 to before 4
           TRUE ~ "Unknown"  # Default case
         ),
         Day_of_week = weekdays(`Date/Time`),  # Extract the day of the week from 'Date/Time'
         Division_Name = paste(Division, Name, sep = " - "),  # Combine 'Division' and 'Name'
         Time_str = sprintf("%02d:%02d:%02d", as.integer(hour(Time)), as.integer(minute(Time)), as.integer(second(Time))),
         FirstSignInTime = as.POSIXct(paste(`Date/Time`, Time_str), format = "%Y-%m-%d %H:%M:%S")
       )
# Calculating Attendance Metrics
attendance_metrics <- Data %>%
  summarise(
    Total_Sign_In = sum(Status == "Sign In", na.rm = T),
    Total_Sign_Out = sum(Status == "Sign Out", na.rm = T),
    Unique_Days_Counted = n_distinct(`Date/Time`)
  ) %>%
  mutate(
    Sign_In_Frequency = Total_Sign_In / Unique_Days_Counted,
    Sign_In_Rate = Total_Sign_In / (Total_Sign_In + Total_Sign_Out),
    Sign_Out_Frequency = Total_Sign_Out / Unique_Days_Counted,
    Sign_Out_Rate = Total_Sign_Out / (Total_Sign_In + Total_Sign_Out)
  )

# Summarize to find the first sign-in time for each combination of Date/Time, Name, and Division
EarlySignInDetails <- Data %>%
  filter(Status == "Sign In") %>%
  group_by(`Date/Time`, Name, Division) %>%
  summarise(FirstSignInTime = min(FirstSignInTime), .groups = 'drop') %>%
  filter(FirstSignInTime < as.POSIXct(paste(`Date/Time`, "08:15:00"), format = "%Y-%m-%d %H:%M:%S"))


# Now to integrate with your Shiny dashboard using bs4Dash:
# Here's a simplified example of how you might set up the dashboard UI to display these metrics:
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  dashboardHeader(
    title = "Attendance Dashboard",
    titleWidth = 400),
  dashboardSidebar(
    width = 400,
    sidebarMenu(
      menuItem("Attendance Metrics", tabName = "metrics", icon = icon("dashboard")),
      menuItem("Data View", tabName = "dataView", icon = icon("table")),
      menuItem("Early Sign Ins", tabName = "data2View", icon = icon("table"))
    )),
  dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon"),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet"),
      tags$style(HTML("
        body, .content-wrapper, .right-side, .main-footer, .main-header, .sidebar, .control-sidebar {
          font-family: 'Mulish', sans-serif;
        }
        .body-footer {
          position: fixed;
          left: 0;
          bottom: 0;
          width: 100%;
          background-color: #007bff;
          color: white;
          text-align: center;
          padding: 10px;
          z-index: 1030;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "metrics",
              fluidRow(
                valueBoxOutput("totalSignIn", width = 3),
                valueBoxOutput("totalSignOut", width = 3),
                valueBoxOutput("uniqueDaysCounted", width = 3),
                valueBoxOutput("AverageDailySignIns", width = 3),
                valueBoxOutput("AverageDailySignOuts", width = 3),
                valueBoxOutput("signInRate", width = 3),
                valueBoxOutput("signOutRate", width = 3)
              )
      ),
      tabItem(tabName = "dataView",
              box(title = "Attendance Data",
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  width = NULL,
                  maximizable = TRUE,
                  DTOutput("dataTable", width = "100%") %>% withSpinner())),
      tabItem(tabName = "data2View",
              box(title = "Early SignIns Data",
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  width = NULL, 
                  maximizable = TRUE,
                  DTOutput("dataTable2", width = "100%") %>% withSpinner()))
    ),
    div(class = "body-footer", "© 2024 Dashboard - All Rights Reserved")  # Body Footer
  ),
  title = "Dashboard",
  skin = "blue",
  controlbar = dashboardControlbar(  # Adding a control bar for extra settings or information
    controlbarMenu(
      id = "controlbar_menu",
      controlbarItem("Adjustments", icon = icon("tools"),
                     p("Sidebar width:"),
                     sliderInput("sidebar_width", "Width", min = 200, max = 500, value = 300)
      )
    )
  )
)
server <- function(input, output) {
  output$dataTable <- renderDT({
    datatable(Data,
              options = list(
                dom = 't',  # Allows for buttons, searching, pagination
                autoWidth = TRUE,
                responsive = TRUE,
                paging = FALSE, # Disable pagination
                ordering = TRUE, # Enable column ordering
                info = FALSE, # Disable showing table information
                searching = FALSE, # Disable search box
                scrollX = TRUE,
                searchHighlight = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = '_all'), # Center text in all columns
                  list(targets = 4, render = JS(
                    "function(data, type, row) {",
                    "return parseFloat(data).toFixed(0);", # Format 4th column with 2 decimal places
                    "}"
                  )),
                  list(targets = 3, render = JS(
                    "function(data, type, row) {",
                    "var date = new Date(data);",
                    "return (date.getMonth() + 1) + '/' + date.getDate() + '/' + date.getFullYear();", # Format Date column to m/d/yyyy
                    "}"
                  ))
                ),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#007bff', 'color': '#ffffff', 'text-align': 'center'});",
                  "$(this.api().table().node()).css({'table-layout': 'fixed'});",  # Ensure table layout is fixed
                  "}"
                )
              ))
  }, server = FALSE)
  
  
  
  observeEvent(input$toggleData, {
    shinyjs::toggle(id = "dataTable")
  })
  
  output$dataTable2 <- renderDT({
    datatable(EarlySignInDetails,
              options = list(
                dom = 't',  # Allows for buttons, searching, pagination
                autoWidth = FALSE,
                responsive = TRUE,
                paging = FALSE, # Disable pagination
                ordering = TRUE, # Enable column ordering
                info = FALSE, # Disable showing table information
                searching = FALSE, # Disable search box
                scrollX = TRUE,
                searchHighlight = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = '_all') # Center text in all columns
                ),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#007bff', 'color': '#ffffff', 'text-align': 'center'});",
                  "}"
                )
              ))
  }, server = FALSE)  # Non-server side processing for interactive features
  
  observeEvent(input$toggleData, {
    shinyjs::toggle(id = "dataTable2")
  })
  
  output$totalSignIn <- renderValueBox({
    valueBox(
      attendance_metrics$Total_Sign_In,
      subtitle = "Total Sign Ins",
      icon = icon("sign-in-alt"),
      color = "primary"
    )
  })
  output$totalSignOut <- renderValueBox({
    valueBox(
      attendance_metrics$Total_Sign_Out,
      subtitle = "Total Sign Outs",
      icon = icon("sign-out-alt"),
      color = "warning"
    )
  })
  output$uniqueDaysCounted <- renderValueBox({
    valueBox(
      attendance_metrics$Unique_Days_Counted,
      subtitle = "Unique Days Counted",
      icon = icon("calendar"),
      color = "info"
    )
  })
  output$AverageDailySignIns <- renderValueBox({
    valueBox(
      sprintf("%.0f", attendance_metrics$Sign_In_Frequency),
      subtitle = "Average Daily Sign Ins",
      icon = icon("sign-in-alt"),
      color = "success"
    )
  })
  output$signInRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", attendance_metrics$Sign_In_Rate),
      subtitle = "Sign In Rate",
      icon = icon("percent"),
      color = "danger"
    )
  })
  output$AverageDailySignOuts <- renderValueBox({
    valueBox(
      sprintf("%.0f", attendance_metrics$Sign_Out_Frequency),
      subtitle = "Average Daily Sign Outs",
      icon = icon("sign-out-alt"),
      color = "success"
    )
  })
  output$signOutRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", attendance_metrics$Sign_Out_Rate),
      subtitle = "Sign Out Rate",
      icon = icon("percent"),
      color = "danger"
    )
  })
}

shinyApp(ui, server)

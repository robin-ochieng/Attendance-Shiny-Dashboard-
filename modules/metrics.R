# UI for displaying metrics
metricsUI <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    valueBoxOutput(ns("totalSignIn"), width = 3),
    valueBoxOutput(ns("totalSignOut"), width = 3),
    valueBoxOutput(ns("signInRate"), width = 3),
    valueBoxOutput(ns("signOutRate"), width = 3)
  ),
  fluidRow(
      box(title = "Distribution of Attendance by Division", status = "gray", solidHeader = TRUE, 
          plotlyOutput(ns("attendanceByDivision")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Attendance by Time Period", status = "gray", solidHeader = TRUE,
          plotOutput(ns("attendanceByTimePeriod")) %>% withSpinner(type = 6)),
      box(title = "Trend of Attendance by Time of Signing", status = "gray", solidHeader = TRUE,
          plotlyOutput(ns("attendance_over_time")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Attendance by Sign Status", status = "gray", solidHeader = TRUE,
          plotlyOutput(ns("attendanceByStatus")) %>% withSpinner(type = 6)),
  )
 )
}



# Server logic for attendance metrics
metricsServer <- function(id, reactiveData, reactiveMetrics) {
  moduleServer(id, function(input, output, session) {


    output$totalSignIn <- renderValueBox({
    valueBox(
      reactiveMetrics()$Total_Sign_In,
      subtitle = "Total Sign Ins",
      icon = icon("sign-in-alt"),
      color = "primary"
    )
  })
  output$totalSignOut <- renderValueBox({
    valueBox(
      reactiveMetrics()$Total_Sign_Out,
      subtitle = "Total Sign Outs",
      icon = icon("sign-out-alt"),
      color = "olive"
    )
  })

  output$signInRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", reactiveMetrics()$Sign_In_Rate),
      subtitle = "Sign In Rate",
      icon = icon("percent"),
      color = "info"
    )
  })

  output$signOutRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", reactiveMetrics()$Sign_Out_Rate),
      subtitle = "Sign Out Rate",
      icon = icon("percent"),
      color = "lightblue"
    )
  })

  custom_colors_cover <- c("#1f77b4", "#17a2b8", "#2ca02c", "#2ca02c", "#9467bd", "#F28E2B", "#d62728")
  output$attendanceByDivision <- renderPlotly({
    data <- reactiveData()  # Ensure 'sales_data' is already loaded and contains the right columns
    # Filter out rows where Sales or Type of Cover might be NA
    data <- data[!is.na(data$Division), ]
    # Group data by 'Type of Cover' and sum 'Sales', and count the occurrences
    count_by_division <- data %>%
      group_by(`Division`) %>%
      summarise(Count = n(), .groups = 'drop')
    # Generate a qualitative color palette
    num_categories <- length(unique(count_by_division$`Division`))
    # Create the donut chart
    p <- plot_ly(count_by_division, labels = ~`Division`, values = ~Count, type = 'pie', hole = 0.4,
                textposition = 'inside', 
                textinfo = 'label+value+percent',  
                insidetextorientation = 'radial',  
                marker = list(colors = custom_colors_cover),
                textfont = list(color = 'white', family = "Mulish", size = 12))
    # Add title and display the plot
    p <- p %>% layout(title = "Distribution of attendance by Division",
                      showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
})

  output$attendanceByTimePeriod <- renderPlot({
    data <- reactiveData() %>%
      group_by(Time_Period) %>%
      summarize(Count = n(), .groups = 'drop')
    ggplot(data, aes(x = reorder(Time_Period, Count), y = Count, fill = "#0d6efd")) +
      geom_bar(stat = "identity") +  
      scale_fill_identity() +  
      geom_text(aes(label = Count), vjust = 0.5, hjust = -0.15, color = "#333333", size=3.02) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none",
            legend.title = element_text(family = "Mulish", size = 10),
            text = element_text(family = "Mulish"),
            axis.title = element_text(family = "Mulish", size = 12),
            axis.text = element_text(family = "Mulish"),
            axis.text.y = element_text(family = "Mulish", size = 12),
            plot.title = element_text(family = "Mulish", size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(title = "Distrbution of Attendance by Time Period",
           y = "Count", x = "Division")
  })

  output$attendance_over_time <- renderPlotly({
    data <- reactiveData() %>%
      group_by(Hour) %>%
      summarise(Status = n(), .groups = "drop") %>%
      arrange(Hour)  
    # Plotting
    plot_ly(data, x = ~Hour, y = ~Status, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
      layout(
        title = "Trend of Attendance by the Hour of Signing",
        xaxis = list(title = "Hour of the Day", tickvals = ~Hour, ticktext = ~Hour),
        yaxis = list(title = "Number of Sign Ins", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })

  custom_colors_status <- c("#17a2b8", "#87CEEB",  "#d62728","#1f77b4", "#F28E2B")
  output$attendanceByStatus <- renderPlotly({
    data <- reactiveData()  
    # Filter out rows where Sales or Type of Cover might be NA
    data <- data[!is.na(data$Status), ]
    # Group data by 'Type of Cover' and sum 'Sales', and count the occurrences
    count_by_status <- data %>%
      group_by(`Status`) %>%
      summarise(Count = n(), .groups = 'drop')
    # Generate a qualitative color palette
    num_categories <- length(unique(count_by_status$`Status`))
    # Create the donut chart
    p <- plot_ly(count_by_status, labels = ~`Status`, values = ~Count, type = 'pie', hole = 0.4,
                textposition = 'inside', 
                textinfo = 'label+value+percent',  
                insidetextorientation = 'tangential',  
                marker = list(colors = custom_colors_status),
                textfont = list(color = 'white', family = "Mulish", size = 12))
    # Add title and display the plot
    p <- p %>% layout(title = "Distribution of attendance by Status",
                      showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
})

  })
}

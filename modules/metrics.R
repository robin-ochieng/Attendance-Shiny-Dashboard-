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
      box(title = "Distribution of Attendance by Division", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("attendanceByDivision")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Attendance by Time Period", status = "white", solidHeader = TRUE,
          plotOutput(ns("attendanceByTimePeriod")) %>% withSpinner(type = 6)),
      box(title = "Trend of Attendance by Time of Signing", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("attendance_over_time")) %>% withSpinner(type = 6)),
      box(title = "Distribution of Attendance by Sign Status", status = "white", solidHeader = TRUE,
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
      color = "white"
    )
  })
  output$totalSignOut <- renderValueBox({
    valueBox(
      reactiveMetrics()$Total_Sign_Out,
      subtitle = "Total Sign Outs",
      icon = icon("sign-out-alt"),
      color = "white"
    )
  })

  output$signInRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", reactiveMetrics()$Sign_In_Rate),
      subtitle = "Sign In Rate",
      icon = icon("percent"),
      color = "white"
    )
  })

  output$signOutRate <- renderValueBox({
    valueBox(
      sprintf("%.2f%%", reactiveMetrics()$Sign_Out_Rate),
      subtitle = "Sign Out Rate",
      icon = icon("percent"),
      color = "white"
    )
  })

custom_colors_cover <- c("#0d6efd", "#4ac4b5", "#1f77b4", "#2ca02c", "#2ca02c", "#F28E2B", "#d62728")
output$attendanceByDivision <- renderPlotly({
  data <- reactiveData()

  # Filter out rows where Division might be NA and perform data manipulation
  count_by_division <- data %>%
    filter(!is.na(Division)) %>%
    group_by(Division) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    arrange(desc(Count))    

  # Create funnel chart in Plotly
  plot_ly(count_by_division, 
          y = ~reorder(Division, -Count), 
          x = ~Count, 
          type = 'funnel', 
          textinfo = "value+percent",
          marker = list(colors = custom_colors_cover)) %>%
    layout(
      title = "Distribution of Attendance by Division",
      yaxis = list(title = "Division"),
      xaxis = list(title = "Number of Attendees"),
      hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      font = list(family = "Mulish")
    )
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

output$attendanceByStatus <- renderPlotly({
    data <- reactiveData() %>%
      filter(Status != "", !is.na(Status)) %>%
      group_by(Status) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    # Calculate cumulative values for the waterfall chart
    data <- data %>%
      mutate(Cumulative = cumsum(Count))
    
    plot_ly(data, 
            x = ~Status, 
            y = ~Count, 
            type = 'waterfall', 
            texttemplate = ~paste(formatC(Count, format = "f", big.mark = ",", digits = 0), 
                                  " (", round((Count / sum(data$Count)) * 100, 2), "%)", sep = ""),
            textposition = "inside", 
            measure = rep("relative", nrow(data)),  # Set measure to relative for each cover type
            connector = list(line = list(color = "blue", width = 2)),
            increasing = list(marker = list(color = '#0d6efd')),  # Set a professional blue color for increasing values
            decreasing = list(marker = list(color = '#0d6efd')),  # Set the same color for decreasing values
            totals = list(marker = list(color = '#0d6efd'))  # Set the same color for totals if needed
            ) %>%
      layout(
        title = "Distribution of Attendance by Status",
        yaxis = list(title = "Count"),
        xaxis = list(title = "Status"),
        hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        font = list(family = "Mulish")
      )
})
  })
}

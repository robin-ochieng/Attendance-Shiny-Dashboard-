# UI for displaying data tables
earlySignOutsDataUI <- function(id) {
  ns <- NS(id)
  box(title = "Early SignOuts Data",
      status = "white", 
      solidHeader = TRUE, 
      collapsible = TRUE, 
      width = 12, 
      maximizable = TRUE,
      div(id = "divisionSelectContainer",
      selectInput(ns("divisionSelect"), "Select Division:", choices = c("All Divisions", "Advisory", "Corporate and Retail", "Directors Wing"))
      ),
      DTOutput(ns("dataTable4")) %>% withSpinner(type = 6))
}

# Server logic for data tables
earlySignOutsDataServer <- function(id, EarlySignOutsDetails) {
  moduleServer(id, function(input, output, session) {
    filteredData <- reactive({
      data <- EarlySignOutsDetails()
      if (input$divisionSelect != "All Divisions") {
        data <- data[data$Division == input$divisionSelect, ]
      }
      data
    })
    output$dataTable4 <- renderDT({
      datatable(
        filteredData(),
                options = list(
                    dom = '<"top"f>rt<"bottom"lip><"clear">',  # Allows for buttons, searching, pagination
                    autoWidth = TRUE,
                    responsive = TRUE,
                    paging = TRUE,
                    info = TRUE, # Disable showing table information
                    searching = TRUE, 
                    pagelength = 30,
                    scrollX = TRUE,
                    searchHighlight = TRUE,
                    columnDefs = list(
                        list(className = 'dt-center', targets = '_all'),  # Center text in all columns
                        list(targets = 1, render = JS(
                            "function(data, type, row) {",
                            "  if (type === 'display') {",
                            "    var date = new Date(data);",
                            "    return (date.getMonth() + 1) + '/' + date.getDate() + '/' + date.getFullYear();",
                            "  }",
                            "  return data;",
                            "}"
                            )),
                        list(targets = 4, render = JS(
                            "function(data, type, row) {",
                            "  if (type === 'display') {",
                            "    var time = new Date(data);",
                            "    return time.getHours() + ':' + time.getMinutes() + ':' + time.getSeconds();",
                            "  }",
                            "  return data;",
                            "}"
                            ))
                    ),
                    initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#fff', 'color': black, 'text-align': 'center'});",
                    "}"
                    )
                ))
    }, server = FALSE) 
  })
}




 


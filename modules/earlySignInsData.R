# UI for displaying data tables
earlySignInDataUI <- function(id) {
  ns <- NS(id)
  box(title = "Early SignIns Data",
      status = "primary", 
      solidHeader = TRUE, 
      collapsible = TRUE, 
      width = 12, 
      maximizable = TRUE,
      div(id = "divisionSelectContainer",
      selectInput(ns("divisionSelect"), "Select Division:", choices = c("All Divisions", "Advisory", "Corporate and Retail"))
      ),
      DTOutput(ns("dataTable2")) %>% withSpinner(type = 6))
}

# Server logic for data tables
earlySignInDataServer <- function(id, EarlySignInDetails) {
  moduleServer(id, function(input, output, session) {
    filteredData <- reactive({
      data <- EarlySignInDetails()
      if (input$divisionSelect != "All Divisions") {
        data <- data[data$Division == input$divisionSelect, ]
      }
      data
    })
    output$dataTable2 <- renderDT({
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
                    "$(this.api().table().header()).css({'background-color': '#007bff', 'color': '#ffffff', 'text-align': 'center'});",
                    "}"
                    )
                ))
    }, server = FALSE) 
  })
}




 


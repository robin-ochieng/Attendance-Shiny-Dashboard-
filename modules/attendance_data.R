# UI for displaying data tables
attendanceDataUI <- function(id) {
  ns <- NS(id)
            box(title = "Attendance Data",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                maximizable = TRUE,
                DTOutput(ns("dataTable"), width = "100%") %>% withSpinner(type = 6))
}

# Server logic for data tables
attendanceDataServer <- function(id, reactiveData) {
  moduleServer(id, function(input, output, session) {
    output$dataTable <- renderDT({
        data_to_display <- reactiveData()
        datatable(data_to_display,
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

  })
}




 


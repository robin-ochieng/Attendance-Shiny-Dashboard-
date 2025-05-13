# UI for displaying late sign-ins data
lateSignOutsDataUI <- function(id) {
  ns <- NS(id)
  box(title = "Late SignOuts Data ()",
      status = "white",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      maximizable = TRUE,
      div(id = "divisionSelectContainer",
      selectInput(ns("divisionSelect"), "Select Division:", choices = c("All Divisions", "Advisory", "Corporate and Retail", "Directors Wing"))
       ),
      DTOutput(ns("dataTableLate")) %>% withSpinner(type = 6))
}


# Server logic for displaying late sign-ins data
lateSignOutsDataServer <- function(id, LateSignOutsDetails) {
  moduleServer(id, function(input, output, session) {
    filteredData <- reactive({
      data <- LateSignOutsDetails()
      if (input$divisionSelect != "All Divisions") {
        data <- data[data$Division == input$divisionSelect, ]
      }
      data
    })
    output$dataTableLate <- renderDT({
      datatable(
        filteredData(),
        options = list(
          dom = '<"top"f>rt<"bottom"lip><"clear">',   
          autoWidth = TRUE,
          responsive = TRUE,
          paging = TRUE,
          info = TRUE,
          searching = TRUE,
          pageLength = 30,
          scrollX = TRUE,
          searchHighlight = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),  
            list(targets = 1, render = JS(
              "function(data, type, row) {
                if (type === 'display') {
                  var date = new Date(data);
                  return (date.getMonth() + 1) + '/' + date.getDate() + '/' + date.getFullYear();
                }
                return data;
              }"
            )),
            list(targets = 4, render = JS(
              "function(data, type, row) {
                if (type === 'display') {
                  var time = new Date(data);
                  return time.getHours() + ':' + time.getMinutes() + ':' + time.getSeconds();
                }
                return data;
              }"
            ))
          )
        ),
        escape = FALSE
      )
    }, server = FALSE)
  })
}

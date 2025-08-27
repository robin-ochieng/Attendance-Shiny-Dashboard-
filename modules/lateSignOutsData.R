# UI for displaying late sign-outs data
lateSignOutsDataUI <- function(id) {
  ns <- NS(id)
  box(title = "Late SignOuts Data (After 5PM)",
      status = "white",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      maximizable = TRUE,
  div(class = "divisionSelectContainer",
      selectInput(ns("divisionSelect"), "Select Division:", choices = c("All Divisions", "Advisory", "Corporate and Retail", "Directors Wing"))
       ),
  # DataTables provides export buttons; custom shiny download buttons removed per request
      DTOutput(ns("dataTableLate")) %>% withSpinner(type = 6))
}


# Server logic for displaying late sign-outs data
lateSignOutsDataServer <- function(id, LateSignOutsDetails) {
  moduleServer(id, function(input, output, session) {

    filteredData <- reactive({
      data <- LateSignOutsDetails()
      req(!is.null(data))
      if (input$divisionSelect != "All Divisions") {
        data <- data[data$Division == input$divisionSelect, ]
      }
      data
    })

    # CSV download
    output$download_csv <- downloadHandler(
      filename = function() paste0("late_signouts_", Sys.Date(), ".csv"),
      content = function(file) {
        dat <- filteredData()
        write_csv_safe(dat, file)
      },
      contentType = "text/csv"
    )

    # Excel download
    output$download_xlsx <- downloadHandler(
      filename = function() paste0("late_signouts_", Sys.Date(), ".xlsx"),
      content = function(file) {
        dat <- filteredData()
        write_xlsx_safe(dat, file)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    # DataTable render with export buttons (as an extra UX option)
    output$dataTableLate <- renderDT({
      datatable(
        filteredData(),
        options = list(
          dom = '<"top"Bf>rt<"bottom"lip><"clear">',
    buttons = list(
      list(extend = "csv",
        text = '<i class="fas fa-file-csv"></i> CSV',
        title = paste0("late_signouts_", Sys.Date()),
        className = "btn btn-sm export-btn"),
      list(extend = "excel",
        text = '<i class="fas fa-file-excel"></i> Excel',
        title = paste0("late_signouts_", Sys.Date()),
        className = "btn btn-sm export-btn")
    ),
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
              "function(data, type, row) {\n                if (type === 'display') {\n                  var date = new Date(data);\n                  return (date.getMonth() + 1) + '/' + date.getDate() + '/' + date.getFullYear();\n                }\n                return data;\n              }"
            )),
            list(targets = 4, render = JS(
              "function(data, type, row, meta) {\n  function pad(n){ return (n<10?'0':'') + n; }\n  function fmt12(h, m){ var p = h >= 12 ? 'PM' : 'AM'; h = h % 12; if (h === 0) h = 12; return pad(h) + ':' + pad(m) + ' ' + p; }\n  if (type !== 'display' && type !== 'filter') return data;\n  if (data == null || data === '') return '';\n  var m = /^\\s*(\\d{1,2}):(\\d{1,2})(?::(\\d{1,2}))?/.exec(data);\n  if (m) { var h = parseInt(m[1],10), min = parseInt(m[2],10); if (isNaN(h) || isNaN(min)) return data; return fmt12(h, min); }\n  var d = new Date(data);\n  if (!isNaN(d.getTime())) { return fmt12(d.getHours(), d.getMinutes()); }\n  return data;\n}"
            ))
          )
        ),
        escape = FALSE,
        extensions = c('Buttons')
      )
    }, server = FALSE)
  })
}

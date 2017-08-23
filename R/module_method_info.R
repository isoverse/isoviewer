#' Method Info Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @param visible reactive function determining visibility of the auxiliary boxes
#' @family method info module functions
methodInfoServer <- function(input, output, session, isofiles, visible = NULL) {

  # namespace
  ns <- session$ns

  # file info selector
  # file_info_selector <- callModule(
  #   selectorTableServer, "selector", id_column = "info", col_headers = c("Info"))

  # generate selector list
  # observe({
  #   req(length(isofiles()) > 0)
  #   columns <- names(aggregate_file_info(isofiles(), quiet = TRUE)) %>%
  #   { .[!. %in% c("file_id", "file_path", "file_subpath")] } # do not allow file path while on server
  #   # set table
  #   file_info_selector$set_table(data_frame(info = columns), initial_selection = "file_datetime")
  # })

  # show selector box
  observe({
    if (is.function(visible))
      toggle("selector_box", condition = visible() & length(isofiles()) > 0 )
    else
      toggle("selector_box", condition = length(isofiles()) > 0)
  })

  # standard info
  output$standards_table <- renderTable({
    validate(need(length(isofiles()) > 0, "Please select a dataset and at least one data file."))
    module_message(ns, "debug", "rendering standards info table")
    aggregate_standards_info(isofiles(), quiet = TRUE)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # resistors info
  output$resistors_table <- renderTable({
    validate(need(length(isofiles()) > 0, "Please select a dataset and at least one data file."))
    module_message(ns, "debug", "rendering resistors info table")
    aggregate_resistors_info(isofiles(), quiet = TRUE)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)

  # toggle visibility
  observeEvent(input$selector, {
    toggle("standards", condition = input$selector == "standards")
    toggle("resistors", condition = input$selector == "resistors")
  })


  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_method_info_code(
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update
  )
}


#' Method Info Table UI
#' @inheritParams isofilesLoadUI
#' @family method info module functions
methodInfoTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("standards"), style = 'overflow-x: scroll; height: 400px;',
        tableOutput(ns("standards_table")) %>% withSpinner(type = 5, proxy.height = "400px;")),
    hidden(div(id = ns("resistors"), style = 'overflow-x: scroll; height: 400px;',
        tableOutput(ns("resistors_table")) %>% withSpinner(type = 5, proxy.height = "400px;")))
  )
}


#' Method Info Selector UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family file info module functions
methodInfoSelectorUI <- function(id, width = 4) {
  ns <- NS(id)
  div(id = ns("selector_box"),
      default_box(
        title = "Method Info Selector", width = width,
        radioButtons(ns("selector"), label = NULL,
                     choices = c("Standards" = "standards", "Resistors" = "resistors"))
      )
  ) %>% hidden()
}

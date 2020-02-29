#' Vendor Data Table Server
#' @inheritParams fileInfoServer
#' @family vendor data table module functions
vendorDataTableServer <- function(input, output, session, isofiles, visible = NULL) {

  # namespace
  ns <- session$ns

  # file info selector
  vdt_selector <- callModule(
    selector_table_server, "selector", id_column = "column", col_headers = c("Column"))

  # generate selector list
  observe({
    req(length(isofiles()) > 0)
    columns <- names(iso_get_vendor_data_table(isofiles(), quiet = TRUE)) %>%
    { .[!. %in% c("file_id")] }
    # set table
    vdt_selector$set_table(tibble::tibble(column = columns))
  })

  # show selector list box
  observe({
    if (is.function(visible))
      toggle("selector_box", condition = visible() & length(isofiles()) > 0 )
    else
      toggle("selector_box", condition = length(isofiles()) > 0)
  })

  # vendor data table
  output$table <- renderTable({
    validate(
      need(length(isofiles()) > 0, "Please select a dataset and at least one data file.") %then%
        need(length(vdt_selector$get_selected()) > 0, "Please select at least one vendor data table column.")
    )
    module_message(ns, "debug", "rendering vendor data table")
    isoreader::iso_get_vendor_data_table(isofiles(), select = c("file_id", !!vdt_selector$get_selected()), quiet = TRUE)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = NULL)


  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_vendor_data_table_code(
        selection = c("file_id", vdt_selector$get_selected()),
        # NOTE: could think about omitting the "select" clause if all are selected
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update
  )
}


#' File Info Table UI
#' @inheritParams isofilesLoadUI
#' @family vendor data table module functions
vendorDataTableTableUI <- function(id) {
  ns <- NS(id)
  div(style = 'overflow-x: scroll; height: 400px;',
      tableOutput(ns("table")) %>% withSpinner(type = 5, proxy.height = "400px;"))
}


#' File Info Selector UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @param selector_height file selector height
#' @family vendor data table module functions
vendorDataTableSelectorUI <- function(id, width = 4, selector_height = "200px") {
  ns <- NS(id)

  div(id = ns("selector_box"),
      default_box(
        title = "Data Table Selector", width = width,
        selector_table_ui(ns("selector"), height = selector_height),
        footer = div(
          #style = "height: 35px;",
          selector_table_buttons_ui(ns("selector")))
      )
  )%>% hidden()
}

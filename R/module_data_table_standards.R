#' Standards Server
#' @inheritParams data_table_server
data_table_standards_server <- function(input, output, session, settings, get_variable, get_iso_files, is_visible) {

  # namespace
  ns <- session$ns

  # base data table
  data_table <-
    callModule(
      data_table_server,
      "data_table",
      settings = settings,
      get_variable = get_variable,
      get_iso_files = get_iso_files,
      is_visible = is_visible,
      # get standards
      get_data_table = function(iso_files, selected) {
        isoreader::iso_get_standards(iso_files, select = c(!!!selected), quiet = TRUE)
      },
      # get standards columns
      get_data_table_columns = function(iso_files) {
        columns <- names(isoreader::iso_get_standards(iso_files, quiet = TRUE))
        columns <- columns[!columns %in% c("file_id")]
        return(dplyr::tibble(Column = columns))
      }
    )

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_standards_code(
        dataset = get_variable(),
        selection =
          if (is.null(data_table$get_selected_columns())) list(rlang::expr(c()))
          else if (data_table$are_all_columns_selected()) list(rlang::expr(everything()))
          else purrr::map(data_table$get_selected_columns(), rlang::sym),
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update
  )
}


#' Standards Table UI
#' @param ... passed on to data_table_ui
data_table_standards_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_ui(ns("data_table"), ...)
}

#' Standards Column Seletor UI
#' @param ... passed on to data_table_column_selector_ui
data_table_standards_column_selector_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_column_selector_ui(ns("data_table"), ...)
}

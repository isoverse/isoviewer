#' File Info Server
#' @inheritParams data_table_server
data_table_file_info_server <- function(input, output, session, get_variable, get_iso_files, is_visible) {

  # namespace
  ns <- session$ns

  # base data table ====
  data_table <-
    callModule(
      data_table_server,
      "data_table",
      get_variable = get_variable,
      get_iso_files = get_iso_files,
      is_visible = is_visible,
      # get file info
      get_data_table = function(iso_files, selected) {
        isoreader::iso_get_file_info(iso_files, select = c(!!!selected), quiet = TRUE) %>%
          isoreader:::collapse_list_columns() %>%
          isoreader:::iso_strip_units() # just in case user introduced some
      },
      # get file info columns
      get_data_table_columns = function(iso_files) {
        columns <- names(isoreader::iso_get_file_info(iso_files, quiet = TRUE))
        columns <- columns[!columns %in% c("file_id", "file_root")] # do not allow file_root while on server
        return(dplyr::tibble(Column = columns))
      }
    )

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_file_info_code(
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


#' File Info Table UI
#' @param ... passed on to data_table_ui
data_table_file_info_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_ui(ns("data_table"), ...)
}

#' File Info Column Seletor UI
#' @param ... passed on to data_table_column_selector_ui
data_table_file_info_column_selector_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_column_selector_ui(ns("data_table"), ...)
}

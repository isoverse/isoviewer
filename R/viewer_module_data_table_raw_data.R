#' Raw Data Server
#' @inheritParams data_table_server
data_table_raw_data_server <- function(input, output, session, get_variable, get_iso_files, is_visible) {

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
      # get raw data
      get_data_table = reactive({

        # parameters
        sig_digits <- input$signif
        if (is.null(sig_digits)) sig_digits <- 4

        # store parameters
        isolate(set_gui_setting(ns(paste0("signif-", get_variable())), sig_digits))

        # table selection function
        function(iso_files, selected) {
          isoreader::iso_get_raw_data(iso_files, select = c(!!!selected), quiet = TRUE) %>%
            isoreader::iso_strip_units() %>%
            dplyr::mutate_if(is.numeric, signif, sig_digits)
        }

      }),
      # get columns
      get_data_table_columns = function(iso_files) {
        columns <- names(isoreader::iso_get_raw_data(iso_files, quiet = TRUE))
        columns <- columns[!columns %in% c("file_id", "file_root")] # do not allow file_root while on server
        return(dplyr::tibble(Column = columns))
      }
    )

  # restore settings ====
  observeEvent(get_variable(), {
    sig_digits <- get_gui_setting(ns(paste0("signif-", get_variable())), default = 4)
    updateNumericInput(session, "signif", value = sig_digits)
  })


  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_raw_data_code(
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


#' Raw Data Table UI
#' @param ... passed on to data_table_ui
data_table_raw_data_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_ui(ns("data_table"), ...)
}

#' Raw Data Column Seletor UI
#' @param ... passed on to data_table_column_selector_ui
data_table_raw_data_column_selector_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_column_selector_ui(
    ns("data_table"),
    pre_table =
      tagList(
        fluidRow(
          h4("Significant Digits:") %>% column(width = 6),
          numericInput(ns("signif"), NULL, value = 4, min = 1) %>% column(width = 6)
        )
      ),
    ...
  )
}

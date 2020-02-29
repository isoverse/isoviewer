#' Peak Table  Server
#' @inheritParams data_table_server
data_table_peak_table_server <- function(input, output, session, get_variable, get_iso_files, is_visible) {

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
      # get peak_table =====
      get_data_table = reactive({

        # parameters
        sig_digits <- input$signif
        if (is.null(sig_digits)) sig_digits <- 4
        expl_units <- input$units
        if (is.null(expl_units)) expl_units <- TRUE

        # store parameters
        isolate(set_gui_setting(ns(paste0("signif-", get_variable())), sig_digits))
        isolate(set_gui_setting(ns(paste0("units-", get_variable())), expl_units))

        # table selection function
        function(iso_files, selected) {
          # get peak table and validate
          pt <- suppressWarnings(isoprocessor::iso_get_peak_table(iso_files, select = c(!!!selected), quiet = TRUE))
          validate(need(nrow(pt) > 0, message = "Error: none of the iso files has a peak table yet. To use peak table functionality, make sure to create a peak table by setting it manually (iso_set_peak_table), or setting it automatically from the vendor data table (iso_set_peak_table_automatically_from_vendor_data_table)"))

          # deal with units
          if (expl_units) pt <- isoreader::iso_make_units_explicit(pt)
          else pt <- isoreader::iso_strip_units(pt)

          # return
          dplyr::mutate_if(pt, is.numeric, signif, sig_digits)
        }
      }),
      # get peak_table columns =====
      get_data_table_columns = function(iso_files) {
        vdt <- suppressWarnings(isoprocessor::iso_get_peak_table(iso_files, quiet = TRUE)) %>%
          dplyr::select(-file_id)
        return(dplyr::tibble(Column = names(vdt), Units = isoreader::iso_get_units(vdt)))
      }
    )


  # restore settings ====
  observeEvent(get_variable(), {
    sig_digits <- get_gui_setting(ns(paste0("signif-", get_variable())), default = 4)
    updateNumericInput(session, "signif", value = sig_digits)
    expl_units <- get_gui_setting(ns(paste0("units-", get_variable())), default = TRUE)
    updateCheckboxInput(session, "units", value = expl_units)
  })

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_peak_table_code(
        dataset = get_variable(),
        selection =
          if (is.null(data_table$get_selected_columns())) list(rlang::expr(c()))
          else if (data_table$are_all_columns_selected()) list(rlang::expr(everything()))
          else purrr::map(data_table$get_selected_columns(), rlang::sym),
        explicit_units = input$units,
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update
  )
}


#' Peak Table  Table UI
#' @param ... passed on to data_table_ui
data_table_peak_table_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_ui(ns("data_table"), ...)
}

#' Peak Table  Column Seletor UI
#' @param ... passed on to data_table_column_selector_ui
data_table_peak_table_column_selector_ui <- function(id, ...) {
  ns <- NS(id)
  data_table_column_selector_ui(
    ns("data_table"),
    pre_table =
      tagList(
        fluidRow(
          h4("Explicit Units:") %>% column(width = 6),
          checkboxInput(ns("units"), NULL, value = TRUE) %>% column(width = 6)
        ),
        fluidRow(
          h4("Significant Digits:") %>% column(width = 6),
          numericInput(ns("signif"), NULL, value = 4, min = 1) %>% column(width = 6)
        )
      ),
    ...
  )
}

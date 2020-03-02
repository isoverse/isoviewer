#' select data trace
trace_selector_server <- function(input, output, session, settings, get_variable, get_iso_files) {

  # namespace
  ns <- session$ns

  # data trace selector =====
  selector <-
    callModule(
      selector_table_server, "selector",
      settings = settings,
      id_column = "data",
      row_column = "rowid",
      column_select = c(Trace = label)
    )

  # generate data trace selector list ====
  observeEvent(get_iso_files(), {
    req(length(get_iso_files()) > 0)
    datas <- get_iso_files() %>%
      isoreader::iso_get_raw_data(gather = TRUE, quiet = TRUE) %>%
      dplyr::select(.data$data, .data$category, .data$units) %>%
      unique() %>%
      dplyr::mutate(
        label = ifelse(
          !is.na(.data$units),
          sprintf("%s %s [%s]", .data$category, .data$data, .data$units),
          paste(.data$category, .data$data)
        ),
        rowid = dplyr::row_number()
      )
    selected <- settings$get(ns(get_variable()), default = NULL)
    selector$set_table(datas)
    selector$set_selected(selected)
  })

  # monitor data trace selector ======
  observeEvent(selector$get_selected(), {
    # info
    module_message(
      ns, "info", sprintf(
        "TRACE TABLE user selected %s for '%s'",
        paste(selector$get_selected(), collapse = ", "), get_variable()
      )
    )
    # store selected in settings
    settings$set(ns(get_variable()), selector$get_selected())
  })

  # get selected traces =====
  # (for syntax with plotting functions)
  get_selected_traces <- reactive({
    if (length(selector$get_selected()) == 0) NULL
    else if (selector$are_all_selected()) character(0)
    else selector$get_selected()
  })

  # return functions =====
  list(
    get_selected = selector$get_selected,
    get_selected_traces = get_selected_traces
  )

}


#' trace selector ui
trace_selector_table_ui <- function(id) {
  ns <- NS(id)
  selector_table_ui(ns("selector"))
}

#' trace selector buttons ui
trace_selector_table_buttons_ui <- function(id) {
  ns <- NS(id)
  selector_table_buttons_ui(ns("selector"))
}


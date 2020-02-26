#' Basic data table server with column selection option
#'
#' This is for the usual iso_get_... aggregation functions.
#'
#' @param get_variable reactive function returning the selected variable
#' @param get_iso_files reactive function returning the currently loaded isofiles
#' @param get_data_table a regular or reactive function taking iso_files and a list of column names to retrieve the data table
#' @param get_data_table_columns a regular function taking iso_files and returning a vector of data table columns
#' @param is_visible reactive function determining visibility of the auxiliary boxes
#' @family file info module functions
data_table_server <- function(input, output, session, get_variable, get_iso_files, is_visible, get_data_table, get_data_table_columns) {

  # namespace
  ns <- session$ns

  # file info selector
  selector <-
    callModule(
      selectorTableServer,
      "selector",
      id_column = "Column",
      row_column = "rowid",
      column_select = c(-rowid)
    )

  # generate selector list ====
  observeEvent(get_iso_files(), {
    req(length(get_iso_files()) > 0)
    columns_tbl <- get_data_table_columns(get_iso_files())
    stopifnot("Column" %in% names(columns_tbl))
    selected <- get_gui_setting(ns(paste0("selector-", get_variable())), default = NULL)
    selector$set_table(dplyr::mutate(columns_tbl, rowid = dplyr::row_number()))
    selector$set_selected(selected)
  })

  # show selector box ====
  observeEvent(is_visible(), { toggle("selector_box", condition = is_visible()) })

  # get selected file info =====
  get_selected_data_table <- reactive({
    # triger for both iso files and selected info columns
    validate(need(length(get_iso_files()) > 0, "loading..."))
    selector$get_selected()

    # info message
    isolate(
      module_message(
        ns, "info", sprintf(
          "DATA TABLE user selected %d/%d columns for '%s'",
          length(selector$get_selected()), selector$get_table_nrow(), get_variable())
      )
    )

    # store selected in settings
    isolate(set_gui_setting(ns(paste0("selector-", get_variable())), selector$get_selected()))

    # get file info
    if (shiny::is.reactive(get_data_table)) {
      # retrieve function from reactive first
      get_data_table()(get_iso_files(), selector$get_selected())
    } else {
      # call function directly
      get_data_table(get_iso_files(), selector$get_selected())
    }
  })

  # file info table =====
  output$table <- DT::renderDataTable({
    req(get_selected_data_table())
    module_message(ns, "info", "DATA TABLE rendering table")
    DT::datatable(
      get_selected_data_table(),
      options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10),
      filter = "bottom"
    )
  })

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_file_info_code(
        dataset = get_variable(),
        selection =
          if (is.null(selector$get_selected())) list(rlang::expr(c()))
          else if (selector$are_all_selected()) list(rlang::expr(everything()))
          else purrr::map(selector$get_selected(), rlang::sym),
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions
  list(
    get_selected_columns = selector$get_selected,
    are_all_columns_selected = selector$are_all_selected
  )
}


#' Data Table UI
data_table_ui <- function(id, min_height = "800px;") {
  ns <- NS(id)
  div(style = paste0('overflow-x: scroll; min-height: ', min_height),
      DT::dataTableOutput(ns("table")) %>% withSpinner(type = 5, proxy.height = min_height))
}

#' Column Selector UI
#' @param width box width
data_table_column_selector_ui <- function(id, width = 4, pre_table = list(), post_table = list()) {
  ns <- NS(id)
  div(id = ns("selector_box"),
      default_box(
        title = "Column Selector", width = width,
        pre_table,
        selectorTableUI(ns("selector")),
        post_table,
        footer = div(selectorTableButtons(ns("selector")))
      )
  )%>% shinyjs::hidden()
}

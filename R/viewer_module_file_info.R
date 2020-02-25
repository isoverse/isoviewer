#' File Info Server
#' @param get_variable reactive function returning the selected variable
#' @param get_iso_files reactive function returning the currently loaded isofiles
#' @param is_visible reactive function determining visibility of the auxiliary boxes
#' @family file info module functions
file_info_server <- function(input, output, session, get_variable, get_iso_files, is_visible) {

  # namespace
  ns <- session$ns

  # file info selector
  selector <-
    callModule(
      selectorTableServer,
      "selector",
      id_column = "info",
      row_column = "rowid",
      column_select = c(Info = info)
    )

  # generate selector list ====
  observeEvent(get_iso_files(), {
    req(length(get_iso_files()) > 0)
    columns <- names(isoreader::iso_get_file_info(get_iso_files(), quiet = TRUE))
    columns <- columns[!columns %in% c("file_id", "file_root")] # do not allow file_root while on server
    selected <- get_gui_setting(ns(paste0("selector-", get_variable())), default = NULL)
    selector$set_table(tibble::tibble(info = columns, rowid = 1:length(columns)))
    selector$set_selected(selected)
  })

  # show selector box ====
  observeEvent(is_visible(), { toggle("selector_box", condition = is_visible()) })

  # get selected file info =====
  get_selected_file_info <- reactive({
    # triger for both iso files and selected info columns
    validate(need(length(get_iso_files()) > 0, "loading..."))
    selector$get_selected()

    isolate({
      # info message
      module_message(
        ns, "info", sprintf(
          "FILE INFO user selected %d/%d file info columns for '%s'",
          length(selector$get_selected()), selector$get_table_nrow(), get_variable())
      )

      # store selected in settings
      set_gui_setting(ns(paste0("selector-", get_variable())), selector$get_selected())

      # get file info
      isoreader::iso_get_file_info(get_iso_files(), select = c(!!!selector$get_selected()), quiet = TRUE) %>%
        isoreader:::collapse_list_columns()
    })
  })

  # file info table =====
  output$table <- DT::renderDataTable({
    req(get_selected_file_info())
    module_message(ns, "info", "FILE INFO rendering file info table")
    DT::datatable(
      get_selected_file_info(),
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
    get_code_update = code_update
  )
}


#' File Info Table UI
#' @family file info module functions
file_info_table_ui <- function(id, min_height = "800px;") {
  ns <- NS(id)
  div(style = paste0('overflow-x: scroll; min-height: ', min_height),
      DT::dataTableOutput(ns("table")) %>% withSpinner(type = 5, proxy.height = min_height))
}


#' File Info Selector UI
#' @param width box width
#' @family file info module functions
file_info_selector_ui <- function(id, width = 4) {
  ns <- NS(id)
  div(id = ns("selector_box"),
      default_box(
        title = "File Info Selector", width = width,
        selectorTableUI(ns("selector")),
        footer = div(selectorTableButtons(ns("selector")))
      )
  )%>% shinyjs::hidden()
}

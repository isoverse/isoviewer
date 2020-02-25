#' Method Info Server
#' @inheritParams file_info_server
method_info_server <- function(input, output, session, get_variable, get_iso_files, is_visible) {

  # namespace
  ns <- session$ns

  # show selector box ====
  observeEvent(is_visible(), {
    toggle("selector_box", condition = is_visible())
  })

  # select default radio button ====
  observeEvent(get_variable(), {
    updateRadioButtons(
      session, "selector",
      selected = get_gui_setting(ns(paste0("selector-", get_variable())), default = "resistors")
    )
  })

  # standard info =====
  output$standards_table <- DT::renderDataTable({
    validate(need(length(get_iso_files()) > 0, "loading..."))
    module_message(ns, "info", "METHOD INFO rendering standards info table")
    DT::datatable(
      isoreader::iso_get_standards(get_iso_files(), quiet = TRUE),
      options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10),
      filter = "bottom"
    )
  })

  # resistors info
  output$resistors_table <- DT::renderDataTable({
    validate(need(length(get_iso_files()) > 0, "loading..."))
    module_message(ns, "info", "METHOD INFO rendering resistors info table")
    DT::datatable(
      isoreader::iso_get_resistors(get_iso_files(), quiet = TRUE),
      options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10),
      filter = "bottom"
    )
  })

  # toggle visibility
  observeEvent(input$selector, {
    # store selected in settings
    set_gui_setting(ns(paste0("selector-", get_variable())), input$selector)
    shinyjs::toggle("standards", condition = (input$selector == "standards"))
    shinyjs::toggle("resistors", condition = (input$selector == "resistors"))
  })

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_method_info_code(
        dataset = get_variable(),
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
#' @family method info module functions
method_info_table_ui <- function(id, min_height = "800px;") {
  ns <- NS(id)
  tagList(
    div(id = ns("standards"), style = paste0('overflow-x: scroll; min-height: ', min_height),
        DT::dataTableOutput(ns("standards_table")) %>% withSpinner(type = 5, proxy.height = min_height)
    ) %>% shinyjs::hidden(),
    div(id = ns("resistors"), style = paste0('overflow-x: scroll; min-height: ', min_height),
        DT::dataTableOutput(ns("resistors_table")) %>% withSpinner(type = 5, proxy.height = min_height)
    ) %>% shinyjs::hidden()
  )
}


#' Method Info Selector UIUI
#' @param width box width
#' @family file info module functions
method_info_selector_ui <- function(id, width = 4) {
  ns <- NS(id)
  div(id = ns("selector_box"),
      default_box(
        title = "Method Info Selector", width = width,
        radioButtons(
          ns("selector"), label = NULL,
          choices = c("Resistors" = "resistors", "Standards" = "standards"))
      )
  ) %>% hidden()
}

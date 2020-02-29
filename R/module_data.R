#' Data server (main module)
#'
#' @param get_selected_variable reactive function returning the variable name
#' @param data_type the type of data server (character), is used to construct iso_read... and iso_is_... calls
#' @param get_code_update a reactive code update function, has to return a function that takes 'rmarkdown' as a parameter
module_data_server <- function(input, output, session, get_selected_variable,
                               data_type = c("continuous_flow", "dual_inlet", "scan"),
                               get_code_update = reactive({ function(rmarkdown) {""} })) {

  # TODO: implement code jumping
  # # code jumping
  # observe({
  #   req(cf_data$get_data_tab())
  #
  #   if (is.null(cf_dataset$get_dataset_path()))
  #     search_term <- "load dataset"
  #   else {
  #     search_term <-
  #       switch(cf_data$get_data_tab(),
  #              raw_data = "raw data",
  #              file_info = "file info",
  #              method_info = "method info",
  #              vendor_data_table = "vendor data table",
  #              export = "export",
  #              NULL)
  #   }
  #
  #   module_message(ns, "debug", "jumping into code at: ", search_term)
  #
  #   if (!is.null(search_term))
  #     code_preview$focus_code_preview(search = search_term, center = TRUE, case_sensitive = FALSE)
  # })


  # namespace
  ns <- session$ns

  # reactive values =====
  values <- reactiveValues(
    data_tabs_visible = NULL
  )

  # file selector module =====
  variable_check_func <- rlang::parse_expr(sprintf("isoreader::iso_is_%s(obj)", data_type))
  files <- callModule(
    module_file_selector_server, "files",
    get_variable = get_selected_variable,
    get_iso_files = reactive({
      req(get_selected_variable())
      obj <- get(get_selected_variable(), envir = .GlobalEnv)
      stopifnot(rlang::eval_tidy(variable_check_func))
      return(obj)
    })
  )

  # page visibility =====
  observeEvent(get_selected_variable(), {
    shinyjs::hide("div")
    shinyjs::hide("no_data")
    if (!is.null(get_selected_variable()) && !is.na(get_selected_variable())) {
      selected_tab <- get_gui_setting(ns(paste0("tabs-", get_selected_variable())), default = "plot")
      module_message(ns, "info", sprintf("DATA loading screen for variable '%s' on tab '%s'",
                                         get_selected_variable(), selected_tab))
      updateTabsetPanel(session, "tabs", selected = selected_tab)
      shinyjs::show("div")
    } else if (!is.null(get_selected_variable()) && is.na(get_selected_variable())) {
      shinyjs::show("no_data")
    }
  }, ignoreNULL = FALSE)

  # data tabs visibility ====
  observeEvent(files$get_selected_iso_files(), {
    if (values$data_tabs_visible) {
      shinyjs::show("data_div")
    } else {
      shinyjs::hide("data_div")
    }
  }, ignoreNULL = TRUE)
  observeEvent(files$get_selected_iso_files(), {
    values$data_tabs_visible <- length(files$get_selected_iso_files()) > 0
  }, priority = 50, ignoreNULL = FALSE, ignoreInit = FALSE)

  # selecting tabs ====
  get_tab_selection <- reactive(input$tabs)
  observeEvent(get_tab_selection(), {
    module_message(ns, "info", sprintf("DATA TABS user selected tab '%s'", input$tabs))
    set_gui_setting(ns(paste0("tabs-", get_selected_variable())), input$tabs)
  }, priority = 100, ignoreInit = TRUE)

  # code preview ====
  code_update <-  reactive({
    function(rmarkdown = TRUE, front_matter = rmarkdown) {
      module_message(ns, "info", "CODE (re-) generating code preview")
      code(
        generate_file_header_code(
          title = paste("Viewing", get_selected_variable()),
          dataset = get_selected_variable(),
          read_func = paste0("iso_read_", data_type),
          setup = TRUE, load = TRUE,
          rmarkdown = rmarkdown, front_matter = front_matter),
        files$get_code_update()(rmarkdown = rmarkdown),
        get_code_update()(rmarkdown = rmarkdown),
        "" # final new line
      )
    }
  })
  code_preview <- callModule(
    code_preview_server, "code", code_func_reac = code_update,
    download_file = reactive({ paste("VIEW", get_selected_variable()) })
  )

  # return functions =====
  list(
    get_selected_iso_files = files$get_selected_iso_files,
    get_tab_selection = get_tab_selection
  )
}

#' Continuous flow files UI
module_data_ui <- function(id, tab_panels = list(), option_boxes = list()) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("div"),
      # file selector
      module_file_selector_ui(ns("files"), width = 6),
      # code preview
      code_preview_ui(ns("code"), width = 6, height = "345px"),
      # data tabs
      div(id = ns("data_div"),
          fluidRow(
            column(
              width = 12,
              default_box(
                title = "Data", width = 8,
                do.call(tabsetPanel, args = c(
                  list(
                    id = ns("tabs")
                  ), tab_panels)
                )
              ),
              option_boxes
            )
          )
      ) %>% shinyjs::hidden()
    ) %>% shinyjs::hidden(),
    # no data box
    div(
      id = ns("no_data"),
      box(h2("Your workspace does not contain any variable that is this type of iso file object."), width = 12)
    ) %>% shinyjs::hidden()
  )
}


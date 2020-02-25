#' Continuous flow files Server
#' @param get_selected_variable reactive function returning the variable name
module_cf_server <- function(input, output, session, get_selected_variable) {

  # namespace
  ns <- session$ns

  # reactive values =====
  values <- reactiveValues(
    data_tabs_visible = NULL
  )

  # file selector module =====
  files <- callModule(
    module_file_selector_server, "files",
    get_variable = get_selected_variable,
    get_iso_files = reactive({
      req(get_selected_variable())
      obj <- get(get_selected_variable(), envir = .GlobalEnv)
      stopifnot(isoreader::iso_is_continuous_flow(obj))
      return(obj)
    })
  )

  # div visibility =====
  observeEvent(get_selected_variable(), {
    shinyjs::hide("div")
    shinyjs::hide("no_data")
    if (!is.null(get_selected_variable()) && !is.na(get_selected_variable())) {
      selected_tab <- get_gui_setting(ns(paste0("tabs-", get_selected_variable())), default = "raw_data")
      module_message(ns, "info", sprintf("DATA loading CF screen for variable '%s' with tab '%s'",
                                         get_selected_variable(), selected_tab))
      updateTabsetPanel(session, "tabs", selected = selected_tab)
      shinyjs::show("div")
    } else if (!is.null(get_selected_variable()) && is.na(get_selected_variable())) {
      shinyjs::show("no_data")
    }
  }, ignoreNULL = FALSE)

  # data GUI visibility ====
  observeEvent(files$get_selected_iso_files(), {
    if (values$data_tabs_visible) {
      module_message(ns, "info", "DATA TABS making data tabs visible")
      shinyjs::show("data_div")
    } else {
      module_message(ns, "info", "DATA TABS hiding data tabs")
      shinyjs::hide("data_div")
    }
  }, ignoreNULL = TRUE)
  observeEvent(files$get_selected_iso_files(), {
    values$data_tabs_visible <- length(files$get_selected_iso_files()) > 0
  }, priority = 50, ignoreNULL = FALSE, ignoreInit = FALSE)

  # data GUI tabs ====
  observeEvent(input$tabs, {
    module_message(ns, "info", "DATA TABs user selected tab ", input$tabs)
    set_gui_setting(ns(paste0("tabs-", get_selected_variable())), input$tabs)
  }, ignoreInit = TRUE)

  # file info ====
  file_info <- callModule(
    file_info_server, "file_info",
    get_variable = get_selected_variable,
    get_iso_files = files$get_selected_iso_files,
    is_visible = reactive({ input$tabs == "file_info" })
  )

  # code preview ====
  code_update <-  reactive({
    function(rmarkdown = TRUE, front_matter = rmarkdown) {
      module_message(ns, "info", "CODE generating updated code for continuous flow data viewer")

      code(
        generate_file_header_code(
          title = stringr::str_c("Viewing ", get_selected_variable()),
          setup = TRUE, caching_on = FALSE,
          rmarkdown = rmarkdown, front_matter = front_matter),
        files$get_code_update()(rmarkdown = rmarkdown),
        file_info$get_code_update()(rmarkdown = rmarkdown),
        #cf_data$get_code_update()(rmarkdown = rmarkdown),
        "" # final new line
      )
    }
  })

  code_preview <- callModule(
    codePreviewServer, "code", code_func_reac = code_update,
    download_file = reactive({ paste("VIEW", get_selected_variable()) })
  )

}


#' Continuous flow files UI
module_cf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("div"),
      # file selector
      module_file_selector_ui(ns("files"), width = 6),
      # code preview
      codePreviewUI(ns("code"), width = 6, height = "300px"),
      # data tabs
      div(id = ns("data_div"),
          fluidRow(
            column(
              width = 12,
              default_box(
                title = "Data", width = 8,
                tabsetPanel(
                  selected = "raw_data",
                  id = ns("tabs"),
                  tabPanel("Raw Data", value = "raw_data"
                           #cfRawDataPlotUI(ns("raw_data"))
                  ),
                  tabPanel("File Info", value = "file_info", file_info_table_ui(ns("file_info"))),
                  tabPanel("Method Info", value = "method_info"
                           #methodInfoTableUI(ns("method_info"))
                  ),
                  tabPanel("Vendor Data Table", value = "vendor_data_table"
                           #vendorDataTableTableUI(ns("vendor_data_table"))
                  )
                )
              ),

              # # TAB SPECIFIC BOXES
              # cfRawDataSelectorUI(ns("raw_data"), width = 4, selector_height = "200px"),
              # cfRawDataSettingsUI(ns("raw_data"), width = 4),
              file_info_selector_ui(ns("file_info"), width = 4),
              # methodInfoSelectorUI(ns("method_info"), width = 4),
              # vendorDataTableSelectorUI(ns("vendor_data_table"), width = 4, selector_height = "400px"),
              # exportSettingsUI(ns("export"), width = 4)
            )
          )
      ) %>% shinyjs::hidden()
    ) %>% shinyjs::hidden(),
    # no data box
    div(
      id = ns("no_data"),
      box(h2("Your workspace does not contain any continuous flow iso file objects."), width = 12)
    ) %>% shinyjs::hidden()
  )
}


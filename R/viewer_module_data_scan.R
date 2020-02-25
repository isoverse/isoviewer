#' Scan flow files Server
#' @param get_selected_variable reactive function returning the variable name
module_data_scan_server <- function(input, output, session, get_selected_variable) {

  # namespace
  ns <- session$ns

  # code preview ====
  code_update <-  reactive({
    function(rmarkdown = TRUE, front_matter = rmarkdown) {
      code(
        file_info$get_code_update()(rmarkdown = rmarkdown)
      )
    }
  })

  # basic data server =====
  base_data <- callModule(
    module_data_server, "base_data",
    get_selected_variable = get_selected_variable,
    variable_check_func = isoreader::iso_is_scan,
    get_code_update = code_update
  )

  # file info ====
  file_info <- callModule(
    file_info_server, "file_info",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "file_info")
  )


}


#' Scan files UI
module_data_scan_ui <- function(id) {
  ns <- NS(id)
  module_data_ui(
    ns("base_data"),
    # TABS =====
    tab_panels = list(
      tabPanel("File Info", value = "file_info", file_info_table_ui(ns("file_info")))
    ),
    # OPTIONS ====
    option_boxes = list(
      file_info_selector_ui(ns("file_info"), width = 4)
    )
  )
}

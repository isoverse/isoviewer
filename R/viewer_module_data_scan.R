#' Scan flow files Server
#' @inheritParams module_data_server
module_data_scan_server <- function(input, output, session, get_selected_variable) {

  # namespace
  ns <- session$ns

  # code preview ====
  code_update <-  reactive({
    function(rmarkdown = TRUE, front_matter = rmarkdown) {
      code(
        file_info$get_code_update()(rmarkdown = rmarkdown),
        resistors$get_code_update()(rmarkdown = rmarkdown)
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
    data_table_file_info_server, "file_info",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "file_info")
  )

  # resistors ====
  resistors <- callModule(
    data_table_resistors_server, "resistors",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "resistors")
  )

}


#' Scan files UI
module_data_scan_ui <- function(id) {
  ns <- NS(id)
  module_data_ui(
    ns("base_data"),
    # TABS =====
    tab_panels = list(
      tabPanel("File Info", value = "file_info", data_table_file_info_ui(ns("file_info"))),
      tabPanel("Resistors", value = "resistors", data_table_standards_ui(ns("resistors")))
    ),
    # OPTIONS ====
    option_boxes = list(
      data_table_file_info_column_selector_ui(ns("file_info"), width = 4),
      data_table_resistors_column_selector_ui(ns("resistors"), width = 4)
    )
  )
}

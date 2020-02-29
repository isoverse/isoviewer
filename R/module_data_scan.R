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
        raw_data$get_code_update()(rmarkdown = rmarkdown),
        resistors$get_code_update()(rmarkdown = rmarkdown),
        plot$get_code_update()(rmarkdown = rmarkdown),
        download$get_code_update()(rmarkdown = rmarkdown)
      )
    }
  })

  # basic data server =====
  base_data <- callModule(
    module_data_server, "base_data",
    get_selected_variable = get_selected_variable,
    data_type = "scan",
    get_code_update = code_update
  )

  # file info ====
  file_info <- callModule(
    data_table_file_info_server, "file_info",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "file_info")
  )

  # raw data ====
  raw_data <- callModule(
    data_table_raw_data_server, "raw_data",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "raw_data")
  )

  # resistors ====
  resistors <- callModule(
    data_table_resistors_server, "resistors",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "resistors")
  )

  # plot ====
  plot <- callModule(
    plot_scan_server, "plot",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "plot")
  )

  # download ====
  download <- callModule(
    data_download_server, "download",
    get_variable = get_selected_variable
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
      tabPanel("Raw Data", value = "raw_data", data_table_raw_data_ui(ns("raw_data"))),
      tabPanel("Resistors", value = "resistors", data_table_standards_ui(ns("resistors"))),
      tabPanel("Plot", value = "plot", plot_scan_ui(ns("plot")))
    ),
    # OPTIONS ====
    option_boxes = list(
      data_table_file_info_column_selector_ui(ns("file_info"), width = 4),
      data_table_raw_data_column_selector_ui(ns("raw_data"), width = 4),
      data_table_resistors_column_selector_ui(ns("resistors"), width = 4),
      plot_scan_data_selector_ui(ns("plot"), width = 4),
      plot_scan_options_ui(ns("plot"), width = 4)
    )
  )
}

#' Dual inlet flow files Server
#' @inheritParams module_data_server
module_data_di_server <- function(input, output, session, get_selected_variable) {

  # namespace
  ns <- session$ns

  # code preview ====
  code_update <-  reactive({
    function(rmarkdown = TRUE, front_matter = rmarkdown) {
      code(
        file_info$get_code_update()(rmarkdown = rmarkdown),
        raw_data$get_code_update()(rmarkdown = rmarkdown),
        standards$get_code_update()(rmarkdown = rmarkdown),
        resistors$get_code_update()(rmarkdown = rmarkdown),
        vendor_data_table$get_code_update()(rmarkdown = rmarkdown),
        plot$get_code_update()(rmarkdown = rmarkdown),
        download$get_code_update()(rmarkdown = rmarkdown)
      )
    }
  })

  # basic data server =====
  base_data <- callModule(
    module_data_server, "base_data",
    get_selected_variable = get_selected_variable,
    data_type = "dual_inlet",
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

  # standards ====
  standards <- callModule(
    data_table_standards_server, "standards",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "standards")
  )

  # resistors ====
  resistors <- callModule(
    data_table_resistors_server, "resistors",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "resistors")
  )

  # vendor data table ====
  vendor_data_table <- callModule(
    data_table_vendor_data_table_server, "vendor_data_table",
    get_variable = get_selected_variable,
    get_iso_files = base_data$get_selected_iso_files,
    is_visible = reactive(base_data$get_tab_selection() == "vendor_data_table")
  )

  # plot ====
  plot <- callModule(
    plot_di_server, "plot",
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


#' Dual files UI
module_data_di_ui <- function(id) {
  ns <- NS(id)
  module_data_ui(
    ns("base_data"),
    # TABS =====
    tab_panels = list(
      tabPanel("File Info", value = "file_info", data_table_file_info_ui(ns("file_info"))),
      tabPanel("Raw Data", value = "raw_data", data_table_raw_data_ui(ns("raw_data"))),
      tabPanel("Standards", value = "standards", data_table_standards_ui(ns("standards"))),
      tabPanel("Resistors", value = "resistors", data_table_standards_ui(ns("resistors"))),
      tabPanel("Vendor Data Table", value = "vendor_data_table", data_table_vendor_data_table_ui(ns("vendor_data_table"))),
      tabPanel("Plot", value = "plot", plot_di_ui(ns("plot")))
    ),
    # OPTIONS ====
    option_boxes = list(
      data_table_file_info_column_selector_ui(ns("file_info"), width = 4),
      data_table_raw_data_column_selector_ui(ns("raw_data"), width = 4),
      data_table_standards_column_selector_ui(ns("standards"), width = 4),
      data_table_resistors_column_selector_ui(ns("resistors"), width = 4),
      data_table_vendor_data_table_column_selector_ui(ns("vendor_data_table"), width = 4),
      plot_di_data_selector_ui(ns("plot"), width = 4),
      plot_di_options_ui(ns("plot"), width = 4)
    )
  )
}


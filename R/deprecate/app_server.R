#' Isoviewer App Server
#'
#' @description Generates the server part of the isoviewer app
#' @inheritParams isofilesLoadServer
app_server <- function(data_dir, allow_data_upload, allow_folder_creation, store_data) {
  shinyServer(function(input, output, session) {

    message("\n\nINFO: Loading GUI instance ...")

    # dual inlet and contuous flow parameters
    params <- c(
      read_raw_data = "Raw Data",
      read_file_info = "File Info",
      read_method_info = "Method Info",
      read_vendor_data_table = "Vendor Data Table"
    )

    # DUAL INLET SERVER LOGIC
    di_load <- callModule(
      isofilesLoadServer, "di_load",
      data_dir = data_dir, allow_data_upload = allow_data_upload,
      allow_folder_creation = allow_folder_creation, store_data = store_data,
      extensions = isoreader:::get_supported_di_files()$extension,
      load_func = "iso_read_dual_inlet", load_params = params,
      post_load = function() { updateTabItems(session, "menu", "di_view") })

    di_view <- callModule(
      dualInletViewServer, "di_view", data_dir = data_dir,
      load_server = di_load)

    # CONTINUOUS FLOW SERVER LOGIC
    cf_load <- callModule(
      isofilesLoadServer, "cf_load",
      data_dir = data_dir, allow_data_upload = allow_data_upload,
      allow_folder_creation = allow_folder_creation, store_data = store_data,
      extensions = isoreader:::get_supported_cf_files()$extension,
      load_func = "iso_read_continuous_flow", load_params = params,
      post_load = function() { updateTabItems(session, "menu", "cf_view") })

    cf_view <- callModule(
      continuousFlowViewServer, "cf_view", data_dir = data_dir,
      load_server = cf_load)

    # NAVIGATION (todo: into separate file?)

    observeEvent(input$di_load, updateTabItems(session, "menu", "di_load"))
    observeEvent(input$di_view, updateTabItems(session, "menu", "di_view"))
    observeEvent(input$cf_load, updateTabItems(session, "menu", "cf_load"))
    observeEvent(input$cf_view, updateTabItems(session, "menu", "cf_view"))

  })

}

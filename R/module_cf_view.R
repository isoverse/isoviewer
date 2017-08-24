#' Continuous Flow View Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @param load_server if a load server is present
#' @family view continuous flow module functions
continuousFlowViewServer <- function(input, output, session, data_dir, load_server = NULL) {

  # namespace
  ns <- session$ns

  # saved datasets ====
  if (!is.null(load_server))
    saved_datasets <- load_server$get_saved_datasets
  else
    saved_datasets <- reactive({ c() })

  # load dataset ====
  cf_dataset <- callModule(
    datasetsServer, "cf_dataset",
    data_dir = data_dir, extensions = isoreader:::get_supported_cf_files()$extension,
    load_func = "read_continuous_flow",
    saved_datasets = saved_datasets)

  # automatically load newly created datasets in the viewer
  if (!is.null(load_server)) {
    observeEvent(load_server$get_saved_datasets(),
                 cf_dataset$load_dataset(load_server$get_loaded_dataset()))
  }

  # data viewer ====
  cf_data <- callModule(
    continuousFlowDataServer, "cf_data",
    isofiles = cf_dataset$get_isofiles, dataset_name = cf_dataset$get_dataset_name
  )

  # code preview ====
  code_update <-  reactive({
    function(rmarkdown = TRUE, front_matter = rmarkdown) {
      module_message(ns, "debug", "generating updated code for continuous flow data viewer")

      code(
        generate_file_header_code(
          title = str_c("Viewing ", cf_dataset$get_dataset_name()),
          setup = TRUE, caching_on = FALSE,
          rmarkdown = rmarkdown, front_matter = front_matter),
        cf_dataset$get_code_update()(rmarkdown = rmarkdown),
        cf_data$get_code_update()(rmarkdown = rmarkdown),
        "" # final new line
      )
    }
  })
  code_preview <- callModule(
    codePreviewServer, "cf_code", code_func_reac = code_update,
    download_file = reactive({ str_c("VIEW ", cf_dataset$get_dataset_name()) }))


  # code jumping
  observe({
    req(cf_data$get_data_tab())

    if (is.null(cf_dataset$get_dataset_path()))
      search_term <- "load dataset"
    else {
      search_term <-
        switch(cf_data$get_data_tab(),
               raw_data = "raw data",
               file_info = "file info",
               method_info = "method info",
               vendor_data_table = "vendor data table",
               export = "export",
               NULL)
    }

    module_message(ns, "debug", "jumping into code at: ", search_term)

    if (!is.null(search_term))
      code_preview$focus_code_preview(search = search_term, center = TRUE, case_sensitive = FALSE)
  })

}


#' Continuous Flow View UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family view continuous flow module functions
continuousFlowViewUI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    datasetsUI(ns("cf_dataset"), width = 6),
    codePreviewUI(ns("cf_code"), width = 6, height = "300px"),
    continuousFlowDataUI(ns("cf_data"), width = 12)
  )
}

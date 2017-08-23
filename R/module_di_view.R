#' Dual Inlet View Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @param load_server if a load server is present
#' @family view dual inlet module functions
dualInletViewServer <- function(input, output, session, data_dir, load_server = NULL) {

  # namespace
  ns <- session$ns

  # saved datasets ====
  if (!is.null(load_server))
    saved_datasets <- load_server$get_saved_datasets
  else
    saved_datasets <- reactive({ c() })

  # load dataset ====
  di_dataset <- callModule(
    datasetsServer, "di_dataset",
    data_dir = data_dir, extensions = isoreader:::get_supported_di_files()$extension,
    load_func = "read_dual_inlet",
    saved_datasets = saved_datasets)

  # automatically load newly created datasets in the viewer
  if (!is.null(load_server)) {
    observeEvent(load_server$get_saved_datasets(),
                 di_dataset$load_dataset(load_server$get_loaded_dataset()))
  }

  # data viewer ====
  di_data <- callModule(
    dualInletDataServer, "di_data",
    isofiles = di_dataset$get_isofiles
  )

  # code preview ====
  code_update <-  reactive({
    function(rmarkdown = TRUE, front_matter = rmarkdown) {
      module_message(ns, "debug", "generating updated code for dual inlet data viewer")

      code(
        generate_file_header_code(
          title = str_c("Viewing ", di_dataset$get_dataset_name()),
          setup = TRUE, caching_on = FALSE,
          rmarkdown = rmarkdown, front_matter = front_matter),
        di_dataset$get_code_update()(rmarkdown = rmarkdown),
        di_data$get_code_update()(rmarkdown = rmarkdown),
        "" # final new line
      )
    }
  })
  code_preview <- callModule(
    codePreviewServer, "di_code", code_func_reac = code_update,
    download_file = reactive({ str_c("LOAD ", di_dataset$get_dataset_name()) }))


  # code jumping
  observe({
    req(di_data$get_data_tab())

    if (is.null(di_dataset$get_dataset_path()))
      search_term <- "load dataset"
    else {
      search_term <-
        switch(di_data$get_data_tab(),
               file_info = "file info",
               NULL)
    }

    module_message(ns, "debug", "jumping into code att: ", search_term)

    if (!is.null(search_term))
      code_preview$focus_code_preview(search = search_term, case_sensitive = FALSE)
  })

  # FIXME/DEBUG
  observe({
    input$testing_it
    isolate(di_dataset$load_dataset("inst/extdata/datasets/test.di.rda"))
  })

}


# Dual Inlet View UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family view dual inlet module functions
dualInletViewUI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    datasetsUI(ns("di_dataset"), width = 6),
    codePreviewUI(ns("di_code"), width = 6, height = "300px"),
    dualInletDataUI(ns("di_data"), width = 12),
    default_box("test", width = 12, checkboxInput(ns("testing_it"), "test", value = TRUE))
  )
}

# observe({
#   req(input$code_line)
#   code_preview$focus_code_preview(line = input$code_line, center = TRUE)
# })
#
# observe({
#   req(input$code_search)
#   code_preview$focus_code_preview(search = input$code_search, case_sensitive = FALSE)
# })

# numericInput(ns("code_line"), "Line", value = 0),
# textInput(ns("code_search"), "Search", value = ""),

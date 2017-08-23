#' Dual Inlet View Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @param load_server if a load server is present
#' @family view dual inlet module functions
dualInletViewServer <- function(input, output, session, data_dir, load_server = NULL) {

  # saved datasets
  if (!is.null(load_server))
    saved_datasets <- load_server$get_saved_datasets
  else
    saved_datasets <- reactive({ c() })

  # loaded dataset
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

  # TESTING (inital load)
  #observeEvent(di_datasets$datasets(),
  #             di_datasets$load_dataset("/Users/sk/Dropbox/Tools/software/R/isoviewer/inst/extdata/datasets/test.di.rda"))

  # data viewer
  di_data <- callModule(
    dualInletDataServer, "di_data",
    isofiles = di_dataset$get_isofiles
  )


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
    dualInletDataUI(ns("di_data"), width = 8)
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

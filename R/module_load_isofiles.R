# Isofiles load module ====

#' Isofiles Load Server
#' @inheritParams app_server
#' @inheritParams fileSelectorServer
#' @family isofiles load module functions
isofilesLoadServer <- function(
  input, output, session, data_dir, pattern = NULL) {

  # namespace
  ns <- session$ns

  # file selector
  files_select <- callModule(
    fileSelectorServer, "files", pattern = pattern,
    root = data_dir, root_name = "Data", multiple = TRUE, start_sort_desc = TRUE,
    enable_recent = TRUE, start_recent = FALSE, start_n_recent = 20)

  # return reactive functions
  list(
    file_browser = files_select
  )
}


#' Isofiles Load UI
#'
#' @param id the module id
#' @param label what to label boxes with
#' @family isofiles load module functions
isofilesLoadUI <- function(id, label = NULL) {
  ns <- NS(id)
  label <- if(!is.null(label)) str_c(label, " ")
  tagList(
    # file/folder selection
    default_box(title = str_c(label, "File and folder selection"), width = 12,
                fileSelectorUI(ns("files"), size = 12),
                footer = div(
                  tooltipInput(actionButton, ns("add_files"), "Add to load list", icon = icon("plus"),
                               tooltip = "Add selected files and folders to the load list"),
                  dataUploadUI(ns("upload"))
                )
    ),

    # load list
    default_box(title = str_c(lable, "Load list"), width = 6,
                selectInput(ns("load_files_list"), label = NULL, multiple = TRUE, size = 8, selectize = FALSE,
                            choices = c(),
                            selected = c())
                # h4(
                #   actionLink("data_files_remove", "Remove", icon = icon("remove")), " | ",
                #   actionLink("data_files_export", "Export Excel", icon = icon("cloud-download")), " |",
                #   bsTooltip("data_files_export", "Export the selected files to excel"),
                #   downloadLink("data_files_download", class = NULL, icon("file-zip-o"), "Download"), " |",
                #   bsTooltip("data_files_download", "Download data files as a zip archieve"),
                #   actionLink("data_files_load", "Load", icon = icon("bar-chart")),
                #   bsTooltip("data_files_load", "Load the selected files together")
                # )
    ),

    # code previes
    default_box(title = "Code preview", width = 6,
                div(class = "pull-right", downloadLink(ns("code_download"), span(icon("download"), " Code"))),

                radioButtons("code_load_source", label = NULL, inline = TRUE,
                             c("load from raw data files (dxf)"="dxf", "load from data export (xlsx)"="xlsx")),
                aceEditor(ns("data_code"), "library(text); \n #blabal \n x <- 1:5", mode = "r",
                          theme="ambiance", readOnly = TRUE,
                          height = "200px")
    )
  )
}




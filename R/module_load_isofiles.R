# Isofiles load module ====

#' Isofiles Load Server
#' @inheritParams fileSelectorServer
#' @param data_dir the directory for local data files
#' @param extensions which extensions to allow
#' @param allow_data_upload whether to allow uploading of data
#' @param store_data whether data files (including .rda exports) are stored permanently (TRUE) or just temporarily (FALSE)
#' @family isofiles load module functions
isofilesLoadServer <- function(
  input, output, session, data_dir, extensions,
  allow_data_upload = FALSE, store_data = TRUE) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    load_list = list()
  )

  # file selector
  root_name <- "Data"
  pattern <- str_c("\\.(", str_c(sprintf("(%s)", extensions), collapse = "|"), ")$")
  files_select <- callModule(
    fileSelectorServer, "files", pattern = pattern,
    root = data_dir, root_name = root_name, multiple = TRUE, start_sort_desc = TRUE,
    enable_recent = TRUE, start_recent = FALSE, start_n_recent = 20)



  # upload yes/no
  if (allow_data_upload) {
    show_folder <- if(store_data) reactive({ file.path(root_name, files_select$path_relative()) }) else NULL
    upload_files <- callModule(dataUploadServer, "upload", folder = files_select$path, show_folder = show_folder)
  }

  # upload UI
  output$upload_wrap <- renderUI({
    store_data_msg <-
      if (store_data) {
        "Upload data to the following folder. These data will be visible to everybody else with access to this server."
      } else {
        "Add data files from your local hard drive to the load list. Uploaded files will only be stored temporarily and are not available to anybody else."
      }
    if (allow_data_upload) {
      dataUploadUI(ns("upload"),
                   dialog_text = store_data_msg,
                   accept = c("application/octet-stream", str_c(".", extensions), "application/zip", ".zip"))
    } else NULL
  })

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
    default_box(title = str_c(label, "File and Folder Selection"), width = 12,
                fileSelectorUI(ns("files"), size = 12),
                footer = div(
                  tooltipInput(actionButton, ns("add_files"), "Add to load list", icon = icon("plus"),
                               tooltip = "Add selected files and folders to the load list"),
                  inline(uiOutput(ns("upload_wrap")))
                )
    ),

    # load list
    default_box(title = str_c(label, "Load List"), width = 6,
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
    default_box(title = "Code Preview", width = 6,
                div(class = "pull-right", downloadLink(ns("code_download"), span(icon("download"), " Code"))),

                radioButtons("code_load_source", label = NULL, inline = TRUE,
                             c("load from raw data files (dxf)"="dxf", "load from data export (xlsx)"="xlsx")),
                aceEditor(ns("data_code"), "library(text); \n #blabal \n x <- 1:5", mode = "r",
                          theme="ambiance", readOnly = TRUE,
                          height = "200px")
    )
  )
}




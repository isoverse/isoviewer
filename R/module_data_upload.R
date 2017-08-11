# Data upload module ====

#' Data Upload Server
#' @param folder should be a reactive function that returns the upload folder
#' @param show_folder reactive function that shows the view folder
#' @param extract_zip whether to extract zip files
#' @param pattern pattern for file types to allow (only these type of files are kept from unpacked .zip and direct file upload)
#' @family data upload module functions
dataUploadServer <- function(input, output, session, folder, show_folder = NULL, extract_zip = TRUE, pattern = NULL) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    upload_counter = 0L,
    upload_batch = 0L,
    last_upload_path = c(), # absolute upload paths
    last_upload_path_relative = c() # paths relative to the folder()
  )

  # update the shown folder
  if (!is.null(show_folder) && is.function(show_folder)) {
    output$folder_name <- renderText(show_folder())
  }

  # uploads
  observe({
    # upload (expand zip)
    req(input$upload_file)
    isolate({
      target <- folder()
      values$last_upload_path <- c()
      values$last_upload_path_relative <- c()
      # upload all selected files
      mapply(
        input$upload_file$name, input$upload_file$datapath,
        FUN = function(name, datapath) {
          # info and uplaod counter
          values$upload_counter <- values$upload_counter + 1L
          module_message(ns, "info", sprintf("Processing upload #%d: %s", values$upload_counter, name))
          ext <- str_match(basename(name), "\\.(\\w+$)")[1,2]
          if (!is.na(ext) && ext == "zip") {
            if (extract_zip) {
              unzip_files <- unzip(datapath, list = TRUE)$Name
              if (!is.null(pattern)) unzip_files <- str_subset(unzip_files, pattern)
              module_message(ns, "debug", sprintf("unpacking .zip with %d suitable files", length(unzip_files)))
              if (length(unzip_files) > 0) {
                unzip(datapath, exdir = target, files = unzip_files)
                values$last_upload_path <- c(values$last_upload_path, file.path(target, unzip_files))
                values$last_upload_path_relative <- c(values$last_upload_path_relative, unzip_files)
              }
            } else
              module_message(ns, "debug", "uploading zip files is not allow")
          } else {
            if (grepl(pattern, name)) {
              module_message(ns, "debug", "keeping suitable file")
              file.copy(datapath, to = file.path(target, name))
              values$last_upload_path <- c(values$last_upload_path, file.path(target, name))
              values$last_upload_path_relative <- c(values$last_upload_path_relative, name)
            } else {
              module_message(ns, "debug", "not keeping unsuitable file")
            }
          }
      })
      # update batch information
      values$upload_batch <- values$upload_batch + 1L
    })
  })

  # return reative values with the upload
  list(
    upload_batch = reactive({ values$upload_batch }),
    last_upload_path = reactive({ values$last_upload_path }),
    last_upload_path_relative = reactive({ values$last_upload_path_relative })
  )
}


#' Data Upload UI
#' @param id data upload id
#' @param open_label the label for the link to open the modal dialog
#' @param dialog_text what should the dialog say
#' @param accept which mime types to allow (e.g. zip is c('application/zip', '.zip'))
#' @family data upload module functions
dataUploadUI <- function(id, open_label = "Upload data",
                         open_tooltip = "Upload local data files to the server",
                         dialog_text = NULL,
                         accept = c("application/zip", ".zip")) {

  ns <- NS(id)

  # modal dlg
  modal_dlg <- bsModal(
    ns("upload_dialog"),
    title = open_label,
    trigger = ns("upload_open"),
    size = "small",
    dialog_text,
    h4(textOutput(ns("folder_name"))),
    fileInput(ns("upload_file"),
              buttonLabel = "Select...",
              label = NULL,
              multiple = TRUE,
              accept = accept)
  )

  # full tag list
  tagList(
    tooltipInput(actionButton, ns("upload_open"), open_label, icon = icon("upload"), tooltip = open_tooltip),
    modal_dlg
  )
}


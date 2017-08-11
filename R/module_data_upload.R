# Data upload module ====

#' Data Upload Server
#' @param folder should be a reactive function that returns the upload folder
#' @param show_folder reactive function that shows the view folder
#' @family data upload module functions
dataUploadServer <- function(input, output, session, folder, show_folder = NULL) {

  # reactive values
  values <- reactiveValues(
    upload_counter = 0
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
      upload <- input$upload_file
      ext <- stringr::str_match(basename(upload$name), "\\.(\\w+$)")[1,2]
      target <- folder()
      if (!is.na(ext) && ext == "zip") upload$datapath %>% unzip(exdir = target)
      else upload$datapath %>% file.copy(to = file.path(target, upload$name))

      # info and update trigger
      values$upload_counter <- values$upload_counter + 1
      module_message(session$ns, "info",
                     sprintf("Upload #%d complete: %s", values$upload_counter,
                             file.path(target, upload$name)))
    })
  })

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


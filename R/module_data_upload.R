

dataUploadUI <- function() {

  #if (allow_upload) fileInput(ns("upload"), upload_label)

}

dataUploadServer <- function() {

  # reactive values
  values <- reactiveValues(
    upload_counter = 1
  )

  # uploads
  observe({
    # upload (expand zip)
    upload <- input$upload
    if (is.null(upload)) return()
    ext <- stringr::str_match(basename(upload$name), "\\.(\\w+$)")[1,2]
    target <- isolate(values$current_directory)
    if (!is.na(ext) && ext == "zip") upload$datapath %>% unzip(exdir = target)
    else upload$datapath %>% file.copy(to = file.path(target, upload$name))

    # info and update trigger
    counter <- isolate(values$upload_counter)
    sprintf("Upload #%d complete: %s", counter + 1, file.path(target, upload$name)) %>%
      message()
    values$upload_counter <- counter + 1
  })

}

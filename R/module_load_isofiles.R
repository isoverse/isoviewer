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
    load_list = data_frame(path = character(), path_rel = character(), label = character()),
    load_list_selected = c()
  )

  # file selector module
  root_name <- "Data"
  pattern <- str_c("\\.(", str_c(sprintf("(%s)", extensions), collapse = "|"), ")$")
  files_select <- callModule(
    fileSelectorServer, "files", pattern = pattern,
    root = data_dir, root_name = root_name, multiple = TRUE, start_sort_desc = TRUE,
    enable_recent = TRUE, start_recent = FALSE, start_n_recent = 20)

  # upload module
  temp_dir <- tempdir()
  upload_folder <- if (store_data) files_select$path else reactive({ tem_dir })
  if (allow_data_upload) {
    show_folder <- if(store_data) reactive({ file.path(root_name, files_select$path_relative()) }) else NULL
    upload_files <- callModule(dataUploadServer, "upload", folder = upload_folder, show_folder = show_folder,
                               pattern = pattern)
  } else {
    upload_files <- list(last_upload = reactive({ c() }))
  }

  # addition to load list
  add_to_load_list <- function(path, path_relative) {
    already_listed <- path %in% values$load_list$path
    if (length(path[!already_listed]) > 0) {
      module_message(ns, "debug", "adding files to load list: ", str_c(path_relative[!already_listed], collapse = ", "))
      new <- data_frame(
        path = path[!already_listed],
        path_rel = path_relative[!already_listed])
      values$load_list <- bind_rows(values$load_list, new)
    }
    # update directory information
    values$load_list <- mutate(
      values$load_list,
      isdir = dir.exists(path),
      n_files = ifelse(
        isdir,
        sapply(path, function(d) {
          length(list.files(d, pattern = pattern, recursive = TRUE, include.dirs = FALSE))
        }), 1L),
      label = ifelse(isdir, sprintf("[%s] (%d files)", path_rel, n_files), path_rel)
    )
  }
  observe({
    req(input$add_files)
    isolate({ add_to_load_list(path = files_select$selection(), path_relative = files_select$selection_relative()) })
  })
  observe({
    req(upload_files$upload_batch())
    isolate({
      uploaded_files_rel <- upload_files$last_upload_path_relative()
      if (length(uploaded_files_rel) > 0) {
        uploaded_folder <- if (store_data) files_select$path_relative() else "temp"
        if(nchar(uploaded_folder) > 0) uploaded_files_rel <- file.path(uploaded_folder, uploaded_files_rel)
        add_to_load_list(path = upload_files$last_upload_path(), path_relative = uploaded_files_rel)
      }
    })
  })

  # remove from load list
  remove_from_load_list <- function(remove_path) {
    values$load_list <- filter(values$load_list, !path %in% remove_path)
  }
  observe({
    req(input$remove_files)
    isolate({ remove_from_load_list(input$load_files_list) })
  })

  # update load list
  observe({
    values$load_list # trigger whenever there is a change to the load list values
    isolate({
      options <- select(values$load_list, label, path) %>% arrange(label) %>% deframe()
      values$load_list_selected <- options[options %in% values$load_list_selected]
      updateSelectInput(session, "load_files_list", choices = options, selected = values$load_list_selected)
    })
  })

  # keep track of load list selection (to keep it the same after reload)
  observe({
    req(input$load_files_list)
    isolate({ values$load_list_selected <- input$load_files_list })
  })

  # upload UI
  output$upload_wrap <- renderUI({
    store_data_msg <-
      if (store_data) {
        "Upload data to the following folder. These data will be visible to everybody else with access to this server."
      } else {
        "Add data files from your local hard drive to the load list. Uploaded files will only be stored temporarily and are not available to anybody else."
      }
    store_data_msg <- str_c(store_data_msg,
                            " Uploaded zip archives (.zip) are automatically extracted.",
                            " Only appropriate file types (", str_c(extensions, collapse = ", "), ") are used. All files are added to the load list.")
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
                            selected = c()),
                tooltipInput(actionButton, ns("remove_files"), "Remove", icon = icon("remove"),
                             tooltip = "Remove selected files and folders from the load list")
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




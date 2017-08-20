# Isofiles load module ====

#' Isofiles Load Server
#' @inheritParams fileSelectorServer
#' @param data_dir the directory for local data files
#' @param extensions which extensions to allow
#' @param load_func the loading function (as character!)
#' @param load_params the loading checkboxes (parameter names and labels)
#' @param allow_data_upload whether to allow uploading of data
#' @param store_data whether data files (including .rda exports) are stored permanently (TRUE) or just temporarily (FALSE)
#' @param collections_dir the director for collection files
#' @family isofiles load module functions
isofilesLoadServer <- function(
  input, output, session, data_dir, extensions,
  load_func, load_params = c(),
  allow_data_upload = FALSE, store_data = TRUE,
  collections_dir = file.path(data_dir, "collections")) {

  # namespace
  ns <- session$ns

  # make sure collections dir exists
  if (!dir.exists(collections_dir)) dir.create(collections_dir)

  # file patterns
  rda_pattern <- str_c("\\.(", str_c(sprintf("(%s)", str_subset(extensions, "rda")), collapse = "|"), ")$")
  pattern <- str_c("\\.(", str_c(sprintf("(%s)", extensions), collapse = "|"), ")$")

  # reactive values
  values <- reactiveValues(
    load_list = data_frame(path = character(), path_rel = character(), label = character()),
    load_list_selected = c(),
    loaded_isofiles = NULL,
    collections =
      list.files(collections_dir, pattern = rda_pattern) %>%
      { setNames(rep(list(NA), length(.)), .) }
  )

  # update collections
  observe({
    updateSelectInput(session, "collections", choices = names(values$collections))
  })

  # file selector module
  root_name <- "Data"
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

  # Managed load list =====

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
  observe({ # enable/disable add to load list button depending on selectoin
    if (length(files_select$selection()) > 0) enable("add_files")
    else disable("add_files")
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
  observe({ # enable/disable remove files button depending on selection
    if (length(input$load_files_list) > 0) enable("remove_files")
    else disable("remove_files")
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

  # Load list ====

  observe({
    req(input$load_files)
    isolate({
      collection <- file.path(collections_dir, input$collection_name)
      print(collection)
      module_message(ns, "info", "loading file list and storing in collection file ", collection)

      # retrieve paths
      files <- isoreader:::retrieve_file_paths(paths = values$load_list$path, extensions = extensions)
      n_files <- length(files)
      args <- c(
        as.list(setNames(names(load_params) %in% input$selected_params,
                         names(load_params))),
        list(cache = FALSE)) # never cache?

      # read files
      withProgress(message = 'Loading file list:', value = 0, {
        isofiles <- lapply(files, function(file) {
          incProgress(1/n_files, detail = sprintf("reading '%s'...", basename(file)))
          isofile <- do.call(load_func, args = c(list(paths=file), args))
          if(isoreader:::n_problems(isofile) > 0) {
            setProgress(detail = sprintf("encountered PROBLEMS with '%s'...", basename(file)))
            Sys.sleep(1)
            # sk note: if at all possible, maybe change color if issues with file
          }
          return(isofile)
        })

        # finalize and save
        values$loaded_isofiles <- as_isofile_list(isofiles)
        setProgress(value = 1, detail = sprintf("finalizing and saving to %s",
                                                file.path(basename(collections_dir), input$collection_name)))
        export_to_rda(values$loaded_isofiles, filename = input$collection_name, folder = collections_dir)
        Sys.sleep(1)
      })
    })
  })

  # enable load files and remove button only if at least one file is selected
  observe({
    if (nrow(values$load_list) > 0) enable("load_files")
    else disable("load_files")
  })

  # Code update ====

  code_update <- reactive({
    # trigger code update for any of the below variables changing
    req(values$load_list)
    req(input$selected_params)
    req(input$collection_name)

    function(rmarkdown = TRUE, header = rmarkdown) {
      module_message(ns, "debug", "generating updated code")
      generate_load_list_code(
        read_paths = values$load_list$path_rel,
        read_func = load_func,
        read_params = setNames(names(load_params) %in% input$selected_params, names(load_params)),
        save_file = input$collection_name,
        save_folder = basename(collections_dir),
        rmarkdown = rmarkdown, header = header
      )
    }
  })
  code_preview <- callModule(
    codePreviewServer, "code_preview", code_func_reac = code_update,
    download_file = reactive({ str_c("LOAD ", input$collection_name) }))

  # UI rendering =====

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

  # loading parameters UI

  output$parameters <- renderUI({
    tagList(
      checkboxGroupInput(ns("selected_params"), NULL,
                         choices = setNames(names(load_params), load_params),
                         selected = names(load_params))
    )
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
                inlineInput(textInput, ns("collection_name"), label = "Name:",
                            value = format(Sys.time(), format = "%Y-%m-%d data collection"), width = "250px"),
                selectInput(ns("load_files_list"), label = NULL, multiple = TRUE, size = 8, selectize = FALSE,
                            choices = c(),
                            selected = c()),
                footer = div(
                  tooltipInput(actionButton, ns("remove_files"), "Remove", icon = icon("remove"),
                               tooltip = "Remove selected files and folders from the load list"),
                  tooltipInput(actionButton, ns("load_files"), "Load list", icon = icon("cog"),
                               tooltip = "Load the files and folders in the load list and store as named collection"),
                  br(),
                  h4("Read Parameters:", id = ns("read_params_header")),
                  bsTooltip(ns("read_params_header"), "Which information to read from the data files."),
                  uiOutput(ns("parameters"))
                ),
                selectInput(ns("collections"), "Collections", choices = c())
    ),

    # code preview
    codePreviewUI(ns("code_preview"), width = 6, height = "400px")
  )
}




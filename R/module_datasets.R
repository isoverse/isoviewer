# Module for the selection of a dataset =====
# TODO: show withProgress for loading data file (and/or modal dialog)

#' Datasets server
#' @inheritParams isofilesLoadServer
#' @param saved_datasets reactive function returning a vector of saved datasets (with names as the relative path)
#' @family datasets module functions
datasetsServer <- function(input, output, session, data_dir, extensions, load_func, saved_datasets) {

  # namespace, and top level params
  ns <- session$ns
  datasets_dir <- get_datasets_path(data_dir)
  rda_pattern <- str_c("\\.(", str_c(sprintf("(%s)", str_subset(extensions, "rda")), collapse = "|"), ")$")

  # reactive values
  values <- reactiveValues(
    datasets = c(), # the datasets
    datasets_hash = NULL, # to check for newly added datasets
    loaded_dataset = NULL, # currently loaded dataset (to make sure datasets are reloaded when file changes)
    loaded_dataset_hash = NULL, # loaded dataset hash
    loaded_isofiles = NULL, # loaded isofiles
    omit_problematic = c(), # which files to omit
    omit_isofiles = NULL, # used isofiles (after omit)
    selected_isofiles = c(), # which files to select
    update_selected = 0 # trigger selection update (circumventing circular triggers with user selection)
  )

  # available datasets ====
  observe({

    # check for updates if content changes or saved datasets changes
    invalidateLater(1000, session)
    datasets <- c(
        saved_datasets(),
        list.files(datasets_dir, pattern = rda_pattern) %>%
        { setNames(file.path(datasets_dir, .), file.path(names(datasets_dir), .)) }) %>%
      { .[!duplicated(.)] }

    # everything else is isolated
    isolate({

      # datasets
      if (length(datasets) == 0) {
        datasets_df <- data_frame()
      } else {
        datasets_df <-
          data_frame(
            filepath = as.character(datasets),
            filepath_rel = names(datasets),
            mtime = file.mtime(filepath)
          ) %>%
          arrange(mtime, filepath)
      }

      # hash
      datasets_hash <- generate_content_hash(datasets_df)

      # check if anything changed
      if (is.null(values$datasets_hash) || values$datasets_hash != datasets_hash ) {
        module_message(ns, "debug", "(re)loading datasets list ")
        values$datasets_hash <- datasets_hash
        values$datasets <- datasets_df
      }
    })
  })

  # update datasets ====
  observe({
    req(values$datasets)
    if (nrow(values$datasets) == 0) {
      # no datasets
      updateSelectInput(session, "datasets", choices = c("No datasets available yet" = ""))
    } else {
      # format datasets for display in groups
      datasets <-
        values$datasets %>%
        mutate(
          sorting = format(mtime, "%Y%m"),
          grouping = format(mtime, "%b %Y"),
          label = sprintf("%s (%s)", basename(filepath_rel), dirname(filepath_rel))
        ) %>%
        arrange(desc(sorting), label) %>% # sort by names
        group_by(grouping) %>%
        do({
          data_frame(items = list(setNames(.$filepath, .$label)))
        }) %>%
        { setNames(as.list(.$items), .$grouping) }
      updateSelectInput(session, "datasets", choices = c("Choose a dataset" = "", datasets), selected = values$loaded_dataset)
    }
  })

  # load dataset ===
  observeEvent(input$datasets, {
    req(input$datasets)
    if (is.null(values$loaded_dataset) || input$datasets != values$loaded_dataset)
      load_dataset(input$datasets)
  })

  load_dataset <- function(dataset) {
    if (is.null(dataset)) return()
    # safety check
    if(!file.exists(dataset))
      module_message(ns, "debug", "ERROR: dataset file does not exist: ", dataset)

    # check if load is necessary
    loaded_dataset_hash <- generate_content_hash(list(file = dataset, mtime = file.mtime(dataset)))
    if (is.null(values$loaded_dataset_hash) || values$loaded_dataset_hash != loaded_dataset_hash ) {
      module_message(ns, "info", "(re)loading dataset '", basename(dataset), "'")
      values$loaded_dataset <- dataset
      values$loaded_dataset_hash <- loaded_dataset_hash
      if (dataset != input$datasets)
        updateSelectInput(session, "datasets", selected = dataset)
      values$loaded_isofiles <- as_isofile_list(do.call(load_func, args = list(paths = dataset, quiet = TRUE)))
      omit_problematic()
    }
  }

  # problems ===
  omit_problematic <- function() {
    if (length(values$omit_problematic) > 0 && !is.null(values$loaded_isofiles)) {
      values$omit_isofiles <- omit_files_with_problems(values$loaded_isofiles, type = values$omit_problematic, quiet = TRUE)
      if (length(values$omit_isofiles) == 0)
        values$omit_isofiles <- NULL
    } else {
      values$omit_isofiles <- values$loaded_isofiles
    }
  }

  # changing omit
  observe({
    new_omit <- input$omit
    isolate({
      if ( all(c("warning", "error") %in% input$omit) )
        values$omit_problematic <- "both"
      else
        values$omit_problematic <- input$omit
      omit_problematic()
    })
  })

  # problems button
  dataset_problems <- callModule(
    problemsServer, "dataset_problems",
    dataset = reactive(values$loaded_isofiles), dataset_path = reactive(values$loaded_dataset)
  )

  # button visibility
  observeEvent(values$loaded_isofiles, toggle("dataset_actions", condition = !is.null(values$loaded_isofiles)))


  # isofiles list ====
  isofiles_selector <- callModule(
    selectorTableServer, "isofiles_selector",
    id_column = "file_id", col_headers = c("File", "Errors", "Warnings"),
    hot_mods = function(hot) hot_col(hot, col = c("Errors", "Warnings"), halign = "htCenter"))

  observe({
    if (length(values$omit_isofiles) == 0) {
      isofiles_selector$set_table(NULL)
    } else {
      selector_table <-
        problems_summary(values$omit_isofiles) %>%
        mutate(
          warning = as.character(warning),
          error = as.character(error)
        ) %>%
        select(file_id, error, warning)
      # set table
      isofiles_selector$set_table(selector_table)
    }
  })

  get_selected_isofiles <- reactive({
    if (length(isofiles_selector$get_selected()) > 0)
      values$loaded_isofiles[isofiles_selector$get_selected()]
    else NULL
  })

  # dataset download ====
  output$dataset_download <- downloadHandler(
    filename = function() { basename(values$loaded_dataset) },
    content = function(filename) {
      req(values$loaded_dataset)
      module_message(ns, "info", "downloading dataset ", basename(values$loaded_dataset))
      file.copy(from = values$loaded_dataset, to = filename)
    }
  )

  output$export_excel <- downloadHandler(
    filename = "mytest.xlsx",
    content = function(filename) {
      cat("test", file = filename)
    }
  )


  # code update ====
  code_update <- reactive({
    # trigger code update for any of the below variables changing
    function(rmarkdown = TRUE) {
      generate_data_selection_code(
        dataset = values$loaded_dataset %>% { if(is.null(.)) NULL else basename(.) },
        read_func = load_func,
        omit_type = values$omit_problematic,
        select_files = # omit file selection if ALL files are selected
          if (!is.null(values$omit_isofiles) && all(names(values$omit_isofiles) %in% isofiles_selector$get_selected()))
            NA_character_
          else isofiles_selector$get_selected(),
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions ====
  list(
    load_dataset = load_dataset, # loading function
    get_dataset_path = reactive({ values$loaded_dataset }),
    get_dataset_name = reactive({
      if (is.null(values$loaded_dataset)) return(NULL)
      else basename(values$loaded_dataset) %>% str_replace("\\.(\\w+)\\.rda$", "")
    }),
    get_isofiles = get_selected_isofiles,
    get_code_update = code_update
  )
}


#' Dataset UI
#' @param id the module id
#' @param width box width
#' @param file_list_height height of the file list
#' @family datasets module functions
datasetsUI <- function(id, width = 12, file_list_height = "200px") {
  ns <- NS(id)
  tagList(
    default_box(
      title = "Select Dataset", width = width,
      selectInput(ns("datasets"), label = NULL, choices = c("Loading..." = "")),
      selectorTableUI(ns("isofiles_selector"), height = "200px"),
      footer =
        div(style = "height: 50px;",
            div(id = ns("dataset_actions"),
                tooltipOutput(downloadButton, ns("dataset_download"), "Download",
                              tooltip = "Download entire dataset"),
                spaces(1),
                problemsButton(ns("dataset_problems"), tooltip = "Show problems reported for this dataset."),
                spaces(1),
                selectorTableButtons(ns("isofiles_selector")),
                checkboxGroupInput(ns("omit"), label = NULL, inline = TRUE,
                                   choices = c("Omit files with errors" = "error",
                                               "Omit files with warnings" = "warning"))
            ) %>% hidden()
        )
    )
  )
}


#' Problems Server
#' Stand-alone for showing a dataset's problems.
#' @inheritParams fileSelectorServer
#' @param dataset reactive function returning the isofiles list
#' @param dataset_path reactive function returning the dataset's path
#' @family datasets module functions
problemsServer <- function(input, output, session, dataset, dataset_path) {

  # namespace and constants
  ns <- session$ns
  mail_address <- "sebastian.kopf@colorado.edu"
  mail_subject <- "Problematic Isofile"
  mail_body <- "I have encountered problems reading the attached IRMS data file(s)."

  # the modal dialog
  problem_modal <- reactive({
    req(dataset_path())
    module_message(ns, "debug", "showing problems modal dialog")
    name <- basename(dataset_path()) %>% str_replace("\\.(\\w+)\\.rda$", "")
    modalDialog(
      title = "Problems",
      sprintf("The following problems were encountered during the loading of dataset '%s'.", name),
      "If any problems are unexpected (i.e. the files should have valid data), please ",
      strong(a(href = sprintf("mailto:%s?subject=%s&body=%s",
                              mail_address, str_replace_all(mail_subject, " ", "%20"), str_replace_all(mail_body, " ", "%20")),
               "send us an email")),
      " and attach at least one of the problematic file(s). Your help is much appreciated.",
      tableOutput(ns('problems')),
      footer = modalButton("Close"), fade = FALSE, easyClose = TRUE, size = "l"
    )
  })

  # problems table
  output$problems <- renderTable({
    req(dataset())
    probs <- problems(dataset())
    if (nrow(probs) == 0) data_frame(Problem = "no problems")
    else select(probs, File = file_id, Type = type, Function = func, Problem = details)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = 'l')

  # functions
  show_problems <- function() {
    showModal(problem_modal())
  }

  # button trigger
  observeEvent(input$show_problems, {
    if (is.null(dataset_path()) || is.null(dataset()))
      alert("No dataset loaded yet.")
    else
      showModal(problem_modal())
  })

  # button label
  observeEvent(problems(dataset()), {
    req(dataset())
    updateActionButton(session, "show_problems",
                       label = sprintf("Problems (%.0f)", nrow(problems(dataset()))))
  })

  # return functions (note: toggling the button visibility somehow does not work)
  list(
    show_problems = show_problems
  )
}


#' Dataset problems button
#' @inheritParams fileSelectorUI
#' @param start_disabled whether to start out with the button disabled
#' @param tooltip what tooltip to display
#' @family datasets module functions
problemsButton <- function(id, tooltip = "Show problems.") {
  ns <- NS(id)
  tooltipInput(actionButton, ns("show_problems"), "Problems", icon = icon("ambulance"), tooltip = tooltip)
}


# helper functions ====

# get the datasets path (and make sure it exists)
# the return path is a named vector with the relative path as the name
# this is fixed at the moment but could conceivably become flexibel in the future
get_datasets_path <- function(data_dir) {
  path <- file.path(data_dir, "datasets")
  if (!dir.exists(path)) dir.create(path)
  return(setNames(path, "datasets"))
}


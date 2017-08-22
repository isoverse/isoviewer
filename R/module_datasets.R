# Module for the selection of a dataset =====

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
    loaded_isofiles = NULL # loaded isofiles
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
      values$loaded_isofiles <- do.call(load_func, args = list(paths = dataset, quiet = TRUE))
    }
  }

  # return functions
  list(
    load_dataset = load_dataset
  )
}


#' Dataset UI
#' @param id the module id
#' @family datasets module functions
datasetsUI <- function(id) {
  ns <- NS(id)
  default_box(title = "Select Dataset", width = 12,
    selectInput(ns("datasets"), label = NULL, choices = c("Loading..." = ""))
    #%>% withSpinner(type = 7, proxy.height = "20px") # is not well hidden behind dataset
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

  # the modal
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

  # return functions
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


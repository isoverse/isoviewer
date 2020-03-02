#' File selector server
#' @param get_variable reactive function returning the name of the variable
#' @param get_iso_files reactive function returning the actual iso files
module_file_selector_server <- function(input, output, session, settings, get_variable, get_iso_files) {

  # namespace
  ns <- session$ns

  # reactive values =====
  values <- reactiveValues(
    show_errors = settings$get(ns("errors"), default = TRUE),
    show_warnings = settings$get(ns("warnings"), default = TRUE)
  )

  # get error/warning filtered isofiles ====
  get_filtered_iso_files <- reactive({
    req(get_iso_files())
    if (values$show_errors && values$show_warnings)
      return(get_iso_files())
    else if (!values$show_errors && values$show_warnings)
      return(isoreader::iso_filter_files_with_problems(get_iso_files(), remove_files_with_errors = TRUE, remove_files_with_warnings = FALSE, quiet = TRUE))
    else if (values$show_errors && !values$show_warnings)
      return(isoreader::iso_filter_files_with_problems(get_iso_files(), remove_files_with_errors = FALSE, remove_files_with_warnings = TRUE, quiet = TRUE))
    else
      return(isoreader::iso_filter_files_with_problems(get_iso_files(), remove_files_with_errors = TRUE, remove_files_with_warnings = TRUE, quiet = TRUE))
  })

  # data set name output ====
  output$dataset <- renderText(paste0("Dataset: ", get_variable()))

  # isofiles selector server ====
  selector <- callModule(
    selector_table_server, "selector",
    settings = settings,
    id_column = "file_id", row_column = "row_id",
    column_select = c(File = file_id, `File Size` = file_size, Errors = error, Warning = warning)
  )

  # get selected isofiles =====
  get_selected_iso_files <- eventReactive(selector$get_selected(), {
    req(get_filtered_iso_files())

    # info message
    module_message(
      ns, "info", sprintf(
        "FILES user selected %d/%d files from '%s'",
        length(selector$get_selected()), length(get_filtered_iso_files()), get_variable())
    )

    # store selected in settings
    settings$set(ns(paste0("selector-", get_variable())), selector$get_selected())

    # return selected iso_files
    iso_files <- get_filtered_iso_files()

    # quick filter based on id is much faster than isoreader::iso_filter_files
    iso_files[names(iso_files) %in% selector$get_selected()]
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # generate isofiles table ====
  observeEvent(get_filtered_iso_files(), {
    isolate({
      # what is selected?
      selected <- settings$get(ns(paste0("selector-", get_variable())), default = c())

      # info message
      module_message(
        ns, "info", sprintf(
          "FILES creating iso files selection table for '%s' with %d/%d selected",
          get_variable(), length(selected), length(get_filtered_iso_files()))
      )

      # generate selector table
      df <- get_filtered_iso_files() %>%
        isoreader::iso_get_problems_summary(
          problem_files_only = FALSE, include_file_info = file_size
        ) %>%
        dplyr::mutate(
          row_id = dplyr::row_number(),
          file_size = ifelse(!is.na(file_size), sprintf("%.1f kB", file_size/1024), "NA"),
          warning = as.character(warning),
          error = as.character(error)
        )
      selector$set_table(df)
      selector$set_selected(selected)
    })
  })

  # dataset download ====
  output$download <- downloadHandler(
    filename = function() { basename(isoreader:::get_rds_export_filepath(get_iso_files(), get_variable())) },
    content = function(filename) {
      req(get_iso_files())
      module_message(ns, "info", "FILES downloading entire dataset ", get_variable())
      file_path <- file.path(tempdir(), isoreader:::get_rds_export_filepath(get_iso_files(), get_variable()))
      withProgress({
          isoreader::iso_save(get_iso_files(), file_path, quiet = TRUE)
          file.copy(from = file_path, to = filename)
        },
        message = "Preparing dataset...",
        detail = sprintf("Generating export file '%s'", basename(file_path)),
        value = 0.5
      )
    }
  )

  # errors =======
  observe({
    toggle("errors_hide", condition = values$show_errors)
    toggle("errors_show", condition = !values$show_errors)
    settings$set(ns("errors"), values$show_errors)
  })
  observeEvent(input$errors_hide, values$show_errors <- FALSE)
  observeEvent(input$errors_show, values$show_errors <- TRUE)

  # warnings =======
  observe({
    toggle("warnings_hide", condition = values$show_warnings)
    toggle("warnings_show", condition = !values$show_warnings)
    settings$set(ns("warnings"), values$show_warnings)
  })
  observeEvent(input$warnings_hide, values$show_warnings <- FALSE)
  observeEvent(input$warnings_show, values$show_warnings <- TRUE)

  # problems ====
  callModule(problems_server, "problems", settings = settings, get_variable = get_variable, get_iso_files = get_filtered_iso_files)

  # code update ====
  code_update <- reactive({
    # trigger code update for any of the below variables changing
    function(rmarkdown = TRUE) {
      generate_data_subset_code(
        dataset = get_variable(),
        remove_errors = !values$show_errors,
        remove_warnings = !values$show_warnings,
        select_files =
          if(selector$are_all_selected()) NA_character_
          else selector$get_selected(),
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions ====
  list(
    get_selected_iso_files = get_selected_iso_files,
    get_code_update = code_update
  )

}


#' File selector UI
#' @param id the module id
#' @param width box width
#' @param file_list_height height of the file list
#' @family datasets module functions
module_file_selector_ui <- function(id, width = 12, file_list_height = "200px") {
  ns <- NS(id)
  tagList(
    default_box(
      title = textOutput(ns("dataset")), width = width,
      selector_table_ui(ns("selector")),
      footer =
        div(id = ns("actions"),
            tooltipOutput(downloadButton, ns("download"), "Download",
                          tooltip = "Download entire dataset"),
            spaces(1),
            problems_button(ns("problems"), tooltip = "Show problems reported for this dataset."),
            spaces(1),
            tooltipInput(actionButton, ns("errors_hide"), label = "Errors", icon = icon("eye-slash"),
                          tooltip = "Click to filter out files with errors") %>% shinyjs::hidden(),
            tooltipInput(actionButton, ns("errors_show"), label = "Errors", icon = icon("eye"),
                          tooltip = "Click to show files with errors") %>% shinyjs::hidden(),
            spaces(1),
            tooltipInput(actionButton, ns("warnings_hide"), label = "Warnings", icon = icon("eye-slash"),
                         tooltip = "Click to filter out files with errors") %>% shinyjs::hidden(),
            tooltipInput(actionButton, ns("warnings_show"), label = "Warnings", icon = icon("eye"),
                         tooltip = "Click to show files with errors") %>% shinyjs::hidden(),
            spaces(1),
            selector_table_buttons_ui(ns("selector"))
        )
    )
  )
}


#' Problems Server
#' Stand-alone for showing a dataset's problems.
#' @inheritParams module_file_selector_server
#' @family datasets module functions
problems_server <- function(input, output, session, settings, get_variable, get_iso_files) {

  # namespace and constants
  ns <- session$ns
  mail_address <- "sebastian.kopf@colorado.edu"
  mail_subject <- "Problematic Isofile"
  mail_body <- "I have encountered problems reading the attached IRMS data file(s)."

  # the modal dialog
  problem_modal <- reactive({
    req(get_iso_files())
    module_message(ns, "info", sprintf("FILES showing problems modal dialog for '%s'", get_variable()))
    modalDialog(
      title = h3(sprintf("Problems in '%s'", get_variable())),
      p("The following read problems were reported in this dataset. If any problems are unexpected (i.e. the files should have valid data), please ",
      strong(a(href = sprintf("mailto:%s?subject=%s&body=%s",
                              mail_address, stringr::str_replace_all(mail_subject, " ", "%20"), stringr::str_replace_all(mail_body, " ", "%20")),
               "send us an email")),
      " and attach at least one of the problematic file(s). Your help is much appreciated."),
      tableOutput(ns('problems')),
      footer = modalButton("Close"), fade = FALSE, easyClose = TRUE, size = "l"
    )
  })

  # problems table
  output$problems <- renderTable({
    req(get_iso_files())
    probs <- isoreader::iso_get_problems(get_iso_files())
    if (nrow(probs) == 0) {
      tibble::tibble(Problem = "no problems")
    } else {
      dplyr::select(probs, File = file_id, Type = type, Function = func, Problem = details) %>%
        dplyr::mutate(Function = wrap_function_name(Function, max_length = 15))
    }
  }, striped = TRUE, spacing = 'xs', width = 'auto', align = 'l')

  # functions
  show_problems <- function() {
    modal <- problem_modal()
    showModal(modal)
  }

  # button trigger
  observeEvent(input$show_problems, {
    req(get_iso_files())
    showModal(problem_modal())
  })

  # button label
  observeEvent(get_iso_files(), {
    req(get_iso_files())
    updateActionButton(session, "show_problems",
                       label = sprintf("Problems (%.0f)", nrow(isoreader::iso_get_problems(get_iso_files()))))
  })

  # return functions (note: toggling the button visibility somehow does not work)
  list(
    show_problems = show_problems
  )
}


#' Dataset problems button
#' @param tooltip what tooltip to display
problems_button <- function(id, tooltip = "Show problems.") {
  ns <- NS(id)
  tooltipInput(actionButton, ns("show_problems"), "Problems", icon = icon("ambulance"), tooltip = tooltip)
}


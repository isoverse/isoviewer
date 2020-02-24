#' File selector server
#' @param get_variable reactive function returning the name of the variable
#' @param get_iso_files reactive function returning the actual iso files
module_file_selector_server <- function(input, output, session, get_variable, get_iso_files) {

  # namespace
  ns <- session$ns

  # reactive values =====
  values <- reactiveValues(
    iso_files = NULL
  )

  # data set name output ====
  output$dataset <- renderText(paste0("Dataset: ", get_variable()))

  # isofiles selector server ====
  selector <- callModule(
    selectorTableServer, "selector",
    id_column = "file_id", row_column = "row_id",
    column_select = c(File = file_id, Errors = error, Warning = warning)
  )

  # generate isofiles table ====
  observe({
    req(iso_files <- get_iso_files())
    isolate({
      module_message(ns, "debug", "setting iso files selection table")
      df <- get_iso_files() %>%
        iso_get_problems_summary(problem_files_only = FALSE) %>%
        mutate(
          row_id = dplyr::row_number(),
          warning = as.character(warning),
          error = as.character(error)
        )
      selector$set_table(
        df,
        initial_selection =
          get_gui_setting(ns(paste0("selector-", get_variable())), default = c())
      )
    })
  })

  # monitor selected files ====
  observe({
    req(get_iso_files())
    module_message(
      ns, "debug", "selected files: ",
      paste(selector$get_selected(), collapse = ", ")
    )
    # store selected in settings
    set_gui_setting(ns(paste0("selector-", get_variable())), selector$get_selected())
  })

  # dataset download ====
  output$download <- downloadHandler(
    filename = function() { basename(isoreader:::get_rds_export_filepath(get_iso_files(), get_variable())) },
    content = function(filename) {
      req(get_iso_files())
      module_message(ns, "info", "downloading entire dataset ", get_variable())
      file_path <- file.path(tempdir(), isoreader:::get_rds_export_filepath(get_iso_files(), get_variable()))
      withProgress({
          iso_save(get_iso_files(), file_path, quiet = TRUE)
          file.copy(from = file_path, to = filename)
        },
        message = "Preparing dataset...",
        detail = sprintf("Generating export file '%s'", basename(file_path)),
        value = 0.5
      )
    }
  )

  # FIXME: omit problematic =======
  # omit_problematic <- function() {
  #   if (length(values$omit_problematic) > 0 && !is.null(values$loaded_isofiles)) {
  #     values$omit_isofiles <- iso_omit_files_with_problems(
  #       values$loaded_isofiles, quiet = TRUE,
  #       remove_files_with_errors = "error" %in% values$omit_problematic,
  #       remove_files_with_warnings = "warning" %in% values$omit_problematic)
  #     if (length(values$omit_isofiles) == 0)
  #       values$omit_isofiles <- NULL
  #   } else {
  #     values$omit_isofiles <- values$loaded_isofiles
  #   }
  # }
  #
  # # changing omit
  # observe({
  #   new_omit <- input$omit
  #   isolate({
  #     values$omit_problematic <- input$omit
  #     omit_problematic()
  #   })
  # })
  #

  # problems ====
  callModule(problems_server, "problems", get_variable = get_variable, get_iso_files = get_iso_files)

  # FIXME: code update ====
  # code_update <- reactive({
  #   # trigger code update for any of the below variables changing
  #   function(rmarkdown = TRUE) {
  #     generate_data_selection_code(
  #       dataset = values$loaded_dataset %>% { if(is.null(.)) NULL else basename(.) },
  #       read_func = load_func,
  #       omit_type = values$omit_problematic,
  #       select_files = # omit file selection if ALL files are selected
  #         if (!is.null(values$omit_isofiles) && all(names(values$omit_isofiles) %in% isofiles_selector$get_selected()))
  #           NA_character_
  #       else isofiles_selector$get_selected(),
  #       rmarkdown = rmarkdown
  #     )
  #   }
  # })

  # FIXME: return functions ====
  # list(
  #   get_isofiles = get_selected_isofiles,
  #   get_code_update = code_update
  # )

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
      selectorTableUI(ns("selector")),
      footer =
        div(id = ns("actions"),
            tooltipOutput(downloadButton, ns("download"), "Download",
                          tooltip = "Download entire dataset"),
            spaces(1),
            problems_button(ns("problems"), tooltip = "Show problems reported for this dataset."),
            spaces(1),
            selectorTableButtons(ns("selector"))#,
            # FIXME: omit with errors functionality =====
            # checkboxGroupInput(
            #   ns("omit"), label = NULL, inline = TRUE,
            #   choices = c("Omit files with errors" = "error",
            #               "Omit files with warnings" = "warning")
            # )
        )
    )
  )
}


#' Problems Server
#' Stand-alone for showing a dataset's problems.
#' @inheritParams module_file_selector_server
#' @family datasets module functions
problems_server <- function(input, output, session, get_variable, get_iso_files) {

  # namespace and constants
  ns <- session$ns
  mail_address <- "sebastian.kopf@colorado.edu"
  mail_subject <- "Problematic Isofile"
  mail_body <- "I have encountered problems reading the attached IRMS data file(s)."

  # the modal dialog
  problem_modal <- reactive({
    req(get_iso_files())
    module_message(ns, "debug", "showing problems modal dialog for ", get_variable())
    modalDialog(
      title = h3(sprintf("Problems in '%s'", get_variable())),
      p("The following read problems were reported in this dataset. If any problems are unexpected (i.e. the files should have valid data), please ",
      strong(a(href = sprintf("mailto:%s?subject=%s&body=%s",
                              mail_address, str_replace_all(mail_subject, " ", "%20"), str_replace_all(mail_body, " ", "%20")),
               "send us an email")),
      " and attach at least one of the problematic file(s). Your help is much appreciated."),
      tableOutput(ns('problems')),
      footer = modalButton("Close"), fade = FALSE, easyClose = TRUE, size = "l"
    )
  })

  # problems table
  output$problems <- renderTable({
    req(get_iso_files())
    probs <-iso_get_problems(get_iso_files())
    if (nrow(probs) == 0) {
      tibble(Problem = "no problems")
    } else {
      select(probs, File = file_id, Type = type, Function = func, Problem = details) %>%
        mutate(Function = wrap_function_name(Function, max_length = 15))
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
                       label = sprintf("Problems (%.0f)", nrow(iso_get_problems(get_iso_files()))))
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


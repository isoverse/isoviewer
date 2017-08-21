

#' Problems Server
#' Stand-alone for showing a collection's problems.
#' @inheritParams fileSelectorServer
#' @param collection reactive function returning the isofiles list
#' @param collection_path reactive function returning the collection's path
#' @family collections module functions
problemsServer <- function(input, output, session, collection, collection_path) {

  # namespace and constants
  ns <- session$ns
  mail_address <- "sebastian.kopf@colorado.edu"
  mail_subject <- "Problematic Isofile"
  mail_body <- "I have encountered problems reading the attached IRMS data file(s)."

  # the modal
  problem_modal <- reactive({
    req(collection_path())
    module_message(ns, "debug", "showing problems modal dialog")
    name <- basename(collection_path()) %>% str_replace("\\.(\\w+)\\.rda$", "")
    modalDialog(
      title = "Problems",
      sprintf("The following problems were encountered during the loading of collection '%s'.", name),
      "If any problems are unexpected (i.e. the files should have valid data), please ",
      strong(a(href = sprintf("mailto:%s?subject=%s&body=%s",
                              mail_address, str_replace_all(mail_subject, " ", "%20"), str_replace_all(mail_body, " ", "%20")),
               "send us an email")),
      " and attach at least one of the problematic file(s). Your help is much appreciated.",
      tableOutput(ns('problems')),
      footer = NULL, fade = FALSE, easyClose = TRUE, size = "l"
    )
  })

  # problems table
  output$problems <- renderTable({
    req(collection())
    probs <- problems(collection())
    if (nrow(probs) == 0) data_frame(Problem = "no problems")
    else select(probs, File = file_id, Type = type, Function = func, Problem = details)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = 'l')

  # functions
  show_problems <- function() {
    showModal(problem_modal())
  }

  # button trigger
  observeEvent(input$show_problems, {
    if (is.null(collection_path()) || is.null(collection()))
      alert("No collection loaded yet.")
    else
      showModal(problem_modal())
  })

  # return functions
  list(
    show_problems = show_problems
  )
}


#' Collection problems button
#' @inheritParams fileSelectorUI
#' @param start_disabled whether to start out with the button disabled
#' @param tooltip what tooltip to display
#' @family collections module functions
problemsButton <- function(id, tooltip = "Show problems.") {
  ns <- NS(id)
  tooltipInput(actionButton, ns("show_problems"), "Problems", icon = icon("ambulance"), tooltip = tooltip)
}

# Selector table

#' Selector table server
#'
#' This generates an rhandson table for selecting items to include in downstream operations.
#'
#' @inheritParams isofilesLoadServer
#' @param id_column name of the id column that should be used for reporting selection
#' @param col_headers name of the column headers
#' @param hot_mods hands on table modifiers
#' @family selector table module functions
selectorTableServer <- function(input, output, session, id_column, col_headers, hot_mods = NULL) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    table = NULL, # what is available
    selected = c(), # what is selected
    update_selected = 0 # trigger selection update (circumventing circular triggers with user selection)
  )

  # render table
  output$selection_table = renderRHandsontable({

    req(values$table) # if the table changes
    values$update_selected # if the selection changes
    module_message(ns, "debug", "rendering selection table")

    # isolate hereafter to generate rhandsontable
    isolate({
      table <- values$table
      table$include <- table[[id_column]] %in% values$selected
      hot <- table %>%
        select(include, everything()) %>%
        rhandsontable(colHeaders = c(" ", col_headers)) %>%
        hot_table(readOnly = TRUE, highlightRow = TRUE, columnSorting = FALSE, contextMenu = FALSE) %>%
        hot_col(col = " ", halign = "htCenter", readOnly = FALSE)
      if (!is.null(hot_mods) && is.function(hot_mods))
        hot <- hot_mods(hot)
      return(hot)
    })
  })

  # selection
  observeEvent(input$selection_table, {
    values$selected <-
      input$selection_table %>%
      hot_to_r() %>%
      filter(include) %>%
      { .[[id_column]] }
  })

  observeEvent(input$select_all, {
    values$selected <- values$table[[id_column]]
    values$update_selected <- values$update_selected + 1
  })

  observeEvent(input$deselect_all, {
    values$selected <- c()
    values$update_selected <- values$update_selected + 1
  })

  # button visibility
  observe({
    toggle("select_all", condition = !is.null(values$table))
    toggle("deselect_all", condition = !is.null(values$table))
  })

  # return functions
  list(
    set_table = function(table) { values$table <- table },
    set_selected = function(selected) { isolate({
      values$selected <- selected
      values$update_selected <- values$update_selected + 1
    }) },
    get_selected = reactive({ values$selected })
  )
}


#' Selector table UI
#' @inheritParams isofilesLoadUI
#' @family selector table module functions
selectorTableUI <- function(id, height = "200px") {
  ns <- NS(id)
  rHandsontableOutput(ns("selection_table"), width = "100%", height = height) %>%
    withSpinner(type = 7, proxy.height = height)
}

#' Selector table buttons
#' @inheritParams isofilesLoadUI
#' @family selector table module functions
selectorTableButtons <- function(id) {
  ns <- NS(id)
  tagList(
    hidden(actionButton(ns("select_all"), "Select all", icon = icon("check-square-o"))),
    spaces(1),
    hidden(actionButton(ns("deselect_all"), "Deselect all", icon = icon("square-o")))
  )
}
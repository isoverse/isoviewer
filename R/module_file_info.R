#' File Info Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @param visible reactive function determining visibility of the auxiliary boxes
#' @family file info module functions
fileInfoServer <- function(input, output, session, isofiles, visible = NULL) {

  # namespace
  ns <- session$ns

  # file info selector
  file_info_selector <- callModule(
    selectorTableServer, "selector", id_column = "info", col_headers = c("Info"))

  # generate selector list
  observe({
    req(length(isofiles()) > 0)
    columns <- names(aggregate_file_info(isofiles(), quiet = TRUE)) %>%
      { .[!. %in% c("file_id", "file_path", "file_subpath")] } # do not allow file path while on server
    # set table
    file_info_selector$set_table(data_frame(info = columns), initial_selection = "file_datetime")
  })

  # show selector list box
  observe({
    if (is.function(visible))
      toggle("selector_box", condition = visible() & length(isofiles()) > 0 )
    else
      toggle("selector_box", condition = length(isofiles()) > 0)
  })

  # file info table
  output$table <- renderTable({
    validate(
      need(length(isofiles()) > 0, "Please select a dataset and at least one data file.") %then%
        need(length(file_info_selector$get_selected()) > 0, "Please select at least one file info column.")
    )
    module_message(ns, "debug", "rendering file info table")
    table <- aggregate_file_info(isofiles(), select = c("file_id", file_info_selector$get_selected()), quiet = TRUE)
    for (col in which(sapply(table, inherits, "POSIXct"))) # xtable does not deal well with datetime
      table[[col]] <- format(table[[col]])
    return(table)
  }, striped = TRUE, spacing = 'xs', width = '100%', align = 'l')


  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_file_info_code(
        selection = c("file_id", file_info_selector$get_selected()),
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update
  )
}


#' File Info Table UI
#' @inheritParams isofilesLoadUI
#' @family file info module functions
fileInfoTableUI <- function(id) {
  ns <- NS(id)
  div(style = 'overflow-x: scroll; height: 400px;',
      tableOutput(ns("table")) %>% withSpinner(type = 5, proxy.height = "400px;"))
}


#' File Info Selector UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @param selector_height file selector height
#' @family file info module functions
fileInfoSelectorUI <- function(id, width = 4, selector_height = "200px") {
  ns <- NS(id)

  div(id = ns("selector_box"),
      default_box(
        title = "File Info Selector", width = width,
        selectorTableUI(ns("selector"), height = selector_height),
        footer = div(
          #style = "height: 35px;",
          selectorTableButtons(ns("selector")))
      )
  )%>% hidden()
}

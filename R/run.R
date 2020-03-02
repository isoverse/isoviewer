# NOTE: consider implementing this
# iso_start_single_viewer(iso_object)

#' Start the isoviewer graphical user interface
#'
#' Searches for all iso file objects in the workspace and provides a graphical interface to exlore the data stored in them.
#'
#' @inheritParams viewer_server
#' @param log whether to show log info messages or not (by default only if running as server application, i.e. non-local)
#' @param runApp_params passed on to the \code{\link[shiny]{runApp}} call. Common parameters to specify include \code{port} and \code{launch.browser}. Only relevant if \code{local = TRUE}.
#' @export
iso_start_viewer <- function(
  iso_objects = iso_find_objects(),
  local = TRUE,
  log = !local,
  runApp_params = list()) {

  # check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the isoviewer graphical user interface during knitting. If you would like to keep a call to iso_start_viewer() in your RMarkdown file and avoid this warning, make sure to set ```{r, eval = FALSE} in the options for this code chunk.", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  # log messages
  if (log) turn_log_on()
  else turn_log_off()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # generate app
  app <- shinyApp(
    ui = viewer_ui(),
    server = viewer_server(
      iso_objects = iso_objects,
      local = local
    )
  )

  # launch if local
  if (local) {
    args <- list(app, display.mode = "normal") %>% modifyList(runApp_params)
    do.call(runApp, args = args)
  } else {
    return(app)
  }
}

#' Start the isoviewer graphical user interface
#'
#' Searches for all iso file objects in the workspace and provides a graphical interface to exlore the data stored in them.
#'
#' @param iso_files variable name - if provided, will start the viewer on this collection of iso files
#' @param restore whether to restore the GUI to the previous state (if available). If \code{TRUE}, will store the GUI settings for future restore.
#' @param reset whether to reset the GUI state upon viewer start
#' @param log whether to show log info messages or not
#' @param launch whether to launch the shiny app or return it as an object
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @export
iso_start_viewer <- function(iso_files = NULL, ..., restore = TRUE, reset = FALSE, log = FALSE, launch = TRUE) {

  # check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the isoviewer graphical user interface during knitting. If you would like to keep a call to iso_start_viewer() in your RMarkdown file and avoid this warning, make sure to set ```{r, eval = FALSE} in the options for this code chunk.", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  # get iso_files name
  iso_files_quo <- rlang::enquo(iso_files)
  if (rlang::quo_is_null(iso_files_quo)) {
    iso_files_name <- NULL
  } else if (isoprocessor:::quo_is_value(iso_files_quo)) {
    iso_files_name <- rlang::eval_tidy(iso_files_quo) %>% as.character()
  } else {
    iso_files_name <- rlang::as_label(iso_files_quo)
  }

  # variable safety checks
  if (!is.null(iso_files_name) && !exists(iso_files_name))
    sprintf("iso files variable '%s' does not exist", iso_files_name) %>%
    stop(call. = FALSE)
  if (!is.null(iso_files_name) && !isoreader::iso_is_object(get(iso_files_name)))
    sprintf("iso files variable '%s' is not an iso object", iso_files_name) %>%
    stop(call. = FALSE)

  # reset
  if(reset) reset_gui_settings()

  # log messages
  if (log) turn_log_on()
  else turn_log_off()

  # GUI settings
  if (restore) turn_gui_settings_on()
  else turn_gui_settings_off()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # generate app
  app <- shinyApp(
    ui = viewer_ui(),
    server = viewer_server(selected_variable = iso_files_name)
  )

  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)

}

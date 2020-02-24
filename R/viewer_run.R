#' Start the user interface
#' @param iso_files variable name - if provided, will load the viewer just for this collection of isofiles
#' @param restore whether to restore the GUI to the previous state (if possible). If \code{TRUE}, will store the GUI settings for future restore.
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @export
iso_start_viewer <- function(iso_files = NULL, restore = TRUE, ..., launch = TRUE) {

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
  if (!is.null(iso_files_name) && !iso_is_object(get(iso_files_name)))
    sprintf("iso files variable '%s' is not an iso object", iso_files_name) %>%
    stop(call. = FALSE)

  # turn GUI settings on/off
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

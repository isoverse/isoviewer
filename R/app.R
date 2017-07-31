#' Start the user interface
#'
#' @inheritParams app_server
#' @inheritParams app_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call, can include server-specific parameters such as host or port
#' @export
run <- function(data_dir = ".", ...) {

  # safety checks
  if (!file.exists(data_dir))
    stop("Could not find data directory '", data_dir, " from the current working directory: ", getwd(), call. = FALSE)

  # start-up message
  message("\n***************************************************************",
          "\nINFO: Launching isoviewer GUI (version ", packageVersion("isoviewer"), ")...",
          if (setting("debug")) "\nINFO: Debug mode is turned ON" else "",
          "\nINFO: App directory: ", getwd(),
          "\nINFO: Data directory: ", data_dir
          #"\nINFO: Settings file: ", file.path(data_dir, SETTINGS_FILE),
          #"\nINFO: History folder: ", file.path(data_dir, INSTRUMENT_HISTORY_FOLDER)
  )

  # start app
  runApp(
    list(
      ui = app_ui(),
      server = app_server(data_dir)
    ),
    display.mode = "normal", ...
  )

}

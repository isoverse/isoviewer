# TODO: the single file direct mode still need implementing! (i.e. if iso_objects points a single variable)
#' Start isoviewer GUI
#'
#' Provides a graphical user interface to explore continuous flow, dual inlet and scan isofile objects. Note that while the GUI is running, your R session will be blocked. To stop the GUI and return to your R session, either click the \code{Close} button inside the GUI navigation bar (if running it in non-server mode) or press \code{Esc} in your R session.
#'
#' @details \code{iso_start_viewer} starts a simple personal GUI that is only accessible on the computer it is running on.
#'
#' @param iso_objects which iso objects to make accessible in the GUI. By default searches through the current workspace and makes all iso objects it can find available. If a single variable is provided (rather than a named list), launches the GUI only for that variable (no others are accessible).
#' @param log whether to show log info messages or not. By default, shows logs when running in server mode (\code{iso_start_viewer_server}) and does not show them otherwise.
#' @param reset whether to reset the viewer settings and memory back to their defaults. Usually only necessary if your viewer crashed unexpectedly and gets stuck during restart. Please consider reporting the details of the crash at https://github.com/isoverse/isoviewer/issues, especially if you can easily reproduce it.
#' @export
iso_start_viewer <- function(iso_objects = iso_find_objects(), log = FALSE, reset = FALSE) {
  # check for reset
  if (reset) assign(".isoviewer_gui_settings", NULL, env = .GlobalEnv)

  # launch viewer
  start_viewer(
    iso_objects = !!rlang::enquo(iso_objects),
    local = TRUE, launch = TRUE, log = log
  )
}

#' @rdname iso_start_viewer
#' @details \code{iso_start_viewer_server} starts a server for GUI. If running it from an R session on a normal computer, it will be available in the local network at the computer's IP address, port 3838. For example, if you run it on your computer and your IP address is 192.168.0.42, other computers on your network can launch your GUI by pointing a browser at http://192.168.0.4:3838 (note that if your firewall or local network blocks port 3838, this will not be possible). If running on a shinyapps server such as shinyapps.io instead, you have to set \code{launch = FALSE} because the shinyapps server takes care of launching the app on demand.
#'
#' @param launch whether to launch the shiny server immediately (if starting the server from a local R instance) or only on request (if running on a server such as shinyApps.io).
#' @export
iso_start_viewer_server <- function(iso_objects = iso_find_objects(), launch = TRUE, log = TRUE) {
  start_viewer(
    iso_objects = !!rlang::enquo(iso_objects),
    local = FALSE, launch = launch, log = log,
    launch.browser = FALSE, host = "0.0.0.0", port = 3838
  )
}

# iso viewer start function
# @param ... parameters passed on to runApp
start_viewer <- function(iso_objects = iso_find_objects(), local, launch, log, ...) {

  # check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the isoviewer graphical user interface during knitting. If you would like to keep a call to iso_start_viewer() in your RMarkdown file and avoid this warning, make sure to set ```{r, eval = FALSE} in the options for this code chunk.", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  stopifnot(!missing(local))
  stopifnot(!missing(launch))
  stopifnot(!missing(log))

  # log messages
  if (log) turn_log_on()
  else turn_log_off()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # iso objects
  iso_objects_quo <- rlang::enquo(iso_objects)
  iso_objects_text <- rlang::as_label(iso_objects_quo)
  iso_objects <- rlang::eval_tidy(iso_objects_quo)

  if (isoreader::iso_is_object(iso_objects))
    iso_objects <- rlang::set_names(list(iso_objects), iso_objects_text)

  if (length(iso_objects) > 0 && !is.list(iso_objects) && !all(purrr::map_lgl(iso_objects, iso_is_object)))
    stop("must provide a single iso file, iso file list or list of iso file objects for the iso_objects parameter", call. = FALSE)

  # generate app
  app <- shinyApp(
    ui = viewer_ui(),
    server = viewer_server(
      iso_objects = iso_objects,
      local = local
    )
  )

  # launch if local
  if (launch) {
    runApp(app, display.mode = "normal", ...)
  } else {
    return(app)
  }
}

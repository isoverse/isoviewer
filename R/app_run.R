#' Start the user interface
#'
#' @inheritParams app_server
#' @inheritParams app_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' @export
run <- function(data_dir = ".", ..., launch = TRUE) {

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

  # generate app
  app <- shinyApp(
    ui = app_ui(),
    server = app_server(data_dir)
  )

  # launch or return
  if (launch)
    runApp(app, display.mode = "normal", ...)
  else
    return(app)
}


# Install the user interface
#
# Installs the user interface in the target folder with the provided call parameters for launching it via \code{\link[shiny]{runApp}} or as a server application.
# @param install_dir the installation directory (has to exist)
# @param ... parameters for the \code{run} function
install <- function(install_dir, ...) {
  if(!dir.exists(install_dir)) stop("directory does not exist: ", install_dir, call. = FALSE)

  dots <- list(...)
  if (!"data_dir" %in% names(dots)) stop("must provide data_dir parameter", call. = FALSE)

  # generate function call
  parameters <-
    lapply(dots, function(i)
      if(is.numeric(i)) str_c("=", i)
      else if (is.character(i)) str_c("=\"", i, "\"")
      else stop("don't know how to process ", class(i))
    ) %>%
    { str_c(names(.), unlist(.)) } %>%
    str_c(collapse = ", ")

  # call
  sprintf("library(isoviewer)\nrun(%s, launch = FALSE)", parameters) %>%
    cat(file = file.path(install_dir, "app.R"))
  message("Info: installed isoviewer app into directory '", install_dir, "'")
}


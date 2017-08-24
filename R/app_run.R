#' Start the user interface
#'
#' @inheritParams app_server
#' @inheritParams app_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' @export
run <- function(data_dir = ".", allow_data_upload = FALSE, store_data = TRUE, ..., launch = TRUE) {

  # safety checks
  if (!file.exists(data_dir))
    stop("Could not find data directory '", data_dir, " from the current working directory: ", getwd(), call. = FALSE)

  # start-up message
  message("\n***************************************************************",
          "\nINFO: Launching isoviewer GUI (version ", packageVersion("isoviewer"), ")...",
          if (setting("debug")) "\nINFO: Debug mode is turned ON" else "",
          "\nINFO: App directory: ", getwd(),
          "\nINFO: Data directory: ", data_dir
  )

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # isoviewer information
  cache_dir <- isoreader:::setting("cache_dir")
  isoviewer_info <- list(
    isoreader_version = packageVersion("isoreader")
  )
  isoviewer_info_file <- file.path(cache_dir, "isoviewer.rds")
  if (!file.exists(cache_dir)) dir.create(cache_dir)

  # isoreader version
  message("INFO: Isoreader version: ", packageVersion("isoreader"))
  if (file.exists(isoviewer_info_file)) {
    old_isoviewer_info <- readRDS(isoviewer_info_file)
    if (isoviewer_info$isoreader_version != old_isoviewer_info$isoreader_version) {
      message("WARNING: Isoreader version has changed from previous run (",
              as.character(old_isoviewer_info$isoreader_version), ") - clearing out cache...")
      cleanup_isoreader_cache()
    }
  }
  message("INFO: Storing isoviewer information: ", isoviewer_info_file)
  saveRDS(isoviewer_info, file = isoviewer_info_file)

  # generate app
  app <- shinyApp(
    ui = app_ui(allow_data_upload = allow_data_upload),
    server = app_server(data_dir,
                        allow_data_upload = allow_data_upload,
                        store_data = store_data)
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

  # generate function call
  dots <- list(...)
  parameters <-
    lapply(dots, function(i)
      if(is.numeric(i) || is.logical(i)) str_c("=", i)
      else if (is.character(i)) str_c(" = \"", i, "\"")
      else stop("don't know how to process ", class(i))
    ) %>%
    { str_c(names(.), unlist(.)) } %>%
    str_c(collapse = ", ") %>%
    { if(length(.) > 0) str_c(., ", ") else "" } %>%
    str_c("launch = FALSE")

  # call
  sprintf("library(isoviewer)\nrun(%s)", parameters) %>%
    cat(file = file.path(install_dir, "app.R"))
  message("Info: installed isoviewer app into directory '", install_dir, "'")
}


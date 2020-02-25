.onLoad <- function(libname, pkgname) {
  # set default package options
  default_options <- list(
    isoviewer.debug = FALSE,
    isoviewer.gui_settings = TRUE
  )
  options(default_options)
  # actual gui settings
  if (!exists(".isoviewer_gui_settings"))
    .isoviewer_gui_settings <<- list()
  invisible()
}

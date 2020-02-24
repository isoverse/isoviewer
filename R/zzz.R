.onLoad <- function(libname, pkgname) {
  # set default package options
  default_options <- list(
    isoviewer.debug = FALSE,
    isoviewer.gui.settings = TRUE
  )
  options(default_options)
  invisible()
}

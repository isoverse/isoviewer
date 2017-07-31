.onLoad <- function(libname, pkgname) {
  # set default package options
  default_options <- list(
    isoviewer.debug = FALSE
  )
  options(default_options)
  invisible()
}

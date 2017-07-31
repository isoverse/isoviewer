# retrieve package settings, internal function, not exported
setting <- function(name) {
  value <- getOption(str_c("isoviewer.", name))
  if (is.null(value)) stop("isoviewer setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_setting <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("isoviewer.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("isoviewer.", name)))
  return(invisible(value))
}

# turn debug messages on/off
# not exported, used for internal debugging
turn_debug_on <- function() {
  set_setting("debug", TRUE)
}

turn_debug_off <- function(data) {
  set_setting("debug", FALSE)
}

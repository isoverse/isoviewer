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

# gui settings
set_gui_setting <- function(name, value) {
  if (are_gui_settings_on()) set_setting(paste0("gui.", name), value)
}

# returns gui setting if gui settings are on and the setting is available
# otherwise returns the default
get_gui_setting <- function(name, default = NULL) {
  if (!are_gui_settings_on()) return(default)
  value <- getOption(paste0("isoviewer.gui.", name))
  if (is.null(value)) return(default)
  return(value)
}

turn_gui_settings_on <- function() {
  set_setting("gui.settings", TRUE)
}

turn_gui_settings_off <- function() {
  set_setting("gui.settings", FALSE)
}

are_gui_settings_on <- function() {
  setting("gui.settings")
}

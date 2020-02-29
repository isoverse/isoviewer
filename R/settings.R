# retrieve package settings, internal function, not exported
setting <- function(name) {
  value <- getOption(stringr::str_c("isoviewer.", name))
  if (is.null(value)) stop("isoviewer setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_setting <- function(name, value, overwrite = TRUE) {
  if (overwrite || !stringr::str_c("isoviewer.", name) %in% names(options()))
    options(list(value) %>% setNames(stringr::str_c("isoviewer.", name)))
  return(invisible(value))
}

# turn debug mode on/off
# not exported, used for internal debugging
turn_debug_on <- function() {
  set_setting("debug", TRUE)
}

turn_debug_off <- function(data) {
  set_setting("debug", FALSE)
}

# turn logs on/off
turn_log_on <- function() {
  set_setting("log", TRUE)
}

turn_log_off <- function() {
  set_setting("log", FALSE)
}

# gui settings
turn_gui_settings_on <- function() {
  set_setting("gui_settings", TRUE)
}

turn_gui_settings_off <- function() {
  set_setting("gui_settings", FALSE)
}

are_gui_settings_on <- function() {
  setting("gui_settings")
}

# set gui settings (stored in workspace instead of options to make it easy to
# back them up or save them in a file)
set_gui_setting <- function(name, value) {
  if (are_gui_settings_on()) {

    settings <- .isoviewer_gui_settings
    settings[[name]] <- value
    assign(
      x = ".isoviewer_gui_settings",
      value = settings,
      envir = .GlobalEnv
    )

    # back up gui settings continuously if in debug mode
    if (setting("debug")) {
      save_gui_settings("gui_settings.rds")
    }
  }
  return(invisible(.isoviewer_gui_settings[[name]]))
}

# returns gui setting if gui settings are on and the setting is available
# otherwise returns the default
get_gui_setting <- function(name, default = NULL) {
  if (
    !are_gui_settings_on() ||
    is.null(.isoviewer_gui_settings[[name]])
  ) {
    return(default)
  } else {
    return(.isoviewer_gui_settings[[name]])
  }
}

# get all gui settings
get_all_gui_settings <- function() {
  if (are_gui_settings_on())
    return(.isoviewer_gui_settings)
  else
    return(NULL)
}

# get all gui settings table
get_all_gui_settings_table <- function() {
  settings <- get_all_gui_settings()
  if (is.null(settings) || length(settings) == 0) return(tibble::tibble())
  tibble::tibble(
    setting = names(settings),
    type = purrr::map_chr(settings, ~class(.x)[1]),
    length = purrr::map_int(settings, length),
    value = purrr::map_chr(settings, ~{
      if (is.list(.x)) "<list>"
      else if (is.data.frame(.x)) "<tibble>"
      else paste(as.character(.x), collapse = ", ")
    })
  ) %>%
    dplyr::arrange(setting)
}

# save gui settings
# @param file_path path to rds file
save_gui_settings <- function(file_path) {
  readr::write_rds(.isoviewer_gui_settings, file_path)
}

# load gui settings
load_gui_settings <- function(file_path) {
  stopifnot(file.exists(file_path))
  assign(
    x = ".isoviewer_gui_settings",
    value = readr::read_rds(file_path),
    envir = .GlobalEnv
  )
}

# reset gui settings
reset_gui_settings <- function() {
  .isoviewer_gui_settings <<- list()
  assign(
    x = ".isoviewer_gui_settings",
    value = list(),
    envir = .GlobalEnv
  )
  # back up gui settings continuously if in debug mode
  if (setting("debug")) {
    save_gui_settings("gui_settings.rds")
  }
}

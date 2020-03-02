#' stores the settings
#' @param initial_settings initial settings
#' @param store_in_global_env whether to load/save settings from/in global environment
module_settings_server <- function(input, output, session, settings, initial_settings = list(), store_in_global_env = FALSE) {

  # init message ====
  global_settings_var <- ".isoviewer_gui_settings"
  if(store_in_global_env && exists(global_settings_var, envir = .GlobalEnv)) {
    initial_settings <- get(global_settings_var, envir = .GlobalEnv)
  } else {
    initial_settings <- NULL
  }
  if (!is.null(initial_settings)) {
    module_message(session$ns, "info", "SETTINGS initializing settings with ", length(initial_settings), " values")
  }

  # reactive values ====
  values <- reactiveValues(
    settings = if(!is.null(initial_settings)) initial_settings else list()
  )

  # reset gui settings
  reset <- function() {
    module_message(session$ns, "info", "RESETTING gui settings")
    isolate({ values$settings <- list() })
    save()
  }

  # generate gui setting name
  make_name <- function(name, ns = NULL, variable = NULL) {
    if (!is.null(variable)){
      name <- paste0(name, "-", variable)
    }
    if (!is.null(ns)) {
      name <- ns(name)
    }
    return(name)
  }

  # genreate message save char
  make_msg_value <- function(value) {
    if (is.list(value)) "<list>"
    else if (is.data.frame(value)) "<tibble>"
    else paste(as.character(value), collapse = ", ")
  }

  # set gui setting
  set <- function(name, value, ns = NULL, variable = NULL, quiet = FALSE) {
    # make name
    name <- make_name(name, ns = ns, variable = variable)

    # info message if namespace is provided
    if (!is.null(ns) && !quiet) {
      module_message(
        ns, "info",
        sprintf("SETTINGS set '%s' to '%s'", name, make_msg_value(value))
      )
    }
    isolate({ values$settings[[name]] <- value })
    save()
  }

  # returns gui setting if available, otherwise returns the default
  get <- function(name, ns = NULL, variable = NULL, default = NULL) {
    # make name
    name <- make_name(name, ns = ns, variable = variable)

    isolate({
      if (is.null(values$settings[[name]])) return(default)
      else return(values$settings[[name]])
    })
  }

  # get all gui settings table
  get_all_as_table <- function() {
    isolate({
      if (length(values$settings) == 0) return(tibble::tibble())
      tibble::tibble(
        setting = names(values$settings),
        type = purrr::map_chr(values$settings, ~class(.x)[1]),
        length = purrr::map_int(values$settings, length),
        value = purrr::map_chr(values$settings, make_msg_value)
      ) %>% dplyr::arrange(setting)
    })
  }

  # save gui settings
  save <- function() {
    isolate({
      if (store_in_global_env) {
        # save settings in global environments
        assign(
          x = global_settings_var,
          value = values$settings,
          envir = .GlobalEnv
        )
        # back up gui settings continuously if in debug mode
        if (setting("debug")) {
          readr::write_rds(values$settings, "gui_settings.rds")
        }
      }
    })
  }

  # return functions
  # @NOTE: make more functions accessible as needed
  list(
    reset = reset,
    set = set,
    get = get,
    get_all_as_table = get_all_as_table
  )
}

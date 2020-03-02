#' Isoviewer App Server
#'
#' Generates the server part of the isoviewer app
#' @param iso_objects named list of iso file objects to make available in the viewer
#' @param local whether running locally or as a server application (e.g. on shinyapps.io)
viewer_server <- function(iso_objects = list(), local = FALSE) {

  shinyServer(function(input, output, session) {

    # settings server
    settings <- callModule(
      module_settings_server, "settings",
      store_in_global_env = local
    )

    # navigation bar (includes info screen)
    navbar <- callModule(
      module_navbar_server, "navbar",
      settings = settings,
      iso_objects = iso_objects,
      close_button = local
    )

    # continuous flow data
    cf <- callModule(
      module_data_cf_server, "cf",
      settings = settings,
      iso_objects = iso_objects,
      get_selected_variable = navbar$get_selected_cf_variable
    )

    # dual inlet data
    di <- callModule(
      module_data_di_server, "di",
      settings = settings,
      iso_objects = iso_objects,
      get_selected_variable = navbar$get_selected_di_variable
    )

    # scan data
    scan <- callModule(
      module_data_scan_server, "scan",
      settings = settings,
      iso_objects = iso_objects,
      get_selected_variable = navbar$get_selected_scan_variable
    )

  })

}

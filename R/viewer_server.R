#' Isoviewer App Server
#'
#' Generates the server part of the isoviewer app
viewer_server <- function(selected_variable = NULL) {

  shinyServer(function(input, output, session) {

    # navigation bar (includes welcome screen)
    navbar <- callModule(
      module_navbar_server, "navbar",
      selected_variable = selected_variable
    )

    # continuous flow data
    cf <- callModule(
      module_cf_server, "cf",
      get_selected_variable = navbar$get_selected_cf_variable
    )

    # dual inlet data
    di <- callModule(
      module_di_server, "di",
      get_selected_variable = navbar$get_selected_di_variable
    )

    # scan data
    scan <- callModule(
      module_scan_server, "scan",
      get_selected_variable = navbar$get_selected_scan_variable
    )

  })

}

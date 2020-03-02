#' Isoviewer App Server
#'
#' Generates the server part of the isoviewer app
#' @inheritParams module_navbar_server
viewer_server <- function(selected_variable = NULL, close_button = FALSE) {

  shinyServer(function(input, output, session) {

    # navigation bar (includes info screen)
    navbar <- callModule(
      module_navbar_server, "navbar",
      selected_variable = selected_variable,
      close_button = close_button
    )

    # continuous flow data
    cf <- callModule(
      module_data_cf_server, "cf",
      get_selected_variable = navbar$get_selected_cf_variable
    )

    # dual inlet data
    di <- callModule(
      module_data_di_server, "di",
      get_selected_variable = navbar$get_selected_di_variable
    )

    # scan data
    scan <- callModule(
      module_data_scan_server, "scan",
      get_selected_variable = navbar$get_selected_scan_variable
    )

  })

}

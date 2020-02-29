#' Isoviewer App UI
#'
#' @description Generates the user interface part of the isoviewer app
#' @param sidebar_width the width of the sidebar
#' @param deault_menu default selected menu
viewer_ui <- function(sidebar_width = 170, start_menu = "welcome", theme = "united") {

  # COLORS ----
  box_default <- "#222d32" # darker
  box_default <- "#2c3b41" # ligther
  box_default <- "#616161" # lightest
  options(spinner.color = "red")

  # TAGS ----
  tagList(

    # USE SHINY JS, DASHBOARD AND EXTENSIONS ----
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    code_preview_shinyjs_extension(),

    # CSS Styles ----
    tags$head(
      tags$style(
        type="text/css",
        HTML(stringr::str_c(
          # error validation output
          ".shiny-output-error-validation { color: black; font-size: 16px; padding: 20px 205px 20px 20px; }",
          ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
          # body top padding
          ".box-body {padding-top: 5px; padding-bottom: 0px}",
          # pads on shiny items
          ".form-group, .selectize-control {margin-bottom: 0px;}",
          # custom background box
          sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}",
                  box_default, box_default),
          sprintf(".box.box-solid.box-info{border:1px solid %s;}", box_default),
          sep="\n"))
      )
    ),

    # NAVBAR ----
    module_navbar_ui("navbar"),

    # BODY ----
    fluidPage(
      title = NULL,
      fluidRow(
        module_data_cf_ui("cf"),
        module_data_di_ui("di"),
        module_data_scan_ui("scan")
      )
    )
  )

}

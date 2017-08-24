#' Isoviewer App UI
#'
#' @description Generates the user interface part of the isoviewer app
#' @inheritParams app_server
#' @param sidebar_width the width of the sidebar
#' @param deault_menu default selected menu
app_ui <- function(allow_data_upload, sidebar_width = 170, start_menu = "welcome") {

  color <- "red" # see ?dashboardPage for options
  #box_default <- "#222d32" # darker
  box_default <- "#2c3b41" # ligther

  # set spinner color
  options(spinner.color = color)
  dashboardPage(

    # SKIN ----
    skin = color,

    # HEADER ----
    dashboardHeader(title = "Isoviewer", titleWidth = sidebar_width),

    # SIDEBAR ----
    sidebarMenu(
      id = "menu",
      "welcome" %>% { menuItem("Welcome", tabName = ., icon = icon("info"), selected = start_menu == .) },

      menuItem(
        "Dual Inlet", icon = icon("signal"), startExpanded = TRUE,
        "di_load" %>% { menuSubItem("Load", tabName = ., icon = icon("folder-open"), selected = start_menu == .) },
        "di_view" %>% { menuSubItem("View", tabName = ., icon = icon("pie-chart"), selected = start_menu == .) }
      ),

      menuItem(
        "Continuous Flow", icon = icon("area-chart"), startExpanded = TRUE,
        "cf_load" %>% { menuSubItem("Load", tabName = ., icon = icon("folder-open"), selected = start_menu == .) },
        "cf_view" %>% { menuSubItem("View", tabName = ., icon = icon("pie-chart"), selected = start_menu == .) }
      ),

     "scans" %>% { menuItem("Scans", tabName = ., icon = icon("line-chart"), selected = start_menu == .) },

      # STYLESHEET ----
      tags$head(
        tags$style(
          type="text/css",
          HTML(str_c(
            # error validation output
            #".shiny-output-error-validation { color: red; font-size: 16px; }", # do we want this read?
            ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
            # adjust sidebar height
            #".sidebar {height: 2000px}", # FIXME: make this dynamically long enough
            # body top padding
            ".box-body {padding-top: 5px; padding-bottom: 0px}",
            # pads on shiny items
            ".form-group, .selectize-control {margin-bottom: 0px;}",
            # custom background box
            str_interp(".box.box-solid.box-info>.box-header{color:#fff; background: ${col}; background-color: ${col};}", list(col = box_default)),
            str_interp(".box.box-solid.box-info{border:1px solid ${col};}", list(col = box_default)),
            sep="\n"))
        )
      ),


      # USE SHINY JS AND EXTENSIONS ---
      useShinyjs(),
      codePreviewShinyjsExtension()

    ) %>%
    dashboardSidebar(width = sidebar_width),

    # BODY ----
    tabItems(
      app_ui_welcome("welcome"),
      # DI ====
      tabItem(tabName = "di_load", isofilesLoadUI("di_load", label = "Dual Inlet")),
      tabItem(tabName = "di_view", dualInletViewUI("di_view")
      ),
      # CF ====
      tabItem(tabName = "cf_load", isofilesLoadUI("cf_load", label = "Continuous Flow")),
      tabItem(tabName = "cf_view", continuousFlowViewUI("cf_view")),
      # Scans ====
      tabItem(tabName = "scans", default_box(
        title = "Scans", width = 12,
        div(style = "height: 400px;", h5("Scan files are not yet supported in the GUI."))
      ))
    ) %>%
      # div class row necessary to have proper full sized body
      div(class = "row") %>%
      dashboardBody()

  )

}

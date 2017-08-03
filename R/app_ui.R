#' Isoviewer App UI
#'
#' @description Generates the user interface part of the isoviewer app
#' @param sidebar_width the width of the sidebar
#' @param deault_menu default selected menu
app_ui <- function(sidebar_width = 170, default_menu = "di_load") {

  color <- "red" # see ?dashboardPage for options

  # set spinner color
  options(spinner.color = color)
  dashboardPage(

    # SKIN ----
    skin = color,

    # HEADER ----
    dashboardHeader(title = "Isoviewer", titleWidth = sidebar_width),

    # SIDEBAR ----
    sidebarMenu(

      "welcome" %>% { menuItem("Welcome", tabName = ., icon = icon("info"), selected = default_menu == .) },

      menuItem(
        "Dual Inlet", icon = icon("signal"), startExpanded = TRUE,
        "di_load" %>% { menuSubItem("Load", tabName = ., icon = icon("folder-open"), selected = default_menu == .) },
        "di_view" %>% { menuSubItem("View", tabName = ., icon = icon("pie-chart"), selected = default_menu == .) },
        "di_export" %>% { menuSubItem("Export", tabName = ., icon = icon("mail-forward"), selected = default_menu == .) }
      ),

      menuItem(
        "Continuous Flow", icon = icon("area-chart"), startExpanded = TRUE,
        "cf_load" %>% { menuSubItem("Load", tabName = ., icon = icon("folder-open"), selected = default_menu == .) },
        "cf_view" %>% { menuSubItem("View", tabName = ., icon = icon("pie-chart"), selected = default_menu == .) },
        "cf_export" %>% { menuSubItem("Export", tabName = ., icon = icon("mail-forward"), selected = default_menu == .) }
      ),

     "scans" %>% { menuItem("Scans", tabName = ., icon = icon("line-chart"), selected = default_menu == .) },

      # STYLESHEET ----
      tags$head(
        tags$style(
          type="text/css",
          str_c(
            # error validation output
            ".shiny-output-error-validation { color: red; font-size: 16px; }",
            ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
            # adjust sidebar height
            # ".sidebar {height:2000px}\n", # FIXME: make this dynamically long enough
            # body top padding
            ".box-body {padding-top: 5px; padding-bottom: 0px;}",
            # pads on shiny items
            ".form-group, .selectize-control {margin-bottom: 0px;}",
            sep="\n")
        )
      ),


      # USE SHINY JS ---
      useShinyjs()

    ) %>%
    dashboardSidebar(width = sidebar_width),

    # BODY ----
    tabItems(
      app_ui_welcome("welcome"),
      app_ui_di_load("di_load")
    ) %>% dashboardBody()

  )

}

#' Isoviewer App UI
#'
#' @description Generates the user interface part of the isoviewer app
#' @param sidebar_width the width of the sidebar
#' @param deault_menu default selected menu
app_ui <- function(sidebar_width = 150, default_menu = "welcome") {

  dashboardPage(

    # SKIN ----
    skin = "red",

    # HEADER ----
    dashboardHeader(title = "Isoviewer"),

    # SIDEBAR ----
    sidebarMenu(

      menuItem("Welcome", tabName = "welcome", icon = icon("info"), selected = default_menu == "welcome"),
      menuItem("Instrument", tabName = "instrument", icon = icon("cog"), selected = default_menu == "instrument"),
      menuItem("Tuning", tabName = "tuning", icon = icon("music"), selected = default_menu == "tuning"),
      menuItem("Standards", tabName = "standards", icon = icon("check"), selected = default_menu == "standards"),
      menuItem("Data", tabName = "data", icon = icon("pie-chart"), selected = default_menu == "data"),
      menuItem("Scans", tabName = "scans", icon = icon("bar-chart"), selected = default_menu == "scans"),
      menuItem("Settings", tabName = "settings", icon = icon("wrench"), selected = default_menu == "settings"),

      # STYLESHEET ----
      tags$head(
        tags$style(HTML(".shiny-output-error-validation { color: red; font-size: 16px; }")),
        tags$style(HTML(".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }")),
        #tags$style(HTML(".sidebar {height:2000px}")), # FIXME: make this dynamically long enough
        tags$style(HTML(".box-body {padding-top: 15px; padding-bottom: 0px;}"))
      ),


      # USE SHINY JS ---
      shinyjs::useShinyjs()

    ) %>% dashboardSidebar(width = sidebar_width),

    # BODY ----
    tabItems(

      # WELCOME ----
      tabItem(tabName = "welcome",
              h1("Welcome to Isoviewer"),
              checkboxGroupInput("checkGroup",
                                 label = h3("Checkbox group"),
                                 choices = list("Choice 1" = 1,
                                                "Choice 2" = 2, "Choice 3" = 3),
                                 selected = 1),
              textOutput("text1")

      ),

      # INSTRUMENT ----
      tabItem(
        tabName = "instrument",
        tabsetPanel(
          id = "instrument_tabs", selected = "new",
          tabPanel(
            "New", value = "new",

            br(),
            box(title = NULL, collapsible = FALSE, solidHeader = FALSE, width = 12,
                column(4, div(align = "left", textInput("user", NULL, placeholder = "Please enter your name"))),
                column(8, div(align = "right", h4(actionLink("instrument_new_clear", "Clear all", icon = icon("rotate-left")))))
            )

          ),

          # PARAMETER HISTORY ----
          ui_component(),


          tabPanel("Full scan History", value = "full_scans",
                   h1("temp")),
          tabPanel("Peak shape History", value = "peak_shapes",
                   h1("temp"))
        )
      )

    ) %>% dashboardBody()

  )

}




# user interface function

ui_component <- function() {
  tabPanel(
    "Parameter History", value = "params",
    # Parameter selection box
    br(),
    box(
      title = "Parameter selection", collapsible = TRUE,
      status = "success", solidHeader = TRUE, width = 12
    ),

    # Plots box
    box(
      title = "Parameter history",
      status = "success", solidHeader = TRUE, width = 12
      #plotDownloadLink(id = "history_plot_download"),
      # tabsetPanel(
      #   id = "history_plot_tabs", selected = "i",
      #   tabPanel("Interactive Plot", value = "i",
      #            plotlyOutput("history_iplot", height="500px", width = "100%")),
      #   tabPanel("Static Plot", value = "gg",
      #            plotOutput("history_plot", height="500px", width = "100%"))
      # )
    )
  )
}

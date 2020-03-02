#' info server
#' @param get_variables reactive function returning the variables
#' @param get_settings reactive function returning the settings
#' @param reset_settings function to trigger settings reset
module_info_server <- function(input, output, session, settings, get_variables, get_settings, reset_settings) {
  # namespace
  ns <- session$ns

  # variables table =====
  output$variables <- DT::renderDataTable({
    req(get_variables())
    module_message(ns, "info", "VARIABLES TABLE rendering table")
    DT::datatable(
      get_variables(),
      options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50, 100), pageLength = 25),
      filter = "bottom"
    )
  })

  # settings table =====
  output$settings <- DT::renderDataTable({
    validate(
      need(!is.null(get_settings()) && nrow(get_settings()) > 0, "No settings available yet.") %then%
      need(input$tabs == "settings", "Loading...")
    )
    module_message(ns, "info", "SETTINGS TABLE rendering table")
    DT::datatable(
      get_settings(),
      options = list(orderClasses = TRUE, lengthMenu = c(5, 10, 25, 50, 100), pageLength = 10),
      filter = "bottom"
    )
  })

  # reset settings ====
  observeEvent(input$reset, reset_settings())

}

#' info ui
module_info_ui <- function(id) {
  ns <- NS(id)
  tabBox(
    id = ns("tabs"), width = 12,
    tabPanel("Welcome", value = "welcome", module_info_welcome_ui(id)),
    tabPanel("Variables", value = "variables", module_info_variables_ui(id)),
    tabPanel("Settings", value = "settings", module_info_settings_ui(id))
  )
}

#' variables ui
module_info_variables_ui <- function(id, min_height = "400px;") {
  ns <- NS(id)
  tagList(
    h3("Available variables in the current workspace"),
    div(style = paste0('overflow-x: scroll; min-height: ', min_height),
        DT::dataTableOutput(ns("variables")) %>% withSpinner(type = 5, proxy.height = min_height)
    )
  )
}

#' settings ui
module_info_settings_ui <- function(id, min_height = "800px;") {
  ns <- NS(id)
  tagList(
    h3("Current viewer settings",
       tooltipInput(actionButton, ns("reset"), label = "Reset", icon = icon("fast-backward"),
                    tooltip = "Reset the viewer to its original state, i.e. have it forget which tabs and tables you were last on and which items you had selected.")),
    div(style = paste0('overflow-x: scroll; min-height: ', min_height),
        DT::dataTableOutput(ns("settings")) %>% withSpinner(type = 5, proxy.height = min_height)
    )
  )
}

#' welcome ui
module_info_welcome_ui <- function(id) {
  tagList(

    h3("About"),
    h4("This graphical user interface is created by the ", strong(a("isoviewer", href = "http://isoviewer.isoverse.org", target = "_new")), " R package, which is part of the ", strong(a("isoverse", href = "http://www.isoverse.org", target = "_new")), " suite of open-source stable isotope data tools. To create this GUI locally, ", a("install isoviewer", href = "https://isoviewer.isoverse.org/#installation", target = "_new"), " and run ", tags$code("isoviewer::iso_start_viewer()"), " to inspect all ", a("isoreader", href = "http://isoreader.isoverse.org", target = "_new"), " file objects in your workspace."),

    h3("Motivation"),
    h4("The primary purpose of , ", strong(a("isoviewer", href = "http://isoviewer.isoverse.org", target = "_new")), " is to provide a learning tool that illustrates how to build literate RMarkdown data processing files for IRMS data using the underlying ", strong(a("isoreader", href = "http://isoreader.isoverse.org", target = "_new")), " and ", strong(a("isprocessor", href = "http://isoprocessor.isoverse.org", target = "_new")), " packages. For this purpose, all screens in this GUI feature a live ", strong("Code Preview"), " that shows the corresponding commands for all GUI functionality. The ", icon("commenting"), " icon in each code preview allows switching between the source cody only and the same code embedded in RMarkdown. The ", icon("download"), " icon in the code preview allows download of a full ", a("RMarkdown", href = "http://rmarkdown.rstudio.com/articles_intro.html", target = "_new"), " file that reflects the parameters entered in the GUI with the goal of allowing the user to reproduce the illustrated functionality offline with a simple script that records all steps. A secondary motivation is to provide flexible GUI modules that run on any computer/server completely platform-independent and open-source and can be used in lab-specific customized IRMS data interfaces, and to easily share the raw data for published dataset."),

    h3("How to use this GUI"),
    h4("The GUI searches for all iso file objects in the workspace and lists them under the ", strong("Continuous Flow"), ", ", strong("Dual Inlet"), " and ", strong("Scan"), " tabs in the menu, respectively. Simply click on the variable name of interest and it will load the iso files stored in the variable. Select which files to work with and explore/download/visualize the data from there. Make sure to check the different tabs ('Raw data', 'File Info', 'Method Info', etc.) when viewing the data to check what information is currently retrieved from the raw data files and what functionality is available."),

    h3("Disclaimer"),
    h4("The ", strong(a("isoreader", href = "http://isoreader.isoverse.org", target = "_new")), " (version ", as.character(packageVersion("isoreader")),"), ", strong(a("isoprocessor", href = "http://isoprocessor.isoverse.org", target = "_new")), " (version ", as.character(packageVersion("isoprocessor")),") and ", strong(a("isoviewer", href = "http://isoviewer.isoverse.org", target = "_new")), " (version ", as.character(packageVersion("isoviewer")),") packages are fully open-source (i.e. they are free as in 'freedom' and free as in 'free beer') and are provided as is. They are still in active development but release of a stable version to the Comprehensive R Archive Network (CRAN) is in progress. The source code is released under ",
       a(href="http://www.gnu.org/licenses/gpl-2.0.html", target = "_new", "GPL-2")," and is available on ",
       a(href="https://www.github.com/isoverse", target = "_new", "GitHub"), "."),
    h3("Feedback"),
    h4("Feedback is most welcome. These packages are intended as tools for data aggregation and large scale reproducible data processing for the scientific geochemical community but are unlikely to capture all relevant data formats at this point. Please use the repositories' respective issue trackers for any feedback, suggestions and especially bug reports (",
       a(href="https://github.com/isoverse/isoreader/issues", target = "_new", "isoreader issues"), ", ",
       a(href="https://github.com/isoverse/isoprocessor/issues", target = "_new", "isoprocessor issues"), ", ",
       a(href="https://github.com/isoverse/isoviewer/issues", target = "_new", "isoviewer issues"), ").")
  )
}

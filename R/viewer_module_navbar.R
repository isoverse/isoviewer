
#' Variable Selection Server
#'
#' Module to select environment variable
#' @param selected_variable the default variable to select (if any)
module_navbar_server <- function(input, output, session, selected_variable = NULL) {

  # namespace, and top level params
  ns <- session$ns

  # available variables in namespace
  available_variables <- find_iso_objects() #list(di = c(), cf = c(), scan = c())

  # non-variable menu items (make sure IDs are unique by tagging pi to them)
  welcome <- sprintf("welcome%.6f", pi)
  available_variables_NA <-
    setNames(
      sprintf("%s%.6f", names(available_variables), pi),
      names(available_variables)
    )

  # reactive values
  values <- reactiveValues(
    initialized = FALSE,
    selected_cf_variable = NULL,
    selected_di_variable = NULL,
    selected_scan_variable = NULL
  )

  # render navbar ====
  output$menu <- renderUI({

    # determine selected
    selected <-
      if (!is.null(selected_variable)) selected_variable
      else get_gui_setting(ns("menu"), default = welcome)
    if (!selected %in% unlist(available_variables))
      selected <- welcome

    # info message
    module_message(
      ns, "info", sprintf("creating navbar with the available variables: '%s', and selection '%s'",
      available_variables %>% unlist() %>%  paste(collapse = "', '"), selected)
    )

    # tab panels
    tab_panels <-
      purrr::map2(
        available_variables, available_variables_NA,
        ~ {
          if (length(.x) > 0)
            map(.x, tabPanel)
          else
            list(tabPanel(shiny::em("No data available"), value = .y))
        })

    # navbar
    navbarPage(
      theme = shinythemes::shinytheme("united"),
      title = "isoviewer",
      collapsible = FALSE,
      position = "static-top",
      id = ns("menu"),
      selected = selected,
      tabPanel("Info", value = welcome, icon = icon("info"),
         div(id = ns("welcome"), viewer_ui_welcome()) %>% shinyjs::hidden()
      ),

      # TODO: add upload screen to upload collection / read files ----

      do.call(navbarMenu, args = c(
        list("Continuous Flow", menuName = "cf", icon = icon("area-chart")),
        tab_panels$cf
      )),
      do.call(navbarMenu, args = c(
        list("Dual Inlet", menuName = "di", icon = icon("signal")),
        tab_panels$di
      )),
      do.call(navbarMenu, args = c(
        list("Scan", menuName = "di", icon = icon("line-chart")),
        tab_panels$scan
      ))
    )
  })

  # user selects navbar ----
  observeEvent(input$menu, {
    req(input$menu)
    select_navbar_item(id = input$menu, update_navbar = FALSE)
  })

  # select navbar item ----
  select_navbar_item <- function(id, update_navbar = TRUE) {
    module_message(ns, "info", "loading menu item: ", id)

    # update navbar if not already the new value
    if (update_navbar) updateNavbarPage(session, ns("menu"), selected = id)
    shinyjs::toggle("welcome", condition = id == welcome)

    # set gui setting
    set_gui_setting(ns("menu"), id)

    # default selections
    selected_cf_variable <- NULL
    selected_di_variable <- NULL
    selected_scan_variable <- NULL

    # check what is selected (if anything)
    if (id %in% available_variables$cf) {
      # cf variable selected
      selected_cf_variable <- id
    } else if (id == available_variables_NA[['cf']]) {
      # NA selected for cf
      selected_cf_variable <- NA_character_
    } else if (id %in% available_variables$di) {
      # di variable selected
      selected_di_variable <- id
    } else if (id == available_variables_NA[['di']]) {
      # NA selected for di
      selected_di_variable <- NA_character_
    } else if (id %in% available_variables$scan) {
      # di variable selected
      selected_scan_variable <- id
    } else if (id == available_variables_NA[['scan']]) {
      # NA selected for scan
      selected_scan_variable <- NA_character_
    }

    # update reactive values
    if (!identical(values$selected_cf_variable, selected_cf_variable))
      values$selected_cf_variable <- selected_cf_variable
    if (!identical(values$selected_di_variable, selected_di_variable))
      values$selected_di_variable <- selected_di_variable
    if (!identical(values$selected_scan_variable, selected_scan_variable))
      values$selected_scan_variable <- selected_scan_variable
  }

  # return reactive functions ====
  list(
    select_navbar_item = select_navbar_item,
    get_selected_cf_variable = reactive(values$selected_cf_variable),
    get_selected_di_variable = reactive(values$selected_di_variable),
    get_selected_scan_variable = reactive(values$selected_scan_variable)
  )
}


#' module variable selection navbar
#'
#' @param id the module id
module_navbar_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("menu"))
}




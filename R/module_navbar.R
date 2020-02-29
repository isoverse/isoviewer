
#' Variable Selection Server
#'
#' Module to select environment variable
#' @param selected_variable the default variable to select (if any)
module_navbar_server <- function(input, output, session, selected_variable = NULL) {

  # namespace
  ns <- session$ns

  # available variables in namespace
  all_objects <- find_iso_objects()
  available_variables <-
    list(
      di = dplyr::filter(all_objects, type == "dual inlet")$variable,
      cf = dplyr::filter(all_objects, type == "continuous flow")$variable,
      scan = dplyr::filter(all_objects, type == "scan")$variable
    )

  # non-variable menu items (make sure IDs are unique by tagging pi to them)
  info <- sprintf("info%.6f", pi)
  close_id <- sprintf("close%.6f", pi)
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
    selected_scan_variable = NULL,
    reset_settings = 1
  )

  # info server ====
  callModule(
    module_info_server, "info",
    get_variables = reactive({ all_objects }),
    get_settings = reactive({
      values$reset_settings
      input$menu
      get_all_gui_settings_table()
    }),
    reset_settings = function() {
      module_message(ns, "info", "RESETTING gui settings")
      reset_gui_settings()
      values$reset_settings <- values$reset_settings + 1
    }
  )

  # render navbar ====
  output$menu <- renderUI({

    # determine selected
    selected <-
      if (!is.null(selected_variable)) selected_variable
      else get_gui_setting(ns("menu"), default = info)
    if (!selected %in% unlist(available_variables))
      selected <- info

    # info message
    module_message(
      ns, "info", sprintf("NAVBAR creating navbar with the available variables: '%s', and selection '%s'",
      available_variables %>% unlist() %>%  paste(collapse = "', '"), selected)
    )

    # tab panels
    tab_panels <-
      purrr::map2(
        available_variables, available_variables_NA,
        ~ {
          if (length(.x) > 0)
            purrr::map(.x, tabPanel)
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
      tabPanel("Info", value = info, icon = icon("info"),
         div(module_info_ui(ns("info")))
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
      # tabPanel("Close", value = close_id, icon = icon("sign-out-alt"),
      #          box(h2("Saving settings and closing the application..."), width = 12)
      # )
    )
  })

  # user selects navbar ----
  observeEvent(input$menu, {
    req(input$menu)
    select_navbar_item(id = input$menu, update_navbar = FALSE)
  })

  # select navbar item ----
  select_navbar_item <- function(id, update_navbar = TRUE) {

    if (id == close_id) {
      # FIXME: this doesn't quite work properly yet with the displaying of the logout message (others aren't hiddne)
      # additional ideas for closing the window: https://github.com/daattali/advanced-shiny/blob/master/close-window/app.R
      module_message(ns, "info", "closing application")
      stopApp()
    } else {
      module_message(ns, "info", "NAVBAR loading menu item: '", id, "'")
    }

    # update navbar if not already the new value
    if (update_navbar) updateNavbarPage(session, ns("menu"), selected = id)
    shinyjs::toggle("info", condition = id == info)

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



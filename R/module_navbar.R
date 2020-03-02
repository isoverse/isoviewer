
#' Variable Selection Server
#'
#' Module to select environment variable
#' @param settings settings_server provide a central settings server if intending to share/save/restore settings accross the app, otherwise the default bare servers works fine.
#' @param close_button whether to include a close button
module_navbar_server <- function(
  input, output, session, settings,
  iso_objects = list(), close_button = FALSE) {

  # namespace
  ns <- session$ns

  # safety check
  if (length(iso_objects) > 0L) {
    if (length(names(iso_objects)) == 0L || any(nchar(names(iso_objects)) == 0L)) {
      stop("all iso objects provided to the viewer must be named", call. = FALSE)
    }
    if (any(duplicated(names(iso_objects)))) {
      stop("all iso objects provided to the viewer must have unique names", call. = FALSE)
    }
  }

  # available variables in namespace
  all_objects <- parse_iso_objects(iso_objects)
  type_objects <-
    list(
      di = dplyr::filter(all_objects, type == "dual inlet")$variable,
      cf = dplyr::filter(all_objects, type == "continuous flow")$variable,
      scan = dplyr::filter(all_objects, type == "scan")$variable
    )

  # non-variable menu items (make sure IDs are unique by tagging pi to them)
  info <- sprintf("info%.6f", pi)
  close_id <- sprintf("close%.6f", pi)
  type_objects_NA <-
    setNames(
      sprintf("%s%.6f", names(type_objects), pi),
      names(type_objects)
    )

  # reactive values
  values <- reactiveValues(
    last_menu_item = info,
    initialized = FALSE,
    selected_cf_variable = NULL,
    selected_di_variable = NULL,
    selected_scan_variable = NULL,
    reset_settings = 1
  )

  # info server ====
  callModule(
    module_info_server, "info",
    settings = settings,
    get_variables = reactive({ dplyr::select(all_objects, -.data$obj) }),
    get_settings = reactive({
      values$reset_settings
      input$menu
      settings$get_all_as_table()
    }),
    reset_settings = function() {
      settings$reset()
      values$reset_settings <- values$reset_settings + 1
    }
  )

  # render navbar ====
  output$menu <- renderUI({

    # determine selected
    selected <- settings$get("menu", ns, default = info)
    if (!selected %in% all_objects$variable) selected <- info

    # info message
    module_message(
      ns, "info", sprintf("NAVBAR creating navbar with the available variables: '%s', and selection '%s'",
      paste(all_objects$variable, collapse = "', '"), selected)
    )

    # tab panels
    tab_panels <-
      purrr::map2(
        type_objects, type_objects_NA,
        ~ {
          if (length(.x) > 0)
            purrr::map(.x, tabPanel)
          else
            list(tabPanel(shiny::em("No data available"), value = .y))
        })

    # navbar arguments
    args <- list(
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
    )

    # close button?
    if (close_button)
      args <- c(args, list(tabPanel("Close", value = close_id, icon = icon("window-close"), width = 12)))

    # navbar
    do.call(navbarPage, args = args)
  })

  # close dialog =====
  close_dialog <- modalDialog(
    title = NULL, fade = FALSE, easyClose = TRUE, size = "s",
    h2("Close GUI?"),
    footer =
      tagList(
        actionButton(ns("close"), label = "Close", icon = icon("window-close")),
        modalButton("Cancel")
      )
  )

  # close event ===
  observeEvent(input$close, {
    module_message(ns, "info", "APP closing...")
    shinyjs::js$closeWindow()
    stopApp()
  })

  # user selects navbar ----
  observeEvent(input$menu, {
    req(input$menu)
    select_navbar_item(id = input$menu, update_navbar = FALSE)
  })

  # select navbar item ----
  select_navbar_item <- function(id, update_navbar = TRUE) {

    if (id == close_id) {
      # show cancel model and switch back to previous tab
      showModal(close_dialog)
      id <- values$last_menu_item
      update_navbar <- TRUE
    } else {
      module_message(ns, "info", "NAVBAR loading menu item: '", id, "'")
    }

    # update navbar if not already the new value
    values$last_menu_item <- id
    if (update_navbar) updateNavbarPage(session, "menu", selected = id)
    shinyjs::toggle("info", condition = id == info)

    # set gui setting
    settings$set("menu", id, ns)

    # default selections
    selected_cf_variable <- NULL
    selected_di_variable <- NULL
    selected_scan_variable <- NULL

    # check what is selected (if anything)
    if (id %in% type_objects$cf) {
      # cf variable selected
      selected_cf_variable <- id
    } else if (id == type_objects_NA[['cf']]) {
      # NA selected for cf
      selected_cf_variable <- NA_character_
    } else if (id %in% type_objects$di) {
      # di variable selected
      selected_di_variable <- id
    } else if (id == type_objects_NA[['di']]) {
      # NA selected for di
      selected_di_variable <- NA_character_
    } else if (id %in% type_objects$scan) {
      # di variable selected
      selected_scan_variable <- id
    } else if (id == type_objects_NA[['scan']]) {
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

# generate code for navbar runctions
code_navbar_shinyjs_extension <- function() {

  js_code <- c(
    # close window
    closeWindow =
      "shinyjs.closeWindow = function() { window.close(); }"
    )

  tagList(
    extendShinyjs(text = paste(js_code, collapse = "\n"), functions = names(js_code))
  )
}

#' module variable selection navbar
#'
#' @param id the module id
module_navbar_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("menu"))
}




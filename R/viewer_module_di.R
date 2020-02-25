#' Dual inlet flow files Server
#' @param get_selected_variable reactive function returning the variable name
module_di_server <- function(input, output, session, get_selected_variable) {

  # namespace
  ns <- session$ns

  # file selector module
  files <- callModule(
    module_file_selector_server, "files",
    get_variable = get_selected_variable,
    get_iso_files = reactive({
      req(get_selected_variable())
      obj <- get(get_selected_variable(), envir = .GlobalEnv)
      stopifnot(iso_is_dual_inlet(obj))
      return(obj)
    })
  )

  # div visibility
  observe({
    shinyjs::toggle("div", condition = !is.null(get_selected_variable()) && !is.na(get_selected_variable()))
    shinyjs::toggle("no_data", condition = !is.null(get_selected_variable()) && is.na(get_selected_variable()))
  })

}


#' Dual files UI
module_di_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("div"),
      module_file_selector_ui(ns("files"))
    ) %>% shinyjs::hidden(),
    div(
      id = ns("no_data"),
      box(h2("Your workspace does not contain any dual inlet iso file objects."), width = 12)
    ) %>% shinyjs::hidden(),
  )
}

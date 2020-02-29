#' Run individual modules
#'
#' @inheritParams app_server
#' @inheritParams app_ui
#' @param ... passed on to the \code{\link[shiny]{runApp}} call (only if \code{launch = TRUE}), can include server-specific parameters such as host or port
#' @param launch whether to launch the app (TRUE) or return a shiny app object (FALSE) that then can be launched via \code{\link[shiny]{runApp}}
#' @export
iso_view_continuous_flow_files <- function(iso_files, ...) {

  # safety check
  iso_files_quo <- rlang::enquo(iso_files)
  if (missing(iso_files) || !isoreader::iso_is_continuous_flow(iso_files))
    stop("no continuous flow iso files provided for the viewer", call. = FALSE)

  # start-up message
  glue::glue(
    "Info: launching continuous flow files viewer (version {packageVersion('isoviewer')})..."
  ) %>% message()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  color <- "red" # see ?dashboardPage for options
  #box_default <- "#222d32" # darker
  box_default <- "#2c3b41" # ligther

  # set spinner color
  options(spinner.color = color)

  # generate app
  app <- shinyApp(
    ui = dashboardPage(

      dashboardHeader(title = "Test"),
      dashboardSidebar(
      ),
      dashboardBody(
        # STYLESHEET ----
        tags$head(
          tags$style(
            type="text/css",
            HTML(stringr::str_c(
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
              stringr::str_interp(".box.box-solid.box-info>.box-header{color:#fff; background: ${col}; background-color: ${col};}", list(col = box_default)),
              stringr::str_interp(".box.box-solid.box-info{border:1px solid ${col};}", list(col = box_default)),
              sep="\n"))
          )
        ),


        # USE SHINY JS AND EXTENSIONS ---
        useShinyjs(),
        code_preview_shinyjs_extension(),
        continuousFlowDataUI("cf_data", width = 12)
      )
    ),
    server = shinyServer(function(input, output, session) {
      # data viewer ====
      cf_data <- callModule(
        continuousFlowDataServer, "cf_data",
        isofiles = reactive(iso_files), dataset_name = reactive(rlang::as_label(iso_files_quo))
      )
    })
  )

  runApp(app, display.mode = "normal", ...)
}

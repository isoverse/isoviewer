#' Isoviewer App Server
#'
#' @description Generates the server part of the isoviewer app
#' @param data_dir the directory for local data files
app_server <- function(data_dir) {
  shinyServer(function(input, output, session) {

    # SETTINGS ----
    message("\n\nINFO: Loading GUI instance ...")
    message("INFO: Loading settings ...")
    #global <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "global")
    #modes <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "modes")
    #parameters <- read_excel(file.path(data_dir, SETTINGS_FILE), sheet = "parameters")

    # REACTIVE VALUES ----
    values <- reactiveValues(
      full_scan_file = NULL, # last saved full scan file
      peak_shape_file = NULL, # last saved peak shape file
      tuning_peak_shape_file = NULL, # last saved tuning peak shape file
      history_variables = c(),
      data_files_list = c(),
      data_files_selected = c(),
      data_files_objects = list(),
      data_files_table_data = NULL,
      data_files_mass_data = NULL,
      scan_files_list = c(),
      scan_files_selected = c(),
      scan_files_objects = list(),
      scan_files_data = NULL
    )

    message("MY SERVER")
    output$text1 <- renderText({
      str_c("You have selected this", str_c(input$checkGroup, collapse = ", "), " ", data_dir)
    })

    #server_component()

  })

}

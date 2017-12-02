# FIXME: exports should reflect the data processing that happens on the raw tab (i.e. signal scaling and ratio calculations)
# FIXME: have default for export be TRUE, not FALSE (if panel is never visited)

#' Export Server
#' @inheritParams fileInfoServer
#' @inheritParams isofilesLoadServer
#' @family export module
exportServer <- function(input, output, session, isofiles, dataset_name, visible = NULL) {

  # namespace and reactive values
  ns <- session$ns
  values <- reactiveValues(
    params = c()
  )

  # show selector box
  observe({
    if (is.function(visible))
      toggle("settings_box", condition = visible() & length(isofiles()) > 0 )
    else
      toggle("settings_box", condition = length(isofiles()) > 0)
  })

  # info and visibility of export buttons
  output$info <- renderText({
    validate(need(length(isofiles()) > 0, "Please select a dataset and at least one data file to enable export."))
    return(NULL)
  })
  observe(toggle("exports", condition = length(isofiles()) > 0))

  # export parameters ====
  params <- c(
    include_raw_data = "Raw Data",
    include_file_info = "File Info",
    include_method_info = "Method Info",
    include_vendor_data_table = "Vendor Data Table",
    include_problems = "Problems"
  )
  output$parameters <- renderUI({
    tagList(
      checkboxGroupInput(
        ns("export_params"), "Which data should be included in the export?",
        choices = setNames(names(params), params),
        selected = names(params))
    )
  })

  # download excel ====
  output$export_excel <- downloadHandler(
    filename = function() { str_c(dataset_name(), ".xlsx") },
    contentType = "application/octet-stream",
    content = function(filename) {
      if (length(isofiles()) > 0) {
        temp_file <- isoreader:::get_excel_export_filepath(isofiles(), tempfile())
        module_message(ns, "info", "exporting dataset to excel: ", basename(temp_file))
        withProgress(message = "Exporting to Excel...", value = 0.5, {
          params <- as.list(setNames(names(params) %in% input$export_params, names(params)))
          do.call(iso_export_to_excel, args = c(list(isofiles(), temp_file), params, list(quiet = TRUE)))
          file.copy(from = temp_file, to = filename)
          file.remove(temp_file)
        })
      }
    }
  )

  # download feather ====
  output$export_feather <- downloadHandler(
    filename = function() { str_c(dataset_name(), ".zip") },
    content = function(filename) {
      if (length(isofiles()) > 0) {
        temp_files <- isoreader:::get_feather_export_filepaths(isofiles(), file.path(tempdir(), dataset_name()))
        module_message(ns, "info", "exporting dataset to feather with basepath: ", temp_files[['base']])
        withProgress(message = "Exporting to Feather...", value = 0.5, {
          params <- as.list(setNames(names(params) %in% input$export_params, names(params)))
          do.call(iso_export_to_feather, args = c(list(isofiles(), temp_files[['base']]), params, list(quiet = TRUE)))

          # zip up the files
          feather_files <- temp_files %>% str_subset("\\.feather$") %>% { .[file.exists(.)] }
          ws_feather_files <- basename(feather_files)
          file.copy(from = feather_files, to = ws_feather_files)
          zip_file <- str_c(basename(tempfile()), ".zip")
          zip(zip_file, files = basename(ws_feather_files))
          file.copy(from = zip_file, to = filename)

          # cleanup
          file.remove(feather_files)
          file.remove(ws_feather_files)
          file.remove(zip_file)
        })
      }
    }
  )

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_export_code(
        filepath = dataset_name(),
        export_params = setNames(names(params) %in% input$export_params, names(params)),
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update
  )
}


#' Method Info Table UI
#' @inheritParams isofilesLoadUI
#' @family export module
exportUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("info")),
    div(id = ns("exports"),
        h3("Excel", align = "center"),
        "Export the", code("isofiles"), " to an Office Open XML (xlsx) file. The different kinds of data (raw data, file info, methods info, etc.) are exported to separate tabs within the excel file.", br(),
        tooltipOutput(customDownloadButton, ns("export_excel"), "Export to Excel", icon = icon("file-excel-o"),
                      tooltip = "Export the data to excel. Includes the kinds of data checked in the settings on the right.") %>% div(align = "center"),
        h3("Feather", align = "center"),
        p("Export the", code("isofiles"), " to the shared Python/R feather file format. The different kinds of data (raw data, file info, methods info, etc.) are exported to separate feather files. The export button bellow will issue download of all feather files combined into a zip archive."),
        tooltipOutput(customDownloadButton, ns("export_feather"), "Export to Feather", icon = icon("paper-plane"),
                      tooltip = "Export the data to feather. Includes the kinds of data checked in the settings on the right.") %>% div(align = "center"),
        br()
    ) %>% hidden()
  )
}


#' Export Setings UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family export module
exportSettingsUI <- function(id, width = 4) {

  ns <- NS(id)
  div(id = ns("settings_box"),
      default_box(
        title = "Export Settings", width = width,
        uiOutput(ns("parameters"))
      )
  ) %>% hidden()
}

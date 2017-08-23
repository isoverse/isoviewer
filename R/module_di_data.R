#' Dual Inlet Data Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @param dataset_name reactive function with the dataset name
#' @family view dual inlet module functions
dualInletDataServer <- function(input, output, session, isofiles, dataset_name) {

  # namespace
  ns <- session$ns


  # File Info =====
  file_info <- callModule(
    fileInfoServer, "file_info",
    isofiles = isofiles, visible = reactive({ input$tabs == "file_info" }))

  # Method Info ===
  method_info <- callModule(
    methodInfoServer, "method_info",
    isofiles = isofiles, visible = reactive({ input$tabs == "method_info" }))

  # Vendor Data Table ===
  vdt <- callModule(
    vendorDataTableServer, "vendor_data_table",
    isofiles = isofiles, visible = reactive({ input$tabs == "vendor_data_table" }))

  # Export ===
  export <- callModule(
    exportServer, "export",
    isofiles = isofiles, dataset_name = dataset_name,
    visible = reactive({ input$tabs == "export" }))

  # code update ====
  code_update <-  reactive({
    function(rmarkdown = TRUE) {
      code(
        file_info$get_code_update()(rmarkdown = rmarkdown),
        method_info$get_code_update()(rmarkdown = rmarkdown),
        vdt$get_code_update()(rmarkdown = rmarkdown),
        export$get_code_update()(rmarkdown = rmarkdown)
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update,
    get_data_tab = reactive({ input$tabs })
  )
}


#' Dual Inlet Data UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family view dual inlet module functions
dualInletDataUI <- function(id, width = 12) {
  ns <- NS(id)

  tagList(
    # TABS ====
    tabBox(
      title = NULL, width = 8, selected = "export",
      id = ns("tabs"),
      tabPanel("Raw Data", value = "raw_data", "First tab content",

               actionButton(ns("load"), "(Re)load", icon = icon("cog"))

      ),
      tabPanel("File Info", value = "file_info",
               fileInfoTableUI(ns("file_info"))),
      tabPanel("Method Info", value = "method_info",
               methodInfoTableUI(ns("method_info"))),
      tabPanel("Vendor Data Table", value = "vendor_data_table",
               vendorDataTableTableUI(ns("vendor_data_table"))),
      tabPanel("Export Data", value = "export",
               exportUI(ns("export")))
    ),

    # TAB SPECIFIC BOXES
    fileInfoSelectorUI(ns("file_info"), width = 4, selector_height = "200px"),
    methodInfoSelectorUI(ns("method_info"), width = 4),
    vendorDataTableSelectorUI(ns("vendor_data_table"), width = 4, selector_height = "300px"),
    exportSettingsUI(ns("export"), width = 4)

  ) %>% column(width = width) %>% fluidRow()
}


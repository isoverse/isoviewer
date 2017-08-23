#' Dual Inlet Data Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @family view dual inlet module functions
dualInletDataServer <- function(input, output, session, isofiles) {

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

  # code update ====
  code_update <-  reactive({
    function(rmarkdown = TRUE) {
      code(
        file_info$get_code_update()(rmarkdown = rmarkdown),
        method_info$get_code_update()(rmarkdown = rmarkdown)
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

  # parameters
  file_info_selection_height <- "100px"

  tagList(
    # TABS ====
    tabBox(
      title = NULL, width = 8, selected = "method_info",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = ns("tabs"), #height = "250px",
      tabPanel("Raw Data", value = "raw_data", "First tab content",

               actionButton(ns("load"), "(Re)load", icon = icon("cog"))

      ),
      # File Info =====
      tabPanel("File Info", value = "file_info", fileInfoTableUI(ns("file_info"))),
      tabPanel("Method Info", value = "method_info", methodInfoTableUI(ns("method_info"))),
      tabPanel("Vendor Data Table", value = "vendor_data_table", h3("Tab content 2"), h1("bla"))
    ),

    # TAB SPECIFIC BOXES
    fileInfoSelectorUI(ns("file_info"), width = 4, selector_height = "200px"),
    methodInfoSelectorUI(ns("method_info"), width = 4)
  ) %>% column(width = width) %>% fluidRow()
}


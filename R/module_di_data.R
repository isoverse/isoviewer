#' Dual Inlet Data Server
#' @inheritParams isofilesLoadServer
#' @param isofiles reactive function returning the currently loaded isofiles
#' @family view dual inlet module functions
dualInletDataServer <- function(input, output, session, isofiles) {


  list(
    start = reactive({  })
  )
}


#' Dual Inlet Data UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family view dual inlet module functions
dualInletDataUI <- function(id, width = 12) {
  ns <- NS(id)
  tagList(
    tabBox(
      title = NULL, width = width,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = ns("tabs"), #height = "250px",
      tabPanel("Raw Data", "First tab content",

               actionButton(ns("load"), "(Re)load", icon = icon("cog"))

      ),
      tabPanel("File Info", "Tab content 2"),
      tabPanel("Method Info", "Tab content 2"),
      tabPanel("Vendor Data Table", h3("Tab content 2"), h1("bla"))
    )
  )
}

# observe({
#   req(input$code_line)
#   code_preview$focus_code_preview(line = input$code_line, center = TRUE)
# })
#
# observe({
#   req(input$code_search)
#   code_preview$focus_code_preview(search = input$code_search, case_sensitive = FALSE)
# })

# numericInput(ns("code_line"), "Line", value = 0),
# textInput(ns("code_search"), "Search", value = ""),

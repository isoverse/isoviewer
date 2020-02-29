#' data download server
data_download_server <- function(input, output, session, get_variable) {

  # TODO: implement issue #16

  # namespace
  ns <- session$ns

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_export_code(
        dataset = get_variable(),
        rmarkdown = rmarkdown
      )
    }
  })

  # return functions =====
  list(
    get_code_update = code_update
  )
}


#' data download ui
plot_ui <- function(id) {

  # namespace
  ns <- NS(id)

}

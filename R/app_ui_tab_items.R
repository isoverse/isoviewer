# tab items ====

app_ui_welcome <- function(id) {
  tabItem(
    tabName = id,
    h1("Welcome to Isoviewer"),
    "my text", tags$br(),
    textOutput("text1")
  )
}

# dual inlet load screen
app_ui_di_load <- function(id, allow_data_upload) {

}

# tab items ====

app_ui_welcome <- function(id) {
  tabItem(
    tabName = id,
    h1("Welcome to Isoviewer"),
    "my text", tags$br(),
    textOutput("text1")
  )
}

app_ui_di_load <- function(id) {
  tabItem(
    tabName = id,
    h1("Load Dual Inlet Data"),
    fileSelectorUI("files", size = 12)
  )
}

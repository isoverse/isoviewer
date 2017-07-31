#' @keywords internal
"_PACKAGE"

#' @import isoreader
#' @import stringr
#' @import ggplot2
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @importFrom stats setNames
#' @importFrom utils packageVersion
#' @importFrom methods is
NULL

# quiets concerns of R CMD check about . that appears in pipelines
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c("."))

# release questions (elaborate once selenium testing is implemented)
# https://cran.r-project.org/web/packages/RSelenium/vignettes/shinytesting.html
release_questions <- function() {
  c(
    "Is it passing win-builder?"
  )
}

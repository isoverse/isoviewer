#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @importFrom rlang sym !!!
#' @importFrom tibble enframe deframe
#' @importFrom tidyr spread
#' @import ggplot2
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs enable disable disabled toggleState hidden hide show toggle addClass removeClass useShinyjs extendShinyjs alert
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyBS bsTooltip bsModal
#' @importFrom shinycssloaders withSpinner
#' @importFrom stats setNames
#' @importFrom utils packageVersion
#' @importFrom R.utils isAbsolutePath filePath
#' @importFrom methods is
NULL

#' @export
magrittr::`%>%`

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

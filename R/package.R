#' @keywords internal
"_PACKAGE"

#' @import isoreader
#' @import dplyr
#' @importFrom tibble enframe deframe
#' @importFrom tidyr spread
#' @import stringr
#' @import ggplot2
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs enable disable disabled toggleState hidden hide show toggle addClass removeClass useShinyjs extendShinyjs alert
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyBS bsTooltip bsModal
#' @importFrom shinycssloaders withSpinner
#' @importFrom DT dataTableOutput datatable dataTableAjax reloadData formatSignif dataTableProxy
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable rhandsontable hot_table hot_col hot_to_r
#' @importFrom stats setNames
#' @importFrom utils packageVersion
#' @importFrom R.utils isAbsolutePath filePath
#' @importFrom methods is
#' @importFrom UNF unf
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

# TODO:
# - allow logging to file in addition to console

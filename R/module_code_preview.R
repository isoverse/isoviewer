# Code previe module ====

#' JS Extension for code preview
#'
#' Generates the shinyJS extensions for updating the code preview. Must be included ONCE in the UI if this module is used (no matter how many times).
#' @family code preview module functions
codePreviewShinyjsExtension <- function() {

  js_code <- c(
# update code preview function that preserves cursor position, sroll position and line selection
# @params see R function in codePreviewServer for details
updateCodePreview =
"shinyjs.updateCodePreview = function(data) {
  var $el = $('#' + data.id);
  var editor = $el.data('aceEditor');
  var cursor = editor.selection.getCursor();
  var selection = editor.selection.getRange();
  var scroll_top = editor.session.getScrollTop();
  var scroll_left = editor.session.getScrollLeft();
  if (data.value) {
    editor.getSession().setValue(data.value, -1);
    editor.session.setScrollTop(scroll_top);
    editor.session.setScrollLeft(scroll_left);
    editor.selection.moveTo(cursor.row, cursor.column);
    editor.selection.setSelectionRange(selection);
  }
}",

# move to specific position in code preview
# @params see R function in codePreviewServer for details
focusCodePreview =
"shinyjs.focusCodePreview = function(data) {
  var $el = $('#' + data.id);
  var editor = $el.data('aceEditor');
  var center = (data.center ? true : false);
  var line = null;

  if (data.line) {
    line = data.line - 1;
  } else if (data.search) {
    var cs = (data.case_sensitive ? true : false);
    editor.$search.set({
      needle: data.search, caseSensitive: cs, range: null, regExp: false
    });
    var found = editor.$search.findAll(editor.getSession());
    if (found.length > 0) {
      line = found[0].start.row;
    }
  }

  if (line) {
    editor.clearSelection();
    editor.scrollToLine(line, center);
    editor.selection.moveTo(line, 0);
  }
}")

  tagList(
    extendShinyjs(text = str_c(js_code, collapse = "\n"), functions = names(js_code))
  )
}

#' Code Preview Server
#' @inheritParams fileSelectorServer
#' @param code_func_reac reactive function that returns a function(!) with parameters \code{rmarkdown} and \code{header} that can be called to generate the code
#' @param download_file reactive function that returns the download file name (with or without .Rmd ending)
#' @family code preview module functions
codePreviewServer <- function(input, output, session, code_func_reac, download_file) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    rmarkdown_view = FALSE
  )

  # update code preview
  # @param id the aceEditor id
  # @param value the new value for the aceEditor
  update_code_preview <- function(value) {
    shinyjs::js$updateCodePreview(id = ns("code_editor"), value = value)
  }

  # focus code preview
  # @param id the aceEditor id
  # @param center if TRUE, will center the focused line
  # @param line if provided, will move to this line
  # @param search if provided (but no line), will search for this term and move to the line
  # @param case_sensitive whether to search case sensitive
  focus_code_preview <- function(line = NULL, search = NULL, center = FALSE, case_sensitive = FALSE) {
    shinyjs::js$focusCodePreview(id = ns("code_editor"), line = line, search = search, center = center, case_sensitive = case_sensitive)
  }

  # update rmarkdown view style whenever link is toggled
  observeEvent(input$code_as_markdown, {
    values$rmarkdown_view <- !values$rmarkdown_view
    module_message(ns, "debug", "switching rmarkdown view ", if (values$rmarkdown_view) "on" else "off")
  })

  # update code whenever code_func changes
  observe({
    code_func <- code_func_reac()
    if(!is.function(code_func)) # safety check
      stop("code function must be a reactive function returning a function, found ", class(code_function), call. = FALSE)
    update_code_preview(code_func(rmarkdown = values$rmarkdown_view, header = FALSE))
  })

  # save/download RMarkdown
  output$code_download <- downloadHandler(
    filename = function() { download_file() %>% str_replace("\\.Rmd$", "") %>% str_c(".Rmd") },
    content = function(filename) {
      module_message(ns, "info", "preparing RMarkdown file for download")
      con <- file(filename)
      writeLines(code_func_reac()(rmarkdown = TRUE, header = TRUE), con)
      close(con)
    }
  )

  # return functions
  list(
    update_code_preview = update_code_preview,
    focus_code_preview = focus_code_preview
  )
}


#' Code Preview UI
#'
#' @param id the module id
#' @param width width of the box
#' @param height height of the code preview
#' @family code preview module functions
codePreviewUI <- function(id, width = 12, height = "400px") {
  ns <- NS(id)

  # code previes
  default_box(title = tagList(
    "Code Preview", spaces(1),
    tooltipInput(actionLink, ns("code_as_markdown"), NULL, icon = icon("commenting"), tooltip = "Toogle preview between 'code only' and 'RMarkdown' view"),
    spaces(1),
    tooltipOutput(downloadLink, ns("code_download"), label = icon("download"), tooltip = "Download code as RMarkdown")
  ), width = width,
  aceEditor(ns("code_editor"), "", mode = "r",
            theme="ambiance", readOnly = TRUE,
            height = height)
  )
}

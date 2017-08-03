# display module message
# @param type if this is an info meessage or debug (debug only shows if in debug mode)
module_message <- function(ns, type = c("info", "debug"), ...) {
  if (type == "debug" && !setting("debug")) return()
  prefix <- if(type == "info") "INFO: " else if (type == "debug") "DEBUG: " else stop("don't know message type", type)
  message(prefix, ..., " (NS: ", ns(NULL),")")
}

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n) {
  HTML(rep("&nbsp;", n))
}

# convenience function for inline inputs
inlineInput <- function(input, inputId, label, ..., tooltip = NULL) {
  tagList(
    inline(tags$label(label, `for` = inputId)),
    inline(do.call(input, args = c(list(inputId = inputId, label = NULL), list(...)))),
    if (!is.null(tooltip)) bsTooltip(inputId, tooltip)
  )
}

# inline item
inline <- function(...) {
  div(style = "display: inline-block;", ...)
}

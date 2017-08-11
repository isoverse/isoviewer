# function for filling code templates
# @param id of the template
# @param ... paramters for str_interp
fill_code_template <- function(id, ...) {

  templates <- c(
    # template for reading files
    read_files =
      str_c(
        "read_"
      )
  )
  stopifnot(id %in% names(templates))

  str_interp(templates[id], ...)
}

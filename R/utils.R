# utility functions

# function to wrap functio names for problem reporting
wrap_function_name <- function(string, max_length, divider = "_", newline = "\n") {
  pieces <- str_split(string, divider)
  nchars <- map(pieces, ~cumsum(nchar(.x)))
  cuts <- map(nchars, ~c(0, diff(floor(.x/max_length))))
  map2_chr(cuts, pieces, ~str_c(str_c(ifelse(.x == 1, newline, ""), .y), collapse = divider))
}

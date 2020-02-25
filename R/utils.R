# utility functions

# function to wrap functio names for problem reporting
wrap_function_name <- function(string, max_length, divider = "_", newline = "\n") {
  pieces <- stringr::str_split(string, divider)
  nchars <- purrr::map(pieces, ~cumsum(nchar(.x)))
  cuts <- purrr::map(nchars, ~c(0, diff(floor(.x/max_length))))
  purrr::map2_chr(cuts, pieces, ~stringr::str_c(stringr::str_c(ifelse(.x == 1, newline, ""), .y), collapse = divider))
}


# find iso objects in the global environment
find_iso_objects <- function() {

  iso_objs <- rlang::set_names(ls(envir = .GlobalEnv))
  iso_di <- purrr::map_lgl(iso_objs, ~isoreader::iso_is_dual_inlet(get(.x)))
  iso_cf <- purrr::map_lgl(iso_objs, ~isoreader::iso_is_continuous_flow(get(.x)))
  iso_scan <- purrr::map_lgl(iso_objs, ~isoreader::iso_is_scan(get(.x)))

  return(
    list(
      di = sort(names(iso_di[iso_di])),
      cf = sort(names(iso_cf[iso_cf])),
      scan = sort(names(iso_scan[iso_scan]))
    )
  )
}

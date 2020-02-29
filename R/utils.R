# utility functions

# function to wrap functio names for problem reporting
wrap_function_name <- function(string, max_length, divider = "_", newline = "\n") {
  pieces <- stringr::str_split(string, divider)
  nchars <- purrr::map(pieces, ~cumsum(nchar(.x)))
  cuts <- purrr::map(nchars, ~c(0, diff(floor(.x/max_length))))
  purrr::map2_chr(cuts, pieces, ~stringr::str_c(stringr::str_c(ifelse(.x == 1, newline, ""), .y), collapse = divider))
}

# calculate number of reasonable digits for an interval
n_interval_digits <- function(interval, diff_sigs = 2) {
  return(-floor(log10(diff(sort(interval)))) + diff_sigs)
}
# round interval depending on its range
round_interval_digits <- function(interval, diff_sigs = 2) {
  n_digits <- n_interval_digits(interval, diff_sigs = diff_sigs)
  round(interval, n_digits)
}


# find iso objects in the global environment
find_iso_objects <- function() {

  # all objects
  all_objs <-
    ls(envir = .GlobalEnv) %>%
    rlang::set_names() %>%
    purrr::map(get)
  iso_objs <- all_objs[purrr::map_lgl(all_objs, isoreader::iso_is_object)]

  # data frame of information
  tibble::tibble(
    type = purrr::map_chr(iso_objs, ~{
      if (isoreader::iso_is_continuous_flow(.x))  "continuous flow"
      else if (isoreader::iso_is_dual_inlet(.x)) "dual inlet"
      else if (isoreader::iso_is_scan(.x)) "scan"
      else NA_character_
    }),
    variable = names(iso_objs),
    n_files = purrr::map_int(iso_objs, length),
    size = purrr::map_chr(iso_objs, ~format(object.size(.x), unit = "auto"))
  )
}

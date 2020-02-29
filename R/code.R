# specific code assembly functions ===

# generate dataset variables
generate_dataset_vars <- function(dataset) {
  list(
    subset = paste0(dataset, "_subset")
  )
}

# generate processing code
generate_cf_processing_code <- function(scale_signal, scale_time, ratios = c(), rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Process raw data",
    chunk_options = list("process data"),
    pipe(
      "# process raw data\nisofiles <- isofiles",
      if(scale_signal != "<NONE>") code_block("iso_convert_signal", units = scale_signal),
      if(scale_time != "<NONE>") code_block("iso_convert_time", units = scale_time),
      if(length(ratios) > 0) code_block("iso_calculate_ratios", ratios = ratios)
    )
  )
}

# generate processing code
generate_di_processing_code <- function(scale_signal, ratios = c(), rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Process raw data",
    chunk_options = list("process data"),
    pipe(
      "# process raw data\nisofiles <- isofiles",
      if(scale_signal != "<NONE>") code_block("iso_convert_signal", units = scale_signal),
      if(length(ratios) > 0) code_block("iso_calculate_ratios", ratios = ratios)
    )
  )
}

# generate plot code
generate_plot_code <- function(data, plot_params, theme1 = NULL, theme2 = NULL, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Plot raw data",
    chunk_options = list("plot data", fig.width = 8, fig.height = 6),
    plus(
      code_block("iso_plot_raw_data", data = data, params = plot_params),
      if (!is.null(theme1)) code_block("plot_theme", theme = theme1),
      if (!is.null(theme2)) code_block("plot_theme", theme = theme2)
    )
  )
}

# generate di plot code
generate_di_plot_code <- function(dataset, scale_signal, data, aes_options = list(), theme_options = list(), rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Plot Raw Data",
    chunk_options = list("plot_raw_data", fig.width = 8, fig.height = 6),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "plot raw data"),
      if(scale_signal != "NULL")
        function_call(
          "iso_convert_signal",
          params = list(to = scale_signal),
          comment = "convert signal units"
        ),
      function_call(
        "iso_plot_dual_inlet_data",
        params = c(
          if(!identical(data, character(0))) list(data = data),
          aes_options
        ),
        comment = "plot dual inlet data",
        fixed_eq_op = "="
      )
    ) %>%
      plus(
        if (length(theme_options) > 0)
          function_call("ggplot2::theme", params = theme_options, comment = "customize the plot theme")
      )
  )
}

# generate cf plot code
generate_cf_plot_code <- function(dataset, scale_signal, scale_time, zoom, data, aes_options = list(), theme_options = list(), rmarkdown = FALSE) {

  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Plot Raw Data",
    chunk_options = list("plot_raw_data", fig.width = 8, fig.height = 6),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "plot raw data"),
      if(scale_signal != "NULL")
        function_call(
          "iso_convert_signal",
          params = list(to = scale_signal),
          comment = "convert signal units"
        ),
      if(scale_time != "seconds") # seconds is the default
        function_call(
          "iso_convert_time",
          params = list(to = scale_time),
          comment = "convert time units"
        ),
      function_call(
        "iso_plot_continuous_flow_data",
        params = c(
          if(!identical(data, character(0))) list(data = data),
          if(!is.null(zoom$x_min) && !is.null(zoom$x_max))
            list(time_interval = round_interval_digits(
              c(
                isoprocessor:::scale_time(zoom$x_min, to = scale_time, from = "seconds"),
                isoprocessor:::scale_time(zoom$x_max, to = scale_time, from = "seconds")
              ))),
          if(!is.null(zoom$zoom)) list(zoom = zoom$zoom),
          aes_options
        ),
        comment = "plot continuous flow data",
        fixed_eq_op = "="
      )
    ) %>%
      plus(
        if (length(theme_options) > 0)
          function_call("ggplot2::theme", params = theme_options, comment = "customize the plot theme")
      )
  )
}

# generate scan plot code
generate_scan_plot_code <- function(dataset, type, scale_signal, data, zoom, aes_options = list(), theme_options = list(), rmarkdown = FALSE) {

  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Plot Raw Data",
    chunk_options = list("plot_raw_data", fig.width = 8, fig.height = 6),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "plot raw data"),
      if(scale_signal != "NULL")
        function_call(
          "iso_convert_signal",
          params = list(to = scale_signal),
          comment = "convert signal units"
        ),
      function_call(
        "iso_plot_scan_data",
        params = c(
          list(type = type),
          if(!identical(data, character(0))) list(data = data),
          if(!is.null(zoom$x_min) && !is.null(zoom$x_max))
            list(x_interval = round_interval_digits(c(zoom$x_min, zoom$x_max))),
          if(!is.null(zoom$y_min) && !is.null(zoom$y_max))
            list(y_interval = round_interval_digits(c(zoom$y_min, zoom$y_max))),
          aes_options
        ),
        comment = "plot scan data",
        fixed_eq_op = "="
      )
    ) %>%
      plus(
        if (length(theme_options) > 0)
          function_call("ggplot2::theme", params = theme_options, comment = "customize the plot theme")
      )
  )
}

# generate export code
generate_export_code <- function(filepath, export_params, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Export data",
    code_block("iso_export_to_excel", filepath = filepath, params = export_params),
    code_block("iso_export_to_feather", filepath = filepath, params = export_params),
    chunk_options = list("export data")
  )
}

# generate file info code
generate_file_info_code <- function(dataset, selection, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# File Information",
    chunk_options = list("file info"),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "aggregate file info"),
      function_call(
        "iso_get_file_info",
        params = list(select = selection)
      )
    )
  )
}

# generate raw data code
generate_raw_data_code <- function(dataset, selection, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Raw Data",
    chunk_options = list("raw_data"),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "aggregate raw data"),
      function_call(
        "iso_get_raw_data",
        params = list(select = selection)
      )
    )
  )
}

# generate standards code
generate_standards_code <- function(dataset, selection, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Standards",
    chunk_options = list("standards"),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "aggregate standards info"),
      function_call(
        "iso_get_standards",
        params = list(select = selection)
      )
    )
  )
}

# generate vendor data table code
generate_vendor_data_table_code <- function(dataset, selection, explicit_units, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Vendor Data Table",
    chunk_options = list("vendor_data_table"),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "aggregate vendor data table"),
      function_call(
        "iso_get_vendor_data_table",
        params = list(select = selection)
      ),
      if (explicit_units) function_call("iso_make_units_explicit", comment = "make implicit units explicit")
    )
  )
}

# generate vendor data table code
generate_peak_table_code <- function(dataset, selection, explicit_units, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Peak Table",
    chunk_options = list("peak_table"),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "aggregate peak table"),
      function_call(
        "iso_get_peak_table",
        params = list(select = selection)
      ),
      if (explicit_units) function_call("iso_make_units_explicit", comment = "make implicit units explicit")
    )
  )
}

# generate resistors code
generate_resistors_code <- function(dataset, selection, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Resistors",
    chunk_options = list("resistors"),
    pipe(
      add_comment(generate_dataset_vars(dataset)$subset, "aggregate resistors info"),
      function_call(
        "iso_get_resistors",
        params = list(select = selection)
      )
    )
  )
}

# generate code for dataset and data files selection
generate_data_subset_code <- function(dataset, remove_errors, remove_warnings, select_files, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "# Subset Dataset",
    chunk_options = list("subset"),
    pipe(
      assign_call(generate_dataset_vars(dataset)$subset, dataset, comment = "subset dataset"),
      if (remove_errors || remove_warnings)
        function_call(
          "iso_filter_files_with_problems",
          params = list(remove_files_with_warnings = remove_warnings, remove_files_with_errors = remove_errors),
          comment = "remove problematic files"
        ),
      if (length(select_files) == 0 || !is.na(select_files[1]))
        function_call(
          "iso_filter_files",
          params = list(file_id = select_files),
          comment = "select specific files"
        )
    )
  )
}

# generate code for loading files/folder list
generate_load_list_code <- function(read_paths, read_func, read_params, save_file, save_folder,
                                    rmarkdown = FALSE) {

  code(
    chunk(
      code_only = !rmarkdown,
      pre_chunk = "## Read Files",
      code_block("file_paths", paths = read_paths),
      code_block("read_files", func = read_func, params = read_params),
      chunk_options = list("read files")
    ),
    chunk(
      code_only = !rmarkdown,
      pre_chunk = "#### Check for problems",
      code_block("show_problems"),
      chunk_options = list("problems")
    ),
    chunk(
      code_only = !rmarkdown,
      pre_chunk = "## Save Dataset",
      code_block("export_rds", save_file = save_file),
      chunk_options = list("save")
    )
  )
}

# generate code for file header and setup
# @param setup_addon additional lines to go into setup
generate_file_header_code <- function(
  title, dataset, read_func, rmarkdown = FALSE, front_matter = rmarkdown,
  install = front_matter, setup = TRUE, load = rmarkdown) {
  # generate header
  code(
    if (rmarkdown && front_matter) code_block("header", title = title),
    if (rmarkdown && install)
      chunk(
        code_only = !rmarkdown,
        chunk_options = list("install", echo=FALSE, eval=FALSE),
        function_call("install.packages", params = list("devtools"),
                      comment = "run once to install"),
        function_call("devtools::install_github", params = list("isoverse/isoreader")),
        function_call("devtools::install_github", params = list("isoverse/isoprocessor"))
      ),
    if (setup)
      chunk(
        code_only = !rmarkdown,
        pre_chunk = "This document was generated with [isoreader](http://isoreader.isoverse.org) version `r packageVersion(\"isoreader\")` and [isoprocessor](http://isoprocessor.isoverse.org) version `r packageVersion(\"isoprocessor\")`.\n\n# Libraries",
        chunk_options = list("setup", message=FALSE, warning=FALSE),
        # load libraries
        add_comment("library(isoreader)\nlibrary(isoprocessor)", "load libraries")
      ),
    if (load)
      chunk(
        code_only = !rmarkdown,
        pre_chunk = "# Load Data",
        chunk_options = list("load"),
        assign_call(
          "path", "\"\"",
          comment = "TODO: fill in the path to your data folder or file(s)"),
        assign_call(
          dataset, function_call(read_func, params = list(rlang::sym("path")))
        ) %>% add_comment("read in dataset")
      )
  )
}

# utility functions for code assembly ====

# function to assemble code chunk
# @param code_only to turn rmarkdown on/off easily
chunk <- function(..., pre_chunk = NULL, post_chunk = NULL, chunk_options = list(), code_only = FALSE) {
  content <- stringr::str_c(..., sep = "\n\n")
  if (code_only) return(content)
  stringr::str_c(
    c(
      if(!is.null(pre_chunk)) sprintf("%s\n", pre_chunk),
      code_block("chunk", chunk_options = chunk_options, content = stringr::str_c(..., sep = "\n")),
      if(!is.null(post_chunk)) sprintf("\n%s\n", post_chunk)
    ), collapse = "\n")
}

# function to assemble pipe
pipe <- function(...) {
  blocks <-
    list(...) %>%
    # remove NULL items
    { .[!purrr::map_lgl(., is.null)] } %>%
    # add indentation to all but first item
    { c(.[1], purrr::map_chr(.[-1], indent_by, 1)) }
  paste(unlist(blocks), collapse = " %>%\n")
}

# function to assemple plusses
plus <- function(...) {
  blocks <-
    list(...) %>%
    # remove NULL items
    { .[!sapply(., is.null)] } %>%
    # add indentation to all but first item
    { c(.[1], sapply(.[-1], indent)) }
  stringr::str_c(unlist(blocks), collapse = " +\n")
}

# function to indent a code block (with each newline)
# @deprecated
indent <- function(block, spaces = "  ") {
  if (length(block) == 0) return(NULL)
  stringr::str_replace_all(stringr::str_c(spaces, block), "\n", stringr::str_c("\n", spaces))
}


# function to assemble character vector
# @deprecated
char_vector <- function(values, spacer = "\n    ") {
  if (length(values) == 0) return("c()")
  spacer <- paste0("\",", spacer, "\"")
  paste0("c(\"", paste0(values, collapse = spacer), "\")")
}

# function to indent by 1 level
indent_by <- function(block, n) {
  if (length(block) == 0) return(NULL)
  spaces <- paste(rep("  ", n), collapse = "")
  stringr::str_replace_all(paste0(spaces, block), "\n", paste0("\n", spaces))
}

# function to add comment
add_comment <- function(code, comment = NULL) {
  if (!is.null(comment)) return(sprintf("# %s\n%s", comment, code))
  else return(code)
}

# function to generate a function call
function_call <- function(func, params = list(), comment = NULL, fixed_eq_op = NULL) {
  if (length(params) == 0) {
    # no parameters
    code <- sprintf("%s()", func)
  } else if (length(params) == 1) {
    # 1 parameter
    if (is.null(names(params)))
      param <- function_parameter(NULL, params[[1]], fixed_eq_op = fixed_eq_op)
    else
      param <- function_parameter(names(params)[1], params[[1]], fixed_eq_op = fixed_eq_op)
    if (!stringr::str_detect(param, "\\n"))
      code <- sprintf("%s(%s)", func, param)
    else
      code <- sprintf("%s(\n%s\n)", func, indent_by(param, 1))
  } else {
    # with multiple parameters
    params <- purrr::map2_chr(names(params), params, function_parameter, fixed_eq_op = fixed_eq_op) %>%
      indent_by(1)
    code <- sprintf("%s(\n%s\n)", func, paste(params, collapse = ",\n"))
  }

  return(add_comment(code, comment))
}

# generate parameter
function_parameter <- function(param, value, nchar_cutoff = 60L, fixed_eq_op = NULL) {
  # value code
  is_symbol <- FALSE
  if (is.list(value) && rlang::is_expression(value[[1]])) {
    is_symbol <- TRUE
    value_code <- purrr::map_chr(value, rlang::expr_text)
  } else if (rlang::is_expression(value)) {
    value_code <- rlang::expr_text(value)
  } else if (is.character(value)) {
    value_code <- sprintf("\"%s\"", value)
  } else if (is.logical(value)) {
    value_code <- ifelse(value, "TRUE", "FALSE")
  } else if (is.numeric(value)) {
    value_code <- as.character(value)
  } else if (is.null(value)) {
    value_code <- "NULL"
  } else {
    # don't know what to do
    stop("unknown value type: ", class(value[[1]])[1], call. = FALSE)
  }

  # equivalence operator
  if(!is.null(fixed_eq_op)) op <- fixed_eq_op
  else if (length(value_code) > 1 && !is_symbol) op <- "%in%"
  else op <- "="

  # parameter code
  if (length(value_code) == 1L && is.null(param)) {
    # single value, no parameter name
    return(value_code)
  } else if (length(value_code) == 1L && !is.null(param)) {
    # single value with parameter name
    return(sprintf("%s %s %s", param, op, value_code))
  } else {
    # multi value
    param_code <- sprintf("%s %s c(%s)", param, op, paste(value_code, collapse = ", "))
    if (nchar(param_code) > nchar_cutoff) {
      value_code <- sprintf("c(%s)", paste(value_code, collapse = ",\n  "))
      param_code <- sprintf("%s %s \n%s", param, op, indent_by(value_code, 1))
    }
    return(param_code)
  }
}

# assign call
assign_call <- function(left, right, comment = NULL) {
  return(add_comment(sprintf("%s <- %s", left, right), comment))
}


# combine multiple chunks
code <- function(...) {
  codes <- list(...)
  codes <- codes[!purrr::map_lgl(codes, is.null)]
  do.call(paste, args = c(codes, list(sep = "\n\n")))
}

# function for filling code block templates
# @param id of the template
# @param ... paramters for stringr::str_interp
code_block <- function(id, ...) {

  templates <- c(

#### processing ####

# convert signal ----
iso_convert_signal =
"# convert signal
iso_convert_signals(to = \"${units}\")",

# convert time ----
iso_convert_time =
"# convert time
iso_convert_time(to = \"${units}\")",


# calculate ratios ----
iso_calculate_ratios =
"# calculate_ratios
iso_calculate_ratios(${ if (!is.null(ratios)) isoviewer:::char_vector(ratios, spacer = ' ') else NA})",


#### plotting ####

# plot raw data ---
iso_plot_raw_data =
"# plot raw data
library(ggplot2)
iso_plot_raw_data(isofiles,
  data = ${ if (!is.null(data)) isoviewer:::char_vector(data, spacer = ' ') else NA },
  ${paste0(paste0(names(params), ' = ', params), collapse = ',\n  ')})",

# plot theme ---
plot_theme =
"# add plot styling
theme(${theme})",

#### data selection ####

# load dataset (deprecated) ----
load_dataset =
"# load dataset
iso_files <- ${func}(${if (!is.null(dataset)) paste0('\"', dataset, '\"') else NA})",

# subset dataset ----
subset_dataset =
"# subset dataset
${dataset}_subset <- ${dataset}",


omit_problems =
"# remove problematic files
iso_filter_files_with_problems(\n  ${paste0(paste0(names(params), ' = ', params), collapse = ',\n  ')}\n  )",

# filter files ----
select_files =
"# select specific files
iso_filter_files(\n  file_id %in% ${ isoviewer:::char_vector(files, spacer = '\n                  ') }\n  )",

#### file info ----
iso_get_file_info =
"# aggregate file info
isofiles %>% iso_get_file_info(\n  select = ${ isoviewer:::char_vector(selection, spacer = ' ')})",

#### method info ----
iso_get_standards_info =
"# aggregate standards method info
isofiles %>% iso_get_standards_info()",

iso_get_resistors_info =
"# aggregate resistors method info
isofiles %>% iso_get_resistors_info()",

#### vendor data table -----
iso_get_vendor_data_table =
"# aggregate vendor data table
isofiles %>% iso_get_vendor_data_table(\n  select=${ isoviewer:::char_vector(selection, spacer = ' ')})",

#### export
# export to excel ----
iso_export_to_excel =
"# export to excel
isofiles %>% iso_export_to_excel(${if (is.null(filepath)) NA else paste0('\"', filepath, '\"')},
  ${paste0(paste0(names(params), ' = ', params), collapse = ',\n  ')})",

# export to feather ----
iso_export_to_feather =
  "# export to feather
isofiles %>% iso_export_to_feather(${if (is.null(filepath)) NA else paste0('\"', filepath, '\"')},
  ${paste0(paste0(names(params), ' = ', params), collapse = ',\n  ')})",

#### file/folder loading ####

# file paths ----
file_paths =
"# specify data files and folders
data_dir <- \".\"
data_paths <- ${if (length(paths) == 0) NA else paste0('file.path(\n  data_dir,\n  ', isoviewer:::char_vector(paths), ')')}",

# read files ----
read_files =
"# load isofiles
isofiles <- ${func}(
  paths = data_paths,
  ${paste0(paste0(names(params), ' = ', params), collapse = ',\n  ')})",

# look at problems ----
show_problems =
"# show problems
isofiles %>% iso_get_problems()",

# export rds ----
export_rds =
"# save dataset
isofiles %>% iso_save(\"${save_file}\")",

#### general purpose ####

# load library ----
load_library =
  "# load libraries
library(isoreader)
library(isoprocessor)",

# caching on ----
caching_on =
  "# turn automatic data caching on
iso_turn_reader_caching_on()",

# rmarkdown chunk ----
chunk =
"```{r ${isoviewer:::chunk_options(chunk_options)}}
${content}
```",

# knit kable pipe ---
kable =
"# format table
knitr::kable()",

# rmarkdown header ----
header =
"---
title: \"${title}\"
date: \"`r Sys.Date()`\"
output:
  html_document:
    toc: yes
    toc_float: true
    code_folding: show
    df_print: paged
---",

# install_github ---
install_github =
"${isoviewer:::code_block('chunk', content = paste0('# run once to install\ndevtools::install_github(\"', package, '\")'), chunk_options = list('install', echo = FALSE, eval = FALSE))}"
)

  # fill template
  if(!id %in% names(templates)) stop("missing template: ", id, call. = FALSE)
  stringr::str_interp(templates[id], list(...))
}

# function to assemble chunk options
chunk_options <- function (options) {
  if (length(options) == 0) return("")
  values <- options %>% sapply(function(i) {
    if (is.character(i)) paste0("\"",i,"\"")
    else paste0(i)
  })
  if (length(values) == 1)
    return(values[[1]])
  else
    c(values[[1]], tail(values, -1) %>% { paste0(names(.), "=", unlist(.)) }) %>%
    paste(collapse = ", ")
}



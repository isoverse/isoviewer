# specific code assembly functions ===

# generate export code
generate_export_code <- function(filepath, export_params, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Export data",
    code_block("export_to_excel", filepath = filepath, params = export_params),
    code_block("export_to_feather", filepath = filepath, params = export_params),
    chunk_options = list("export data")
  )
}

# generate vendor data table code
generate_vendor_data_table_code <- function(selection, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Show Vendor Data Table",
    chunk_options = list("vendor data table"),
    pipe(
      code_block("aggregate_vendor_data_table", selection = selection),
      if(rmarkdown) code_block("kable")
    )
  )
}

# generate methods info code
generate_method_info_code <- function(rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Show Method Information",
    chunk_options = list("method info"),
    pipe(
      code_block("aggregate_standards_info"),
      if(rmarkdown) code_block("kable")
    ),
    pipe(
      code_block("aggregate_resistors_info"),
      if(rmarkdown) code_block("kable")
    )
  )
}

# generate file info code
generate_file_info_code <- function(selection, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Show File Information",
    chunk_options = list("file info"),
    pipe(
      code_block("aggregate_file_info", selection = selection),
      if(rmarkdown) code_block("kable")
    )
  )
}

# generate code for dataset and data files selection
generate_data_selection_code <- function(dataset, read_func, omit_type, select_files, rmarkdown = FALSE) {
  chunk(
    code_only = !rmarkdown,
    pre_chunk = "## Load Dataset",
    chunk_options = list("load"),
    pipe(
      code_block("load_dataset", func = read_func, dataset = dataset),
      if (length(omit_type) >0)
        code_block("omit_problems", type = omit_type),
      if (length(select_files) == 0 || !is.na(select_files[1]))
        code_block("select_files", files = select_files)
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
      code_block("export_rda", save_file = save_file),
      chunk_options = list("save")
    )
  )
}

# generate code for file header and setup
# @param setup_addon additional lines to go into setup
generate_file_header_code <- function(title, rmarkdown = FALSE, front_matter = rmarkdown, install = front_matter,
                                      setup = TRUE, caching_on = FALSE) {
  # generate header
  code(
    if (rmarkdown && front_matter) code_block("header", title = title),
    if (rmarkdown && install) code_block("install_github", package = "KopfLab/isoreader"),
    if (setup) chunk(
      code_only = !rmarkdown,
      pre_chunk = "## Setup",
      post_chunk = "This document was generated with isoreader version `r packageVersion(\"isoreader\")`.",
      chunk_options = list("setup", message=FALSE, warning=FALSE),
      code_block("load_library"),
      if (caching_on) code_block("caching_on")
    )
  )
}

# utility functions for code assembly ====

# function to assemble code chunk
# @param code_only to turn rmarkdown on/off easily
chunk <- function(..., pre_chunk = NULL, post_chunk = NULL, chunk_options = list(), code_only = FALSE) {
  content <- str_c(..., sep = "\n\n")
  if (code_only) return(content)
  str_c(
    c(
      if(!is.null(pre_chunk)) sprintf("%s\n", pre_chunk),
      code_block("chunk", chunk_options = chunk_options, content = str_c(..., sep = "\n")),
      if(!is.null(post_chunk)) sprintf("\n%s\n", post_chunk)
    ), collapse = "\n")
}

# function to assemble pipe
pipe <- function(...) {
  blocks <-
    list(...) %>%
    # remove NULL items
    { .[!sapply(., is.null)] } %>%
    # add indentation to all but first item
    { c(.[1], sapply(.[-1], indent)) }
  str_c(unlist(blocks), collapse = " %>%\n")
}

# function to indent a code block (with each newline)
indent <- function(block, spaces = "  ") {
  if (length(block) == 0) return(NULL)
  str_replace_all(str_c(spaces, block), "\n", str_c("\n", spaces))
}

# function to assemble character vector
char_vector <- function(values, spacer = "\n    ") {
  if (length(values) == 0) return("c()")
  else paste0("c(\"", paste0(values, collapse = paste0("\",", spacer, "\"")), "\")")
}

# combine multiple chunks
code <- function(...) {
  str_c(..., sep = "\n\n")
}

# function for filling code block templates
# @param id of the template
# @param ... paramters for str_interp
code_block <- function(id, ...) {

  templates <- c(



#### data selection ####

# load dataset ---
load_dataset =
"# load dataset
isofiles <- ${func}(${if (!is.null(dataset)) paste0('\"', dataset, '\"') else NA})",

omit_problems =
"# omit problems
omit_files_with_problems(type = \"${type}\")",

select_files =
"# select specific files
filter_files(file_id %in%\n    ${ isoviewer:::char_vector(files, spacer = '\n      ') })",

#### file info
aggregate_file_info =
"# aggregate file info
aggregate_file_info(isofiles,\n  select=${ isoviewer:::char_vector(selection, spacer = ' ')})",

#### method info
aggregate_standards_info =
"# aggregate standards method info
aggregate_standards_info(isofiles)",

aggregate_resistors_info =
"# aggregate resistors method info
aggregate_resistors_info(isofiles)",

#### vendor data table
aggregate_vendor_data_table =
"# aggregate vendor data table
aggregate_vendor_data_table(isofiles,\n  select=${ isoviewer:::char_vector(selection, spacer = ' ')})",

#### export
# export to excel ----
export_to_excel =
"# export to excel
export_to_excel(isofiles, ${if (is.null(filepath)) NA else paste0('\"', filepath, '\"')},
  ${paste0(paste0(names(params), ' = ', params), collapse = ',\n  ')})",

# export to feather ----
export_to_feather =
  "# export to feather
export_to_feather(isofiles, ${if (is.null(filepath)) NA else paste0('\"', filepath, '\"')},
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
problems(isofiles) %>% knitr::kable()",

# export rda ----
export_rda =
"# save dataset
export_to_rda(isofiles, \"${save_file}\")",

#### general purpose ####

# load library ----
load_library =
  "# load library
library(isoreader)",

# caching on ----
caching_on =
  "# turn automatic data caching on
turn_caching_on()",

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
output: html_document
---",

# install_github ---
install_github =
"${isoviewer:::code_block('chunk', content = paste0('# run once to install\ndevtools::install_github(\"', package, '\")'), chunk_options = list('install', echo = FALSE, eval = FALSE))}"
)

  # fill template
  stopifnot(id %in% names(templates))
  str_interp(templates[id], list(...))
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



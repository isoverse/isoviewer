# specific code assembly functions ===

generate_load_list_code <- function(read_paths, read_func, read_params, save_file, save_folder,
                                    rmarkdown = FALSE, header = rmarkdown, install = header) {

  code(
    if (rmarkdown && header) code_block("header", title = str_c("Loading ", save_file)),
    if (rmarkdown && install) code_block("install_github", package = "KopfLab/isoreader"),
    chunk(
      code_only = !rmarkdown,
      pre_chunk = "## Setup",
      code_block("load_library"),
      options = list("setup", message=FALSE, warning=FALSE)),
    chunk(
      code_only = !rmarkdown,
      pre_chunk = "## Read Files",
      code_block("file_paths", paths = read_paths),
      code_block("read_files", func = read_func, params = read_params),
      options = list("read files")
    ),
    chunk(
      code_only = !rmarkdown,
      pre_chunk = "## Save Collection",
      code_block("export_rda", save_file = save_file, save_folder = save_folder),
      options = list("save")
    )
  )

}



# utility functions for code assembly ====

# function to assemble code chunk
# @param code_only to turn rmarkdown on/off easily
chunk <- function(..., pre_chunk = NULL, post_chunk = NULL, options = list(), code_only = FALSE) {
  content <- str_c(..., sep = "\n\n")
  if (code_only) return(content)
  str_c(
    c(
      if(!is.null(pre_chunk)) sprintf("%s\n", pre_chunk),
      code_block("chunk", options = options, content = str_c(..., sep = "\n")),
      if(!is.null(post_chunk)) sprintf("\n%s\n", post_chunk)
    ), collapse = "\n")
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

# load library ----
load_library =
"# load library
library(isoreader)",

# read files ----
file_paths =
"# specify data files and folders
data_dir <- \".\"
data_paths <- ${if (length(paths) == 0) NA else paste0('file.path(\n  data_dir,\n  c(\"', paste0(paths, collapse = '\",\n    \"'), '\"))')}",

# read files ----
read_files =
"# load isofiles
isofiles <- ${func}(
  paths = data_paths,
  ${paste0(paste0(names(params), ' = ', params), collapse = ',\n  ')})",

# export rda ----
export_rda =
"# save data collection
export_to_rda(
  isofiles,
  filepath = file.path(
    data_dir, \"${save_folder}\", \"${save_file}\"))",

# rmarkdown chunk ----
chunk =
"```{r ${isoviewer:::chunk_options(options)}}
${content}
```",

# rmarkdown header ----
header =
"---
title: \"${title}\"
date: \"`r Sys.Date()`\"
output: html_document
---",

# install_githb ---
install_github =
"${isoviewer:::code_block('chunk', content = paste0('# run once to install\ndevtools::install_github(\"', package, '\")'), options = list('install', echo = FALSE, eval = FALSE))}"
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



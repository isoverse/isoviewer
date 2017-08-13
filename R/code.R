# function for filling code templates
# @param id of the template
# @param ... paramters for str_interp
fill_code_template <- function(id, ...) {

  templates <- c(
    # template for reading files
    read_files =
      str_c(
        "# load library\n",
        "library(isoreader)\n\n",
        "# specify data files and folders\n",
        "data_dir <- \".\"\n",
        "data_paths <- ${if (length(paths) == 0) NA else paste0('file.path(\n\tdata_dir,\n\tc(\"', paste0(paths, collapse = '\",\n\t  \"'), '\"))')}",
        "\n\n",
        "# load isofiles\n",
        "isofiles <- ${func}(\n",
        "\tpaths = data_paths,\n",
        "\t${paste0(paste0(names(params), ' = ', params), collapse = ',\n\t')})\n\n",
        "# save data collection\n",
        "export_to_rda(isofiles,\n\tfilename = \"${save_file}\",\n\tfolder = \"${save_folder}\")\n"
      )
  )
  stopifnot(id %in% names(templates))

  str_interp(templates[id], list(...))
}

# NOTE: deprecated?
storage_deprecated <- function() {

# storage objects =====

#' Local file system storage
#'
#' This creates a storage object for local data storage for the isoviewer GUI.
#' Only files and folders under hte \code{root} directory will be accessible by isoviewer.
#'
#' The \code{\link{run}} function creates local storage automatically if only provided with a path.
#' @param root the local directory (relative to working directory or absolute) that will serve as the root directory for data storage
#' @param root_name the name of the root directory that should be shonw in isoviewer
#' @family data storage functions

local_storage <- function(root, root_name = "Data") {
  if (missing(root)) stop("no data directory provided", call. = FALSE)
  structure(
    list(
      initialized = FALSE,
      root = root, # local folder
      root_name # root name
    ),
    class = c("local", "isoviewer_storage")
  )
}

#' Create Google Drive remote storage
#'
#' This creates a google drive storage object as a remote data directory for the isoviewer GUI.
#' Only files and folders under the \code{root} directory will be accessible by isoviewer.
#'
#' @param root the google drive path that will serve as the root directory for data storage
#' @param oauth_token the file name for the authentication token (will be stored in the cache folder)
#' @param local_dir the local directory where temporary files from the remote should be stored
#' @family data storage functions

gdrive_storage <- function(root, root_name = "Data", oauth_token = "gdrive_token.rds", local_dir = tempdir()) {
  if (missing(root)) stop("no remote data directory provided", call. = FALSE)
  structure(
    list(
      initialized = FALSE,
      root = root, # google drive path to root
      root_name = root_name, # root name
      root_obj = NULL, # the actual google drive root entry
      local_dir = local_dir, # local directory for backup
      oauth_token = file.path(isoreader:::default("cache_dir"), oauth_token), # token path
      token = NULL # the actual token
    ),
    class = c("gdrive", "isoviewer_storage")
  )
}

#' Initialize storage
#'
#' Initializes a storage object and checks that all necessary information for it to work properly is provided.
#' Throws errors if anything is amiss. Can be called explicitly (or implictly by calling any other operation to
#' work with the storage). Will only do the initializing once and afterwards just return the storage object.
#'
#' @param storage the storage object
#' @param save_credentials if login credentials are required - should they be stored for next time?
#' @family data storage functions

init_storage <- function(storage, save_credentials = TRUE) {

  if (!is_storage(storage)) stop("not a storage object", call. = FALSE)
  if (storage$initialized) return(storage) # already initialized

  # init checks
  if (is_local_storage(storage)) {
    # local storage
    if (!dir.exists(storage$root))
      stop("directory for local storage does not exist: ", storage$root, call. = FALSE)
  } else if (is_gdrive_storage(storage)) {
    # gdrive storage
    if (!dir.exists(storage$local_dir))
      stop("directory for local copies does not exist: ", storage$local_dir, call. = FALSE)

    # authenticate
    if (file.exists(storage$oauth_token)) {
      message("Info: authenticating google drive storage with token")
      storage$token <- drive_auth(oauth_token = storage$oauth_token)
    } else {
      message("Info: authenticating for the first time via browser")
      storage$token <- drive_auth(cache = FALSE)
    }

    # save credentials
    if (save_credentials && !file.exists(storage$oauth_token)) {
      storage <- save_credentials(storage)
    }

    # check that root exists in the drive
    message("Info: retrieving remote root")
    root <- drive_get(storage$root)
    if (nrow(root) == 0) {
      stop("could not find path in google drive: ", storage$root, call. = FALSE)
    } else if (nrow(root) > 1) {
      stop("multiple paths found in google drive: ", stringr::str_c(root$path, collapse = ", "), call. = FALSE)
    }
    storage$root_obj <- root

  } else {
    stop("unknown storage object", call. = FALSE)
  }
  storage$initialized <- TRUE
  return(storage)
}

# save token
# @param base_dir base directory for where to store the credentials
save_credentials <- function(storage, base_dir = ".") {
  if (is_gdrive_storage(storage)) {
    path <- file.path(base_dir, storage$oauth_token)
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    message("Info: storing google drive access token in ", path)
    saveRDS(storage$token, file = path)
  }
  return(storage)
}

# check for isoviewe storage object
is_storage <- function(x) {
  is(x, "isoviewer_storage")
}

is_local_storage <- function(x) {
  is_storage(x) & is(x, "local")
}

is_gdrive_storage <- function(x) {
  is_storage(x) & is(x, "gdrive")
}

# recent files =====

#' Refresh recent files
#' @return storage object with recent_files updated
#' @param storage local storage object
#' @param file_pattern what types of files to return
#' @param n_max how many recent files to fetch
#' @param verbose whether to provide information messages or not
#' @family data storage functions

refresh_recent_files <- function(storage, file_pattern, n_max, verbose = FALSE) {
  storage <- init_storage(storage)
  if (missing(file_pattern)) stop("no filename pattern supplied", call. = FALSE)
  if (missing(n_max)) stop("number of recent files not supplied", call. = FALSE)

  if (is_local_storage(storage)) {
    storage$recent_files <- fetch_local_recent_files(storage, file_pattern = file_pattern, n_max = n_max, verbose = verbose)
  } else if (is_gdrive_storage(storage)) {
    if (is.null(storage$directory_tree)) storage <- refresh_directory_tree(verbose = verbose)
    storage$recent_files <- fetch_gdrive_recent_files(storage, file_pattern = file_pattern, n_max = n_max, verbose = verbose)
  } else {
    stop("unknown storage object", call. = FALSE)
  }
  return(storage)
}

#' Get the recent files
#' Get recent files from the storage object
#' @family data storage functions

get_recent_files <- function(storage) {
  if(!is_storage(storage)) stop("not a storage object", call. = FALSE)
  return(storage$recent_files)
}

# directory tree =====

#' Refresh directory tree
#' @return storage object with directory_tree updated
#' @family data storage functions

refresh_directory_tree <- function(storage, verbose = FALSE) {
  storage <- init_storage(storage)

  if (is_local_storage(storage)) {
    storage$directory_tree <- fetch_local_directory_tree(storage)
  } else if (is_gdrive_storage(storage)) {
    storage$directory_tree <- fetch_gdrive_directory_tree(storage, verbose = verbose)
  } else {
    stop("unknown storage object", call. = FALSE)
  }
  return(storage)
}

#' Get the directory tree
#' Get directory tree from the storage object
#' @family data storage functions

get_directory_tree <- function(storage) {
  if(!is_storage(storage)) stop("not a storage object", call. = FALSE)
  return(storage$directory_tree)
}

# select folder ====

#' Select a current folder in the directory tree
#' @param folder which folder to select

select_current_folder <- function(storage, folder) {
  storage <- init_storage(storage)
  if (is_local_storage(storage)) {
    storage$current_folder <- select_local_current_folder(storage, folder)
    storage$current_folder_files <- fetch_local_folder_files(storage)
    storage$current_folder_folders <- fetch_local_folder_folders(storage)
  } else if (is_gdrive_storage(storage)) {
    storage$current_folder <- select_gdrive_current_folder(storage, folder)
  } else {
    stop("unknown storage object", call. = FALSE)
  }
  return(storage)

  # while (tmp_path != dirname(root)){
  #   if (tmp_path == root)
  #     parent <- root_name
  #   else
  #     parent <- basename(tmp_path)
  #   parents <- c(parents, list(tabPanel(parent, value = tmp_path)))
  #   tmp_path <- dirname(tmp_path)
  # }

}


# local operations ====

# Fetch local directory tree
fetch_local_directory_tree <- function(storage) {
  stopifnot(is_local_storage(storage))
  tibble::tibble(
    path = list.dirs(storage$root),
    name = basename(path),
    parent = dirname(path) %>% stringr::str_replace(fixed(storage$root), "") %>% stringr::str_replace("^[\\/]?", "")) %>%
    dplyr::filter(path != storage$root) %>%
    dplyr::select(parent, name) %>%
    arrange(parent)
}

# Fetch recent files
fetch_local_recent_files <- function(storage, file_pattern = NULL, n_max = 20, verbose = TRUE) {
  stopifnot(is_local_storage(storage))
  if (verbose) message("Info: retrieving ", n_max, " most recent local files")
  recent <- list.files(storage$root, pattern = file_pattern, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

  # see if there are any
  if (length(recent) == 0) return(NULL)

  recent %>%
    lapply(function(file) list(filepath = file, mtime = file.mtime(file))) %>%
    bind_rows() %>%
    arrange(desc(mtime), filepath) %>%
    dplyr::mutate(
      name = basename(filepath),
      parent = dirname(filepath) %>% stringr::str_replace(fixed(storage$root), "") %>% stringr::str_replace("^[\\/]?", "")) %>%
    dplyr::filter(row_number() <= n_max) %>%
    dplyr::select(parent, name, mtime)
}


# gdrive operations ====

# Fetch files inside a gdrive folder
# @param directories data frame with the directory information
# @param folder_id the id of the folder to retrieve
# @param file_pattern the regexp pattern for which files to keep
fetch_gdrive_folder_files <- function(directories, folder_id, file_pattern = NULL, verbose = TRUE) {

  # get directory information
  directory <- dplyr::filter(directories, id == folder_id)
  if (nrow(directory) == 0) stop("could not find folder with id ", folder_id, call. = FALSE)
  if (nrow(directory) > 1) stop("more than one directory seems to have this id ", folder_id, call. = FALSE)
  if(verbose) message("Info: retrieving files for gdrive folder ", directory$name, " (", directory$folder, ")")

  # find files
  drive_ls(as_id(folder_id), pattern = file_pattern, verbose = verbose) %>%
    dplyr::mutate(
      folder = if(directory$root) "" else stringr::str_c(directory$folder, "/", directory$name),
      parent_id = folder_id,
      mtime = as.POSIXct(purrr::map_chr(drive_resource, "modifiedTime"), format = "%Y-%m-%dT%H:%M:%OS")
    ) %>%
    dplyr::mutate(folder = stringr::str_replace(folder, "^/", "")) # remove //
}

# Fetch folders inside a gdrive folder
# @param directories data frame with the directory information
# @param folder_id the id of the folder to retrieve
fetch_gdrive_folder_folders <- function(directories, folder_id, verbose = TRUE) {

  # get directory information
  directory <- dplyr::filter(directories, id == folder_id)
  if (nrow(directory) == 0) stop("could not find folder with id ", folder_id, call. = FALSE)
  if (nrow(directory) > 1) stop("more than one directory seems to have this id ", folder_id, call. = FALSE)
  if(verbose) message("Info: retrieving folders for gdrive folder ", directory$name, " (", directory$folder, ")")

  # find files
  drive_ls(as_id(folder_id), type = "folder", verbose = verbose) %>%
    dplyr::mutate(
      folder = if(directory$root) "" else stringr::str_c(directory$folder, "/", directory$name),
      parent_id = folder_id,
      mtime = as.POSIXct(purrr::map_chr(drive_resource, "modifiedTime"), format = "%Y-%m-%dT%H:%M:%OS")
    ) %>%
    dplyr::mutate(folder = stringr::str_replace(folder, "^/", "")) # remove //
}

# Get directory tree
fetch_gdrive_directory_tree <- function(storage, verbose = TRUE) {
  stopifnot(is_gdrive_storage(storage))
  storage$root_obj %>%
    dplyr::mutate(root = TRUE, level = 0, name = storage$root_name, parent = "") %>%
    dplyr::select(-path) %>%
    fetch_gdrive_subfolders(verbose = verbose) %>%
    dplyr::select(parent, everything())
}

# Fetch subfolders inside gdrive parent folders
# This function is ideal for traversing the directory tree recursively.
# @param parents (can be a single parent or multiple that are at the same level)
# @param level what level the traversing tree is at
# @param chain_limit how many or conditions allowed to chain for google drive request
fetch_gdrive_subfolders <- function(parents, level = 1, chain_limit = 50, verbose = TRUE, recursive = TRUE) {

  if (verbose) message("Info: retrieving gdrive directory depth level ", level)

  # function to find which of the immediate parents is listed for each subfolder
  parent_ids <- parents$id
  find_parent_id <- function(drive_resource) {
    parent_ids <- parent_ids[parent_ids %in% unlist(drive_resource$parents)]
    if (length(parent_ids) == 0) return(NA_character_)
    return (parent_ids[1]) # just go with the first of the eligible parents if there are more
  }

  # find subfolders
  subdirs <- parents %>%
    dplyr::mutate(n = 1:n(), group = floor(n / chain_limit)) %>%
    dplyr::group_by(group) %>%
    do({
      query <-
        sprintf("'%s' in parents", .$id) %>% stringr::str_c(collapse = " or ") %>%
        { sprintf("(%s) and mimeType='%s'", ., "application/vnd.google-apps.folder") }
      drive_find(q = query, verbose = verbose)
    }) %>% ungroup() %>%
    dplyr::mutate(
      level = level,
      parent_id = purrr::map_chr(drive_resource, find_parent_id)
    ) %>%
    dplyr::left_join(dplyr::select(parents, parent_id = id, parent_name = name, parent_folder = parent, parent_root = root), by = "parent_id") %>%
    dplyr::mutate(parent = ifelse(parent_root, "", stringr::str_c(parent_folder, "/", parent_name))) %>%
    dplyr::select(-group, -parent_name, -parent_folder, -parent_root) %>%
    dplyr::mutate(root = FALSE, parent = stringr::str_replace(parent, "^/", ""))

  # recursive call
  if (nrow(subdirs) > 0 && recursive)
    return(bind_rows(parents, fetch_gdrive_subfolders(subdirs, level + 1, chain_limit = chain_limit, verbose = verbose)))
  else
    return(bind_rows(parents, subdirs))
}

# Fetches the most recently added files
# This is a separate function because loading all files from the entire directory tree is too slow once file trees get large.
# @note I tested it and the n_max limit works with the file pattern restrictor (i.e. that number of files will be returned)
# @param storage the storage object
# @param file_pattern the regexp pattern for which files to keep
# @param n_max how many most recent files to fetch
fetch_gdrive_recent_files <- function(storage, file_pattern = NULL, n_max = 20, chain_limit = 50, verbose = TRUE) {

  stopifnot(is_gdrive_storage(storage))
  if (is.null(storage$directory_tree)) stop("must refresh directory tree first", call. = FALSE)
  if(verbose) message("Info: retrieving ", n_max, " most recent gdrive files")

  # function to find which of the immediate parents is listed for each file
  directories <- storage$directory_tree
  parent_ids <- directories$id
  find_parent_id <- function(drive_resource) {
    parent_ids <- parent_ids[parent_ids %in% unlist(drive_resource$parents)]
    if (length(parent_ids) == 0) return(NA_character_)
    return (parent_ids[1]) # just go with the first of the eligible parents if there are more
  }

  # find files
  directories %>%
    dplyr::mutate(n = 1:n(), group = floor(n / chain_limit)) %>%
    dplyr::group_by(group) %>%
    do({
      query <-
        sprintf("'%s' in parents", .$id) %>% stringr::str_c(collapse = " or ") %>% { sprintf("(%s)", .) }
      drive_find(q = query, verbose = verbose, pattern = file_pattern, n_max = n_max)
    }) %>% ungroup() %>%
    dplyr::mutate(
      parent_id = purrr::map_chr(drive_resource, find_parent_id)
    ) %>%

    dplyr::left_join(dplyr::select(directories, parent_id = id, parent_name = name, parent_folder = parent, parent_root = root), by = "parent_id") %>%
    dplyr::mutate(parent = ifelse(parent_root, "", stringr::str_c(parent_folder, "/", parent_name))) %>%
    dplyr::select(-group, -parent_name, -parent_folder, -parent_root) %>%
    dplyr::mutate(parent = stringr::str_replace(parent, "^/", ""),
           mtime = as.POSIXct(purrr::map_chr(drive_resource, "modifiedTime"), format = "%Y-%m-%dT%H:%M:%OS")) %>%
    arrange(desc(mtime), name) %>%
    dplyr::filter(row_number() <= n_max) %>%
    dplyr::select(parent, everything())
}

}

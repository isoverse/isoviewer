# File/folder selection module ====

#' file selector server side
#'
#' @param root directory
#' @param root_name the root directory name
#' @param pattern regexp file selection pattern
#' @param multiple whether to allow multiple file selection
#' @param enable_recent whether to have a recent tab
#' @param start_recent whether to start on the recent tab
#' @param start_n_recent how many recent items to show at the beginning
#' @param exclude_recent regex what to explude in the recent list (by default nothing is excluded)
#' @param start_sort_desc whether to start sorting in descending order
#' @family file selection module functions
fileSelectorServer <- function(
  input, output, session,
  root, root_name = basename(root), pattern = NULL, multiple = TRUE,
  enable_recent = TRUE, start_recent = TRUE, start_n_recent = 50, exclude_recent = NULL,
  start_sort_desc = TRUE) {

  # namespace and constants
  ns <- session$ns
  recent_value <- "<<RECENT>>"
  recent_label <- "Recent"
  module_message(ns, "info", "Initiating file selector")

  # make sure root is absolute for safety
  if (!isAbsolutePath(root)) root <- filePath(getwd(), root)

  # reactive values ====
  values <- reactiveValues(
    current_directory = root, # keep track of current directory
    content_hash = NULL, # to check for updates to current directory
    content_list = c(), # actual listed content
    recent_active = start_recent, # whether showing "Recent" files
    last_double_click_file = NULL, # keep track of last double clicked file
    n_recent = start_n_recent, # how many recent to show
    sort_desc = start_sort_desc # sorting direction
  )

  # manage sorting ====
  observe({ req(input$sort_desc); values$sort_desc <- TRUE })
  observe({ req(input$sort_asc); values$sort_desc <- FALSE })
  observe({
    active <- if (values$sort_desc) "sort_desc" else "sort_asc"
    inactive <- if (!values$sort_desc) "sort_desc" else "sort_asc"
    module_message(ns, "debug", "sorting is now ", active)
    addClass(active, "active")
    removeClass(inactive, "active")
  })

  # manage n recent ===
  observe({
    req(input$n_recent)
    module_message(ns, "debug", "# recent to show is now: ", input$n_recent)
    values$n_recent <- input$n_recent
  })

  # tab path on-click ====
  # trigger file system navigation from tab on-click or select box double click
  shinyjs::onclick("tab_path", function(event) {
    tabdir <- input$tab_path
    # only do anything if different from current directory
    if (!is.null(tabdir) && (tabdir != values$current_directory || (values$recent_active && tabdir != recent_value))) {
      values$content_hash <- NULL # always reset content
      if (tabdir == recent_value) {
        module_message(ns, "info", "Moving to RECENT files")
        values$recent_active <- TRUE
      } else {
        module_message(ns, "info", "Moving to directory ", sub(root, root_name, tabdir))
        values$current_directory <- tabdir
        values$recent_active <- FALSE
      }
    }
  })

  # select list double-click ====
  # trigger file system navigation from doubleclick on folder in folder/file list
  # also triggers storage of double clicked files in last_double_click_file
  shinyjs::onevent("dblclick", "content_list", function(event) {
    # check if selected item is different from current folder, and is a valid subdirectory
    item <- input$content_list
    if(!is.null(item) && item != "" && # make sure not empty
       item != values$current_directory) { # make sure not the same as current directory
      # check if directory or file
      if (dir.exists(item)) {
        module_message(ns, "info", "Moving to sub directory ", sub(root, root_name, item))
        values$content_hash <- NULL # always reset content
        values$current_directory <- item
        values$recent_active <- FALSE
      } else if (file.exists(item)) {
        module_message(ns, "debug", "assigning last double click file", item)
        values$last_double_click_file <- item
      }
    }
  })

  # safety checks ====
  # on selected paths (to avoid anyone getting access to unintended parts of the file system)
  observe({
    req(values$current_directory)
    module_message(ns, "debug", "safety checking folder ", sub(root, root_name, values$current_directory))
    stopifnot(file.exists(values$current_directory)) # must exists
    stopifnot(isAbsolutePath(values$current_directory)) # must be absolute path
    stopifnot(grepl(root, values$current_directory, fixed = TRUE)) # must be subdirectory of root
  })

  # potentially useful: disable the file select list:
  # disable(selector = sprintf("#%s select", ns("content_list"))) # does not work with disable("content_list")
  # enable(selector = sprintf("#%s select", ns("content_list"))) # does not work with disable("content_list")

  # check for updates ====
  # to what is in content list
  observe({

    # check for updates if directory changes, if recent is activated, recent number changes or if sorting changes
    # also triggers when current folder content changes (checks for updates every second)
    req(values$current_directory)
    req(!is.null(values$recent_active))
    req(!is.null(values$sort_desc))
    req(values$n_recent)
    invalidateLater(1000, session)

    # everything else is isolated
    isolate({
      if (values$recent_active) {
        # find recent files (recursive from root)
        recent_files <- find_recent_files(root, pattern, exclude_recent) %>%
          dplyr::filter(row_number() <= values$n_recent) %>%  # get the right number of recent
          dplyr::mutate(rel_dir = sub("^[\\/]?", "", sub(root, "", folder, fixed = TRUE)), # replace root in folder
                 label = ifelse(nchar(rel_dir) > 0, sprintf("%s (%s)", time_file_label, rel_dir), time_file_label)) %>%
          { if (values$sort_desc) arrange(., desc(mtime)) else arrange(., mtime) } # sort
        content_hash <- generate_content_hash(recent_files$mtime)
        new_content <- setNames(recent_files$filepath, recent_files$label)
      } else {
        # content of selected folder (files and subfolders) in right sorting order
        folders <- list.dirs(values$current_directory, rec=FALSE, full.names = T) %>%
        { if(values$sort_desc) rev(.) else . } # sort desc or asc
        files <- setdiff(list.files(values$current_directory, full.names = T, pattern = pattern, ignore.case = TRUE), folders)%>%
        { if(values$sort_desc) rev(.) else . } # sort desc or asc
        content_hash <- generate_content_hash(c(folders, files))
        new_content <- setNames(c(folders, files), c(sprintf("[ %s ]", basename(folders)), basename(files)))
      }

      # check if anything changed
      if (is.null(values$content_hash) || values$content_hash != content_hash ) {
        module_message(ns, "debug", "(re)loading content for directory ",
                       if(values$recent_active) recent_value else sub(root, "", values$current_directory))
        values$content_hash <- content_hash
        values$content_list <- new_content
      }

    })
  })

  # render recent input =====
  output$n_recent_wrap <- renderUI({
    if(enable_recent) {
      inlineInput(numericInput, ns("n_recent"), h4("# Recent: "), value = start_n_recent,
                  min = 1, step = 1, width = "60px",
                  tooltip = "Enter how many recent files to display")
    } else NULL
  })

  # generate path tabs ====
  output$tab_path <- renderUI({
    # only regenerate tabs if current directoy changes
    module_message(ns, "debug", "new tab generation triggered")
    tmp_path <- values$current_directory

    # everything else is isolated
    isolate({
      selected_tab <- if (enable_recent && values$recent_active) recent_value else tmp_path
      parents <- list(id = ns("tab_path"), selected = selected_tab)
      while (tmp_path != dirname(root)){
        if (tmp_path == root)
          parent <- root_name
        else
          parent <- basename(tmp_path)
        parents <- c(parents, list(tabPanel(parent, value = tmp_path)))
        tmp_path <- dirname(tmp_path)
      }
      if (enable_recent) {
        # make recent bullet
        parents <- c(parents, list(tabPanel(recent_label, value = recent_value)))
      }
      do.call(tabsetPanel, args = parents[length(parents):1])
    })
  })

  # generate folder content listing ====
  output$content_list <-renderUI({
    module_message(ns, "debug", "new folder content listing triggered")
    selectInput(ns("content_list"), NULL, width = "100%",
                size = isolate(input$size), selectize = F, multiple = multiple,
                values$content_list)
  })

  # return both the current path and the selected folder contents
  list(
    modal_closed = reactive(input$close),
    path = reactive(values$current_directory),
    path_relative = reactive(sub("^[\\/]?", "", sub(root, "", values$current_directory, fixed = TRUE))),
    selection = reactive(input$content_list),
    selection_relative = reactive(sub("^[\\/]?", "", sub(root, "", input$content_list, fixed = TRUE))),
    double_click_file = reactive(values$last_double_click_file)
  )
}


#' File Selector UI
#'
#' @param id the file selector id
#' @param size the height of the selection box (number of rows)
#' @family file selection module functions
fileSelectorUI <- function(id, size = 12) {
  ns <- NS(id)
  tagList(
    rightInline(
        inline(uiOutput(ns("n_recent_wrap"))),
        spaces(3),
        inline(h4("Sorting: ")),
        tooltipInput(actionButton, ns("sort_asc"), NULL, icon = icon("sort-alpha-asc"),
                     tooltip = "Sort in ascending order"),
        tooltipInput(actionButton, ns("sort_desc"), NULL, icon = icon("sort-alpha-desc"),
                     tooltip = "Sort in desending order")
    ),
    uiOutput(ns("tab_path")),
    uiOutput(ns("content_list")) %>% withSpinner(type = 5, proxy.height = stringr::str_c(size * 20.2, "px")),
    hidden(numericInput(ns("size"), NULL, value = size)) # size information for server
  )
}

#' modalFileSelector
#' @inheritParams fileSelectorUI
#' @param open_label the label for the link to open the modal dialog
#' @param dialog_label the label of the modal dialog, same as the open_label by default
#' @param close_label the label for the button to close the modal dialog
#' @param dialog_size the size of the dialog, default is small
#' @family file selection module functions
modalFileSelectorUI <- function(id, size,
                                dialog_size = "large", open_label = "Select file", dialog_label = open_label, close_label = "Select",
                                link_wrapper = identity) {
  ns <- NS(id)
  modal_dlg <- bsModal(ns("modal_dialog"), dialog_label, ns("modal_link"), size = dialog_size,
                       column(width = 12, fileSelectorUI(id, size)) %>% fluidRow())

  modal_dlg$children[[1]]$children[[1]]$children[[3]]$children[[1]]$children[[1]] <- close_label
  modal_dlg$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs$id <- ns("close")
  modal_dlg$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs$class <- "btn btn-default action-button"

  dialog_tags <-
    tagList(
      link_wrapper(actionLink(ns("modal_link"), open_label, icon = icon("file"))),
      modal_dlg
    )
  return(dialog_tags)
}


# helper functions ====

# recursively find all data files and sort by last modified time (exclude pattern)
find_recent_files <- function(folder, pattern, exclude = NULL) {
  folder %>%
    list.files(pattern = pattern, full.names = T, recursive = TRUE, ignore.case = TRUE) %>%
    lapply(function(file) list(filepath = file, mtime = file.mtime(file))) %>%
    bind_rows() %>%
    { if (!is.null(exclude)) dplyr::filter(., !grepl(exclude, filepath)) else . } %>%
    arrange(desc(mtime), filepath) %>%
    dplyr::mutate(
      file = basename(filepath),
      folder = dirname(filepath),
      time_file_label = stringr::str_c(format(mtime, format = "%b %d, %Hh %Mm: "), basename(file)))
}



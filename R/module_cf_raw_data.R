# TODO: implement zooming on the chromatograms

#' Continuous Flow Raw Data
#' @inheritParams fileInfoServer
#' @family continuous flow raw data module functions
cfRawDataServer <- function(input, output, session, isofiles, dataset_name, visible = NULL) {

  # namespace
  ns <- session$ns

  # zooming ====
  zoom_factor <- 2 # zoom in and out factor with each click
  zoom_move <- 0.5 # sideways move interval
  values <- reactiveValues(
    zoom_stack = list(list(zoom = NULL, x_min = NULL, x_max = NULL)),
    zoom_update = 0,
    rendered_time = "seconds" # to keep track what x units last rendered in
  )

  # add to zoom stack
  add_to_zoom_stack <- function(zoom, x_min, x_max, update = TRUE, only_add_if_new = TRUE) {
    if (missing(zoom)) zoom <- get_last_zoom()$zoom
    if (missing(x_min)) x_min <- get_last_zoom()$x_min
    if (missing(x_max)) x_max <- get_last_zoom()$x_max
    new_zoom <- list(zoom = zoom, x_min = x_min, x_max = x_max)
    if (only_add_if_new && identical(get_last_zoom(), new_zoom)) return(NULL)
    module_message(ns, "debug", "adding to zoom stack: ", zoom, " time: ", x_min, " to ", x_max)
    values$zoom_stack <- c(values$zoom_stack, list(new_zoom))
    if (update) values$zoom_update <- values$zoom_update + 1
  }

  # load last zoom
  load_last_zoom <- function(update = TRUE) {
    last_element <- length(values$zoom_stack)
    if (last_element > 1) values$zoom_stack[last_element] <- NULL
    if (update) values$zoom_update <- values$zoom_update + 1
  }

  # get current zoom
  get_last_zoom <- function() {
    values$zoom_stack[[length(values$zoom_stack)]]
  }

  # reset zoom stack
  observeEvent(dataset_name(), {
    values$zoom_stack <- list(list(zoom = NULL, x_min = NULL, x_max = NULL))
  })
  # zoom back
  observeEvent(input$zoom_back, load_last_zoom())
  observeEvent(input$plot_dblclick, load_last_zoom())
  # zoom whole chromatogram
  observeEvent(input$zoom_all, {
    add_to_zoom_stack(zoom = NULL, x_min = NULL, x_max = NULL)
  })
  # zoom fit
  observeEvent(input$zoom_fit, {
    add_to_zoom_stack(zoom = NULL)
  })
  # zoom in
  observeEvent(input$zoom_in, {
    if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(zoom = zoom_factor)
    else add_to_zoom_stack(zoom = get_last_zoom()$zoom * zoom_factor)
  })
  # zoom out
  observeEvent(input$zoom_out, {
    if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(zoom = 1/zoom_factor)
    else add_to_zoom_stack(zoom = get_last_zoom()$zoom/zoom_factor)
  })
  # time zoom
  observeEvent(input$plot_brush, {
    brush <- input$plot_brush
    if (!is.null(brush$xmin) && !is.null(brush$xmax)) {
      # convert to seconds
      x_min <- isoreader:::scale_time(brush$xmin, to = "seconds", from = values$rendered_time)
      x_max <- isoreader:::scale_time(brush$xmax, to = "seconds", from = values$rendered_time)
      add_to_zoom_stack(x_min = x_min, x_max = x_max)
    }
  })
  # left right movening
  move_zoom <- function(direction) {
    if ( !is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max) ) {
      add_to_zoom_stack(
        x_min = get_last_zoom()$x_min + direction * zoom_move * (get_last_zoom()$x_max - get_last_zoom()$x_min),
        x_max = get_last_zoom()$x_max + direction * zoom_move * (get_last_zoom()$x_max - get_last_zoom()$x_min)
      )
    }
  }
  observeEvent(input$zoom_move_left, move_zoom(-1))
  observeEvent(input$zoom_move_right, move_zoom(+1))

  # masses and ratios ====

  #FIXME: determine these dynamically
  ratios <-
    data_frame(
      ratio = c("29/28", "30/28", "45/44", "46/44", "32/30", "33/32", "34/32", "36/32", "3/2")
    ) %>%
    mutate(
      top = sub("(\\d+)/(\\d+)", "\\1", ratio),
      bot = sub("(\\d+)/(\\d+)", "\\2", ratio)
    )

  # mass and ratio selector
  mass_ratio_selector <- callModule(
    selectorTableServer, "selector", id_column = "column", col_headers = c("Data", "Type"),
    hot_mods = function(hot) hot_col(hot, col = c("Data", "Type"), halign = "htCenter"))

  # generate selector list
  observe({
    req(length(isofiles()) > 0)
    masses <- aggregate_raw_data(isofiles(), gather = TRUE, quiet = TRUE)$data %>% unique()
    ratios <- filter(ratios, top %in% masses, bot %in% masses)$ratio
    mass_ratio_selector$set_table(
      bind_rows(
        data_frame(column = masses, type = "mass"),
        data_frame(column = ratios, type = "ratio")
      ))
  })

  # functions
  get_ratios <- reactive({
    mass_ratio_selector$get_selected() %>% { .[. %in% ratios$ratio] }
  })

  # show mass/ratio selector list box
  observe({
    if (is.function(visible)) {
      toggle("selector_box", condition = visible() & length(isofiles()) > 0 )
      toggle("settings_box", condition = visible() & length(isofiles()) > 0 )
    } else {
      toggle("selector_box", condition = length(isofiles()) > 0)
      toggle("settings_box", condition = length(isofiles()) > 0)
    }
  })

  # visibility of plot messages and buttons  ====
  observe({
    toggle("plot_actions", condition =
             length(isofiles()) > 0 & length(mass_ratio_selector$get_selected()) > 0)
    toggle("plot_div", condition =
             length(isofiles()) > 0 & length(mass_ratio_selector$get_selected()) > 0)
    toggle("plot_messages", condition =
             length(isofiles()) == 0 | length(mass_ratio_selector$get_selected()) == 0)
  })

  # plot messages
  output$plot_message <- renderText({
    validate(
      need(length(isofiles()) > 0, "Please select a dataset and at least one data file.") %then%
        need(length(mass_ratio_selector$get_selected()) > 0, "Please select at least one mass or ratio.")
    )
  })

  # generate  plot ====
  get_plot_params <- reactive({
    c(
      panel_by = input$panel_by,
      color_by = input$color_by,
      linetype_by = input$linetype_by,
      shape_by = input$shape_by
    )
  })

  refresh_plot <- reactive({
    # all plot referesh triggers
    input$render_plot
    input$selector_refresh
    input$settings_refresh
    values$zoom_update
  })

  generate_plot <- reactive({

    # update triggers
    dataset_name()
    refresh_plot()

    # rest is isolated
    isolate({
      req(length(isofiles()) > 0)
      req(length(mass_ratio_selector$get_selected()) > 0)
      req(input$scale_time)
      module_message(ns, "debug", "rendering continuous flow raw data plot")

      # prep data
      plot_isofiles <- isofiles()
      if (input$scale_signal != "<NONE>") {
        plot_isofiles <- convert_signals(plot_isofiles, to = input$scale_signal)
      }
      plot_isofiles <- convert_time(plot_isofiles, to = input$scale_time)
      values$rendered_time <- input$scale_time
      if (length(get_ratios()) > 0) {
        plot_isofiles <- calculate_ratios(plot_isofiles, get_ratios())
      }

      # time interval
      time_params <- list()
      if (!is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max)) {
        time_params <- list(time_interval = c(get_last_zoom()$x_min, get_last_zoom()$x_max))
      }

      # plot data
      p <- do.call(plot_raw_data, args =
                c(list(isofiles = plot_isofiles,
                       data = mass_ratio_selector$get_selected(),
                       zoom = get_last_zoom()$zoom),
                  time_params,
                  as.list(get_plot_params()))) +
        theme(text = element_text(size = 18))

      # legend position
      if (input$legend_position == "bottom") {
        p <- p + theme(legend.position = "bottom", legend.direction="vertical")
      } else if (input$legend_position == "hide") {
        p <- p + theme(legend.position = "none")
      }

      return(p)
    })
  })

  output$plot <- renderPlot(
    generate_plot(),
    height = reactive({ refresh_plot(); isolate(input$plot_height) }))

  # plot download ====
  download_handler <- callModule(
    plotDownloadServer, "plot_download",
    plot_func = generate_plot,
    filename_func = reactive({ str_c(dataset_name(), ".pdf") }))

  # code update ====
  code_update <- reactive({

    theme_extra <-
      if (input$legend_position == "bottom") 'legend.position = "bottom", legend.direction = "vertical"'
      else if (input$legend_position == "hide") 'legend.position = "none"'
      else NULL

    plot_params <- get_plot_params() %>% { setNames(sprintf("\"%s\"",.), names(.))  }
    if (!is.null(get_last_zoom()$x_min) && !is.null(get_last_zoom()$x_max)) {
      scaled_x_min <- isoreader:::scale_time(get_last_zoom()$x_min, to = input$scale_time, from = "seconds")
      scaled_x_max <- isoreader:::scale_time(get_last_zoom()$x_max, to = input$scale_time, from = "seconds")
      plot_params <- c(
        c(time_interval = sprintf("c(%.2f, %.2f)", scaled_x_min, scaled_x_max),
          time_interval_units = sprintf("\"%s\"", input$scale_time)),
        plot_params)
    }
    if (!is.null(get_last_zoom()$zoom)) {
      plot_params <- c(zoom = get_last_zoom()$zoom, plot_params)
    }


    function(rmarkdown = TRUE) {
      code(
        generate_cf_processing_code(
          scale_signal = input$scale_signal,
          scale_time = input$scale_time,
          ratios = get_ratios(),
          rmarkdown = rmarkdown
        ),
        generate_plot_code(
          data = mass_ratio_selector$get_selected(),
          plot_params = plot_params,
          theme1 = "text = element_text(size = 18)",
          theme2 = theme_extra,
          rmarkdown = rmarkdown
        )
      )
    }
  })

  # return functions
  list(
    get_code_update = code_update
  )
}


#' Continuous Flow Raw Data Plot UI
#' @inheritParams isofilesLoadUI
#' @family continuous flow raw data module functions
cfRawDataPlotUI <- function(id) {
  ns <- NS(id)
  div(style = "min-height: 500px;",
      div(id = ns("plot_messages"),
          textOutput(ns("plot_message"))),
      # plot actions
      div(id = ns("plot_actions"),
          fluidRow(
            column(width = 3),
            column(width = 6, align = "center",
                   tooltipInput(actionButton, ns("zoom_all"), "", icon = icon("resize-full", lib = "glyphicon"),
                                tooltip = "Show whole chromatogram"),
                   tooltipInput(actionButton, ns("zoom_in"), "", icon = icon("plus"),
                                tooltip = "Zoom in"),
                   tooltipInput(actionButton, ns("zoom_out"), "", icon = icon("minus"),
                                tooltip = "Zoom out"),
                   tooltipInput(actionButton, ns("zoom_fit"), "", icon = icon("resize-vertical", lib = "glyphicon"),
                                type = "toggle", tooltip = "Switch to optimal zoom<br/>for visible peaks"),
                   tooltipInput(actionButton, ns("zoom_move_left"), "", icon = icon("arrow-left"),
                                tooltip = "Move along the chromatogram<br/>to the left"),
                   tooltipInput(actionButton, ns("zoom_move_right"), "", icon = icon("arrow-right"),
                                tooltip = "Move along the chromatogram<br/>to the right"),
                   tooltipInput(actionButton, ns("zoom_back"), "", icon = icon("rotate-left"),
                                tooltip = "Revert to previous zoom")

            ),
            column(width = 3, align = "right",
                   tooltipInput(actionButton, ns("render_plot"), "Plot", icon = icon("refresh"),
                                tooltip = "Refresh the plot with the selected files and parameters."),
                   spaces(1),
                   plotDownloadLink(ns("plot_download"), label = "Save")
            )
          )
      ) %>% hidden(),
      div(id = ns("plot_div"),
          plotOutput(ns("plot"), height = "100%",
                     dblclick = ns("plot_dblclick"),
                     brush = brushOpts(
                       id = ns("plot_brush"),
                       delayType = "debounce",
                       direction = "x",
                       resetOnNew = TRUE
                     )) %>%
            withSpinner(type = 5, proxy.height = "450px")
      )
  )
}


#' Continuous Flow Raw Data Selector UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family continuous flow raw data module functions
cfRawDataSelectorUI <- function(id, width = 4, selector_height = "200px") {
  ns <- NS(id)

  # scaling options
  scaling_options <- c("use original" = "<NONE>", "mV" = "mV", "V" = "V",
                       "pA" = "pA", "nA" = "nA", "µA" = "µA", "mA" = "mA")

  time_options <- c("use original" = "<NONE>", "seconds" = "seconds",
                    "minutes" = "minutes", "hours" = "hours")

  div(id = ns("selector_box"),
      default_box(
        title = "Masses and Ratios", width = width,
        fluidRow(
          h4("Scale signals:") %>% column(width = 4),
          selectInput(ns("scale_signal"), NULL, choices = scaling_options) %>% column(width = 8)),
        fluidRow(
          h4("Scale time:") %>% column(width = 4),
          selectInput(ns("scale_time"), NULL, choices = time_options, selected = "seconds") %>% column(width = 8)),
        selectorTableUI(ns("selector"), height = selector_height),
        footer = div(
          #style = "height: 35px;",
          selectorTableButtons(ns("selector")),
          spaces(1),
          tooltipInput(actionButton, ns("selector_refresh"), label = "Plot", icon = icon("refresh"),
                       tooltip = "Refresh plot with new scale, mass and ratio selections.")
        )
      )
  )%>% hidden()
}


#' Continuous Flow Raw Data Settings UI
#' @inheritParams isofilesLoadUI
#' @param width box width
#' @family continuous flow raw data module functions
cfRawDataSettingsUI <- function(id, width = 4) {
  ns <- NS(id)

  # options for aesthetics
  aes_options <- c("None" = "none", "Masses & Ratios" = "data", "Files" = "file")

  div(id = ns("settings_box"),
      default_box(
        title = "Plot Settings", width = width,
        fluidRow(
          h4("Plot height:") %>% column(width = 4),
          numericInput(ns("plot_height"), NULL, value = 500, min = 100, step = 50) %>% column(width = 8)),
        fluidRow(
          h4("Panel by:") %>% column(width = 4),
          selectInput(ns("panel_by"), NULL, choices = aes_options, selected = "data") %>% column(width = 8)),
        fluidRow(
          h4("Color by:") %>% column(width = 4),
          selectInput(ns("color_by"), NULL, choices = aes_options, selected = "file") %>% column(width = 8)
        ),
        fluidRow(
          h4("Linetype by:") %>% column(width = 4),
          selectInput(ns("linetype_by"), NULL, choices = aes_options, selected = "none") %>% column(width = 8)
        ),
        fluidRow(
          h4("Legend:") %>% column(width = 4),
          selectInput(ns("legend_position"), NULL, choices = c("right", "bottom", "hide"), selected = "right") %>% column(width = 8)
        ),
        footer = tooltipInput(actionButton, ns("settings_refresh"), label = "Plot", icon = icon("refresh"),
                              tooltip = "Refresh plot with new plot settings.")
      )
  )%>% hidden()
}

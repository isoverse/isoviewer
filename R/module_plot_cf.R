#' cf plot
#' @param get_variable get variable name
plot_cf_server <- function(input, output, session, settings, get_variable, get_iso_files, is_visible) {

  # namespace
  ns <- session$ns

  # plot server =====
  base_plot <- callModule(
    plot_server, "base_plot",
    settings = settings,
    get_variable = get_variable,
    generate_plot = generate_plot,
    reset_trigger = reactive({ input$reset })
  )

  # signal selection =====
  observeEvent(get_variable(), {
    req(get_variable())
    updateSelectInput(
      session, "scale_signal",
      selected = settings$get(ns(paste0("signal-", get_variable())), default = "NULL")
    )
  })
  observeEvent(input$scale_signal, {
    req(get_variable())
    module_message(ns, "info", "PLOT user set scale_signal to '", input$scale_signal, "'")
    settings$set(ns(paste0("signal-", get_variable())), input$scale_signal)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # time selection =====
  observeEvent(get_variable(), {
    req(get_variable())
    updateSelectInput(
      session, "scale_time",
      selected = settings$get(ns(paste0("time-", get_variable())), default = "seconds")
    )
  })
  observeEvent(input$scale_time, {
    module_message(ns, "info", "PLOT user set scale_time to '", input$scale_time, "'")
    settings$set(ns(paste0("time-", get_variable())), input$scale_time)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # file info ====
  get_file_info <- reactive({
    req(length(get_iso_files()) > 0)
    isoreader::iso_get_file_info(get_iso_files(), quiet = TRUE)
  })

  # traces selector ====
  traces <- callModule(
    trace_selector_server, "traces",
    settings = settings,
    get_variable = get_variable,
    get_iso_files = get_iso_files
  )

  # show plot options boxes ====
  observeEvent(is_visible(), {
    toggle("selector_box", condition = is_visible())
    toggle("settings_box", condition = is_visible())
  })

  # trigger plot render ====
  observeEvent(input$settings_refresh, { base_plot$render_plot() })
  observeEvent(input$selector_refresh, { base_plot$render_plot() })

  # get aesthetics dropdowns options ====
  get_aes_options <- eventReactive(get_file_info(), {
    file_info_cols <- names(get_file_info())
    aes_options <- c(
      "None" = "NULL",
      list(
        `Data Columns` = c(
          "Traces (data)" = "data",
          #"Traces no units (data_wo_units)" = "data_wo_units", # too confusing
          "Category" = "category"
        ),
        `File Info` = file_info_cols
      ))
    return(aes_options)
  })

  # function formals
  func_formals <- formals(isoprocessor:::iso_plot_continuous_flow_data.iso_file_list)

  # panel aesthetic ======
  panel <- callModule(
    function_plot_param_server, "panel",
    settings = settings,
    get_variable = get_variable,
    type = "expression",
    get_value_options = get_aes_options,
    default_value = func_formals$panel,
    reset_trigger = reactive({ input$reset })
  )

  # color aesthetic ======
  color <- callModule(
    function_plot_param_server, "color",
    settings = settings,
    get_variable = get_variable,
    type = "expression",
    get_value_options = get_aes_options,
    default_value = func_formals$color,
    reset_trigger = reactive({ input$reset })
  )

  # linetype aesthetic ======
  linetype <- callModule(
    function_plot_param_server, "linetype",
    settings = settings,
    get_variable = get_variable,
    type = "expression",
    get_value_options = get_aes_options,
    default_value = func_formals$linetype,
    reset_trigger = reactive({ input$reset })
  )

  # get function parameters ====
  get_function_parameter_values <- reactive({
    list(
      panel = panel$get_value(),
      color = color$get_value(),
      linetype = linetype$get_value()
    )
  })
  get_function_parameter_is_default <- reactive({
    c(
      panel = panel$is_default(),
      color = color$is_default(),
      linetype = linetype$is_default()
    )
  })

  # generate plot ====
  generate_plot <- function() {
    # get data from scratch (not from options) to enable signal conversion
    validate(
      need(!is.null(traces$get_selected_traces()), "Error: please select at least one data trace.")
    )
    zoom$rendered_time <- input$scale_time
    get_iso_files() %>%
      {
        if (input$scale_signal != "NULL")
          isoprocessor::iso_convert_signals(., to = input$scale_signal, quiet = TRUE)
        else .
      } %>%
      isoprocessor::iso_convert_time(to = input$scale_time, quiet = TRUE) %>%
      isoprocessor::iso_plot_continuous_flow_data(
        data = traces$get_selected_traces(),
        time_interval = c(get_last_zoom()$x_min, get_last_zoom()$x_max),
        time_interval_units = "seconds", # always in secondds
        zoom = get_last_zoom()$zoom,
        panel = !!get_function_parameter_values()$panel,
        color = !!get_function_parameter_values()$color,
        linetype = !!get_function_parameter_values()$linetype
      )
  }

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_cf_plot_code(
        dataset = get_variable(),
        scale_signal = input$scale_signal,
        scale_time  = input$scale_time,
        data = traces$get_selected_traces(),
        zoom = get_last_zoom(),
        aes_options = get_function_parameter_values()[!get_function_parameter_is_default()],
        theme_options = base_plot$get_theme_options(),
        rmarkdown = rmarkdown
      )
    }
  })

  # disable/enable buttons =====
  observeEvent(base_plot$has_plot(), {
    shinyjs::toggleState("zoom_all", condition = base_plot$has_plot())
    shinyjs::toggleState("zoom_in", condition = base_plot$has_plot())
    shinyjs::toggleState("zoom_out", condition = base_plot$has_plot())
    shinyjs::toggleState("zoom_fit", condition = base_plot$has_plot())
    shinyjs::toggleState("zoom_move_left", condition = base_plot$has_plot())
    shinyjs::toggleState("zoom_move_right", condition = base_plot$has_plot())
    shinyjs::toggleState("zoom_back", condition = base_plot$has_plot())
  })

  # zoom functions =====
  zoom_factor <- 2 # zoom in and out factor with each click
  zoom_move <- 0.5 # sideways move interval
  zoom <- reactiveValues(
    stack = list(list(zoom = NULL, x_min = NULL, x_max = NULL)),
    rendered_time = "seconds" # to keep track what x units last rendered in
  )
  add_to_zoom_stack <- function(new_zoom, x_min, x_max, update = TRUE, only_add_if_new = TRUE) {
    if (missing(new_zoom)) new_zoom <- get_last_zoom()$zoom
    if (missing(x_min)) x_min <- get_last_zoom()$x_min
    if (missing(x_max)) x_max <- get_last_zoom()$x_max
    new_zoom <- list(zoom = new_zoom, x_min = x_min, x_max = x_max)
    if (only_add_if_new && identical(get_last_zoom(), new_zoom)) return(NULL)
    module_message(ns, "info", "ZOOM adding to stack: ", new_zoom$zoom, " time: ", x_min, " to ", x_max)
    zoom$stack <- c(zoom$stack, list(new_zoom))
    if (update) base_plot$render_plot()
  }
  load_last_zoom <- function(update = TRUE) {
    last_element <- length(zoom$stack)
    if (last_element > 1) zoom$stack[last_element] <- NULL
    if (update) base_plot$render_plot()
  }
  get_last_zoom <- function() {
    zoom$stack[[length(zoom$stack)]]
  }
  reset_zoom_stack <- function() {
    zoom$stack <- list(list(zoom = NULL, x_min = NULL, x_max = NULL))
  }

  # zoom events ====
  observeEvent(get_variable(), reset_zoom_stack())
  observeEvent(base_plot$dblclick(), load_last_zoom())
  observeEvent(input$zoom_back, load_last_zoom())
  observeEvent(input$zoom_all, add_to_zoom_stack(new_zoom = NULL, x_min = NULL, x_max = NULL))
  observeEvent(input$zoom_fit, add_to_zoom_stack(new_zoom = NULL))
  observeEvent(input$zoom_in, {
    if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(new_zoom = zoom_factor)
    else add_to_zoom_stack(new_zoom = get_last_zoom()$zoom * zoom_factor)
  })
  observeEvent(input$zoom_out, {
    if (is.null(get_last_zoom()$zoom)) add_to_zoom_stack(new_zoom = 1/zoom_factor)
    else add_to_zoom_stack(new_zoom = get_last_zoom()$zoom/zoom_factor)
  })
  observeEvent(base_plot$brush(), {
    brush <- base_plot$brush()
    if (!is.null(brush$xmin) && !is.null(brush$xmax)) {
      # convert to seconds
      x_min <- isoprocessor:::scale_time(brush$xmin, to = "seconds", from = zoom$rendered_time)
      x_max <- isoprocessor:::scale_time(brush$xmax, to = "seconds", from = zoom$rendered_time)
      add_to_zoom_stack(x_min = x_min, x_max = x_max)
    }
  })
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

  # return functions =====
  list(
    get_code_update = code_update
  )

}


#' cf plot UI
plot_cf_ui <- function(id) {
  ns <- NS(id)
  plot_ui(
    ns("base_plot"),
    brush_direction = "x", dblclick = TRUE,
    center_actions = tagList(
      tooltipInput(actionButton, ns("zoom_all"), "", icon = icon("resize-full", lib = "glyphicon"),
                   tooltip = "Show whole chromatogram") %>% shinyjs::disabled(),
      tooltipInput(actionButton, ns("zoom_in"), "", icon = icon("plus"),
                   tooltip = "Zoom in") %>% shinyjs::disabled(),
      tooltipInput(actionButton, ns("zoom_out"), "", icon = icon("minus"),
                   tooltip = "Zoom out") %>% shinyjs::disabled(),
      tooltipInput(actionButton, ns("zoom_fit"), "", icon = icon("resize-vertical", lib = "glyphicon"),
                   type = "toggle", tooltip = "Switch to optimal zoom<br/>for visible peaks") %>% shinyjs::disabled(),
      tooltipInput(actionButton, ns("zoom_move_left"), "", icon = icon("arrow-left"),
                   tooltip = "Move along the chromatogram<br/>to the left") %>% shinyjs::disabled(),
      tooltipInput(actionButton, ns("zoom_move_right"), "", icon = icon("arrow-right"),
                   tooltip = "Move along the chromatogram<br/>to the right") %>% shinyjs::disabled(),
      tooltipInput(actionButton, ns("zoom_back"), "", icon = icon("rotate-left"),
                   tooltip = "Revert to previous zoom") %>% shinyjs::disabled()
    )
  )
}

#' data selector
plot_cf_data_selector_ui <- function(id, width = 4) {
  ns <- NS(id)

  # scaling options
  scaling_options <- c("use original" = "NULL", "mV" = "mV", "V" = "V",
                       "pA" = "pA", "nA" = "nA", "µA" = "µA", "mA" = "mA")

  time_options <- c("seconds" = "seconds", "minutes" = "minutes", "hours" = "hours")

  div(id = ns("selector_box"),
      default_box(
        title = "Data Selector", width = width,
        fluidRow(
          h4("Scale signals:") %>% column(width = 4),
          selectInput(ns("scale_signal"), NULL, choices = scaling_options) %>% column(width = 8)
        ),
        fluidRow(
          h4("Scale time:") %>% column(width = 4),
          selectInput(ns("scale_time"), NULL, choices = time_options, selected = "seconds") %>% column(width = 8)
        ),
        trace_selector_table_ui(ns("traces")),
        footer = div(
          tooltipInput(actionButton, ns("selector_refresh"), label = "Plot", icon = icon("refresh"),
                       tooltip = "Refresh plot with new scale and data selections."),
          spaces(1),
          trace_selector_table_buttons_ui(ns("traces"))
        )
      )
  )%>% hidden()
}

# cf plot options UI
plot_cf_options_ui <- function(id, width = 4) {
  ns <- NS(id)

  div(id = ns("settings_box"),
      default_box(
        title = "Plot Settings", width = width,
        function_plot_param_ui(ns("panel"), label = "Panel by:"),
        function_plot_param_ui(ns("color"), label = "Color by:"),
        function_plot_param_ui(ns("linetype"), label = "Linetype by:"),
        plot_height_ui(ns("base_plot")),
        plot_font_size_ui(ns("base_plot")),
        plot_legend_ui(ns("base_plot")),
        footer = div(
          tooltipInput(actionButton, ns("settings_refresh"), label = "Plot", icon = icon("refresh"),
                       tooltip = "Refresh plot with new plot settings."),
          spaces(1),
          tooltipInput(actionButton, ns("reset"), label = "Reset", icon = icon("fast-backward"),
                       tooltip = "Reset the plot settings back to their defaults.")
        )
      )
  )%>% shinyjs::hidden()

}

#' scan plot
#' @param get_variable get variable name
plot_scan_server <- function(input, output, session, get_variable, get_iso_files, is_visible) {

  # namespace
  ns <- session$ns

  # FIXME: remember scale signal settings!

  # plot server =====
  base_plot <- callModule(
    plot_server, "base_plot",
    get_variable = get_variable,
    generate_plot = generate_plot
  )

  # file info ====
  get_file_info <- reactive({
    req(length(get_iso_files()) > 0)
    isoreader::iso_get_file_info(get_iso_files(), quiet = TRUE)
  })

  # type selection ====
  observeEvent(get_file_info(), {
    types <- unique(get_file_info()$type)
    if (length(types) > 0) {
      selected <- get_gui_setting(ns(paste0("type-", get_variable())), default = types[1])
      if (!selected %in% types) selected <- types[1]
      updateSelectInput(session, "type", choices = types, selected = selected)
    }
  })
  observeEvent(input$type, {
    set_gui_setting(ns(paste0("type-", get_variable())), input$type)
    # type selection triggers a new plot immediately since it is such a big change
    if (base_plot$has_plot()) {
      reset_zoom_stack()
      base_plot$render_plot()
    }
  })

  # traces selector ====
  traces <- callModule(
    trace_selector_server, "traces",
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
          "Traces with units (data)" = "data",
          "Traces no units (data_wo_units)" = "data_wo_units",
          "Category (category)" = "category"
        ),
        `File Info` = file_info_cols
      ))
    return(aes_options)
  })

  # function formals
  func_formals <- formals(isoprocessor:::iso_plot_scan_data.iso_file_list)

  # panel aesthetic ======
  panel <- callModule(
    function_plot_param_server, "panel",
    get_variable = get_variable,
    type = "expression",
    get_value_options = get_aes_options,
    default_value = func_formals$panel,
    reset_trigger = reactive({ input$reset })
  )

  # color aesthetic ======
  color <- callModule(
    function_plot_param_server, "color",
    get_variable = get_variable,
    type = "expression",
    get_value_options = get_aes_options,
    default_value = func_formals$color,
    reset_trigger = reactive({ input$reset })
  )

  # linetype aesthetic ======
  linetype <- callModule(
    function_plot_param_server, "linetype",
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
      need(!is.null(traces$get_selected_traces()), "Error: please select at least one data trace.") %then%
        need(!is.null(input$type), "Error: please select a data type to plot.")
    )
    zoom$last_type <- input$type
    get_iso_files() %>%
      {
        if (input$scale_signal != "NULL")
          isoprocessor::iso_convert_signals(., to = input$scale_signal, quiet = TRUE)
        else .
      } %>%
      isoprocessor::iso_plot_scan_data(
        type = input$type,
        data = traces$get_selected_traces(),
        x_interval = c(get_last_zoom()$x_min, get_last_zoom()$x_max),
        y_interval = c(get_last_zoom()$y_min, get_last_zoom()$y_max),
        panel = !!get_function_parameter_values()$panel,
        color = !!get_function_parameter_values()$color,
        linetype = !!get_function_parameter_values()$linetype
      )
  }

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_scan_plot_code(
        dataset = get_variable(),
        type = input$type,
        scale_signal = input$scale_signal,
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
    shinyjs::toggleState("zoom_back", condition = base_plot$has_plot())
  })

  # zoom functions =====
  zoom <- reactiveValues(
    stack = list(list(x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL)),
    last_type = NULL
  )
  add_to_zoom_stack <- function(x_min, x_max, y_min, y_max, update = TRUE, only_add_if_new = TRUE) {
    new_zoom <- list(x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max)
    if (only_add_if_new && identical(get_last_zoom(), new_zoom)) return(NULL)
    module_message(ns, "info", "ZOOM adding to stack: x = [", x_min, "; ", x_max, "] / y = [", y_min, "; ", y_max, "]")
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
  observeEvent(input$zoom_all, add_to_zoom_stack(x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL))
  observeEvent(base_plot$brush(), {
    brush <- base_plot$brush()
    if (!is.null(brush$xmin) && !is.null(brush$xmax) && !is.null(brush$ymin) && !is.null(brush$ymax)) {
     add_to_zoom_stack(x_min = brush$xmin, x_max = brush$xmax, y_min = brush$ymin, y_max = brush$ymax)
    }
  })

  # return functions =====
  list(
    get_code_update = code_update
  )

}


#' scan plot UI
plot_scan_ui <- function(id) {
  ns <- NS(id)
  plot_ui(
    ns("base_plot"),
    brush_direction = "xy", dblclick = TRUE,
    center_actions = tagList(
      tooltipInput(actionButton, ns("zoom_all"), "", icon = icon("resize-full", lib = "glyphicon"),
                   tooltip = "Show all data") %>% shinyjs::disabled(),
      tooltipInput(actionButton, ns("zoom_back"), "", icon = icon("rotate-left"),
                   tooltip = "Revert to previous zoom") %>% shinyjs::disabled()
    )
  )
}

#' data selector
plot_scan_data_selector_ui <- function(id, width = 4) {
  ns <- NS(id)

  # scaling options
  scaling_options <- c("use original" = "NULL", "mV" = "mV", "V" = "V",
                       "pA" = "pA", "nA" = "nA", "µA" = "µA", "mA" = "mA")

  div(id = ns("selector_box"),
      default_box(
        title = "Data Selector", width = width,
        fluidRow(
          h4("Data Type:") %>% column(width = 4),
          selectInput(ns("type"), NULL, choices = c()) %>% column(width = 8)
        ),
        fluidRow(
          h4("Scale signals:") %>% column(width = 4),
          selectInput(ns("scale_signal"), NULL, choices = scaling_options) %>% column(width = 8)
        ),
        trace_selector_table_ui(ns("traces")),
        footer = div(
          trace_selector_table_buttons_ui(ns("traces")),
          spaces(1),
          tooltipInput(actionButton, ns("selector_refresh"), label = "Plot", icon = icon("refresh"),
                       tooltip = "Refresh plot with new scale and data selections."))
      )
  )%>% hidden()
}

# scan plot options UI
plot_scan_options_ui <- function(id, width = 4) {
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

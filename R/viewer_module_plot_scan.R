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

  # type select ====
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
    get_iso_files() %>%
      {
        if (input$scale_signal != "NULL")
          isoprocessor::iso_convert_signals(., to = input$scale_signal, quiet = TRUE)
        else .
      } %>%
      isoprocessor::iso_plot_scan_data(
        type = input$type,
        data = traces$get_selected_traces(),
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
        aes_options = get_function_parameter_values()[!get_function_parameter_is_default()],
        theme_options = base_plot$get_theme_options(),
        rmarkdown = rmarkdown
      )
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
  plot_ui(ns("base_plot"))
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

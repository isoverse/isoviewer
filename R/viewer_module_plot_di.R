#' Dual inlet plot
#' @param get_variable get variable name
plot_di_server <- function(input, output, session, get_variable, get_iso_files, is_visible) {

  # namespace
  ns <- session$ns

  # reactive values =====
  values <- reactiveValues(
    reset_settings = 1
  )

  # plot server =====
  base_plot <- callModule(
    plot_server, "base_plot",
    get_variable = get_variable,
    generate_plot = generate_plot
  )

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
  get_aes_options <- reactive({
    req(length(get_iso_files()) > 0)
    file_info_cols <- get_iso_files() %>%
      isoreader::iso_get_file_info(quiet = TRUE) %>%
      names()
    aes_options <- c(
      "None" = "NULL",
      list(
        `Data Columns` = c(
          "Standard/Sample (type)" = "type",
          "Traces with units (data)" = "data",
          "Traces no units (data_wo_units)" = "data_wo_units",
          "Category (category)" = "category"
        ),
        `File Info` = file_info_cols
      ))
    return(aes_options)
  })

  # function formals
  func_formals <- formals(isoprocessor:::iso_plot_dual_inlet_data.iso_file_list)

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

  # shape aesthetic ======
  shape <- callModule(
    function_plot_param_server, "shape",
    get_variable = get_variable,
    type = "expression",
    get_value_options = get_aes_options,
    default_value = func_formals$shape,
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
      shape = shape$get_value(),
      linetype = linetype$get_value()
    )
  })
  get_function_parameter_is_default <- reactive({
    c(
      panel = panel$is_default(),
      color = color$is_default(),
      shape = shape$is_default(),
      linetype = linetype$is_default()
    )
  })

  # generate plot ====
  generate_plot <- function() {
    # get data from scratch (not from options) to enable signal conversion
    validate(need(!is.null(traces$get_selected_traces()), "Error: please select at least one data trace."))
    get_iso_files() %>%
      {
        if (input$scale_signal != "NULL")
          isoprocessor::iso_convert_signals(., to = input$scale_signal, quiet = TRUE)
        else .
      } %>%
      isoprocessor::iso_plot_dual_inlet_data(
        data = traces$get_selected_traces(),
        panel = !!get_function_parameter_values()$panel,
        color = !!get_function_parameter_values()$color,
        shape = !!get_function_parameter_values()$shape,
        linetype = !!get_function_parameter_values()$linetype
      )
  }

  # code update ====
  code_update <- reactive({
    function(rmarkdown = TRUE) {
      generate_di_plot_code(
        dataset = get_variable(),
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


#' dual inlet plot UI
plot_di_ui <- function(id) {
  ns <- NS(id)
  plot_ui(ns("base_plot"))
}

#' data selector
plot_di_data_selector_ui <- function(id, width = 4) {
  ns <- NS(id)

  # scaling options
  scaling_options <- c("use original" = "NULL", "mV" = "mV", "V" = "V",
                       "pA" = "pA", "nA" = "nA", "µA" = "µA", "mA" = "mA")

  div(id = ns("selector_box"),
      default_box(
        title = "Data Selector", width = width,
        fluidRow(
          h4("Scale signals:") %>% column(width = 4),
          selectInput(ns("scale_signal"), NULL, choices = scaling_options) %>% column(width = 8)),
        trace_selector_table_ui(ns("traces")),
        footer = div(
          trace_selector_table_buttons_ui(ns("traces")),
          spaces(1),
          tooltipInput(actionButton, ns("selector_refresh"), label = "Plot", icon = icon("refresh"),
                       tooltip = "Refresh plot with new scale and data selections."))
      )
  )%>% hidden()
}

# dual inlet plot options UI
plot_di_options_ui <- function(id, width = 4) {
  ns <- NS(id)

  div(id = ns("settings_box"),
      default_box(
        title = "Plot Settings", width = width,
        plot_height_ui(ns("base_plot")),
        plot_font_size_ui(ns("base_plot")),
        function_plot_param_ui(ns("panel"), label = "Panel by:"),
        function_plot_param_ui(ns("color"), label = "Color by:"),
        function_plot_param_ui(ns("shape"), label = "Shape by:"),
        function_plot_param_ui(ns("linetype"), label = "Linetype by:"),
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

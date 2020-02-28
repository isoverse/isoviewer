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

  # show plot options boxes ====
  observeEvent(is_visible(), {
    toggle("selector_box", condition = is_visible())
    toggle("settings_box", condition = is_visible())
  })

  # trigger plot render ====
  observeEvent(input$settings_refresh, { base_plot$render_plot() })
  observeEvent(input$selector_refresh, { base_plot$render_plot() })

  # options data ====
  get_options_data <- reactive({
    req(length(get_iso_files()) > 0)
    get_iso_files() %>% isoprocessor::iso_prepare_dual_inlet_plot_data(
      # just so it's available
      include_file_info = everything()
    )
  })

  # get all aesthetics options ====
  get_aes_options <- reactive({
    req(length(get_iso_files()) > 0)
    file_info_cols <- get_iso_files() %>%
      isoreader::iso_get_file_info() %>%
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

  # data trace selector =====
  selector <-
    callModule(
      selectorTableServer, "selector",
      id_column = "data_wo_units",
      row_column = "rowid",
      column_select = c(Trace = label)
    )

  # generate data trace selector list ====
  observeEvent(get_options_data(), {
    datas <- get_options_data() %>%
      dplyr::select(data, data_wo_units, category) %>% unique() %>%
      dplyr::mutate(
        data_wo_units = as.character(data_wo_units),
        label = paste(category, data),
        rowid = dplyr::row_number()
      )
    selected <- get_gui_setting(ns(paste0("selector-", get_variable())), default = NULL)
    selector$set_table(datas)
    selector$set_selected(selected)
  })

  # monitor data trace selector ======
  observeEvent(selector$get_selected(), {
    # info
    module_message(
      ns, "info", sprintf(
        "TRACE TABLE user selected %s for '%s'",
        paste(selector$get_selected(), collapse = ", "), get_variable()
      )
    )
    # store selected in settings
    set_gui_setting(ns(paste0("selector-", get_variable())), selector$get_selected())
  })

  # generate plot ====
  generate_plot <- function() {
    # get data from scratch (not from options) to enable signal conversion
    get_iso_files() %>%
      {
        if (input$scale_signal != "NULL")
          isoprocessor::iso_convert_signals(., to = input$scale_signal, quiet = TRUE)
        else .
      } %>%
      isoprocessor::iso_plot_dual_inlet_data(
        data = selector$get_selected(),
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
        data =
          if (length(selector$get_selected()) == 0) NULL
          else if (selector$are_all_selected()) character(0)
          else selector$get_selected(),
        aes_options = get_function_parameter_values()[!get_function_parameter_is_default()],
        theme_options = base_plot$get_theme_options(),
        rmarkdown = rmarkdown
      )

    }
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
        selectorTableUI(ns("selector")),
        footer = div(
          selectorTableButtons(ns("selector")),
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

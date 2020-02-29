#' generic plot server
#' @param get_variable get variable name
#' @param generate_plot function to create the plot
#' @param reset_trigger reactive function to trigger a reset of the plot options (plot height, legend and font size)
plot_server <- function(input, output, session, get_variable, generate_plot, reset_trigger = reactive({})) {

  # namespace
  ns <- session$ns

  # reactive values =====
  values <- reactiveValues(
    reset_plot = 1,
    render_plot = 1,
    has_plot = FALSE
  )

  # remember settings ====
  observeEvent(input$plot_height, {
    module_message(ns, "info", "PLOT user set height to ", input$plot_height)
    set_gui_setting(ns(paste0("plot_height-", get_variable())), input$plot_height)
  })
  observeEvent(input$legend_position, {
    module_message(ns, "info", "PLOT user set legend position to '", input$legend_position, "'")
    set_gui_setting(ns(paste0("legend_position-", get_variable())), input$legend_position)
  })
  observeEvent(input$font_size, {
    module_message(ns, "info", "PLOT user set font size to ", input$font_size)
    set_gui_setting(ns(paste0("font_size-", get_variable())), input$font_size)
  })

  # restore settings =====
  observeEvent(get_variable(), {
    req(get_variable())
    updateNumericInput(
      session, "plot_height",
      value = get_gui_setting(ns(paste0("plot_height-", get_variable())))
    )
  })
  observeEvent(get_variable(), {
    req(get_variable())
    updateSelectInput(
      session, "legend_position",
      selected = get_gui_setting(ns(paste0("legend_position-", get_variable())))
    )
  })
  observeEvent(get_variable(), {
    req(get_variable())
    updateNumericInput(
      session, "font_size",
      value = get_gui_setting(ns(paste0("font_size-", get_variable())))
    )
  })

  # reset settings
  observeEvent(reset_trigger(), {
    updateNumericInput(session, "plot_height", value = 500)
    updateSelectInput(session, "legend_position", selected = "right")
    updateNumericInput(session, "font_size", value = 18)
  })

  # reset plot =====
  reset_plot <- function() {
    module_message(ns, "info", "PLOT resetting plot")
    values$reset_plot <- values$reset_plot + 1
    values$has_plot <- FALSE
  }
  # reset with change in dataset
  observeEvent(get_variable(), reset_plot())

  # render plot ======
  render_plot <- function() {
    module_message(ns, "info", "PLOT rendering plot")
    values$render_plot <- values$render_plot + 1
    values$has_plot <- TRUE
  }
  # render on click
  observeEvent(input$render_plot, render_plot())

  # disable/enable buttons =====
  observeEvent(values$has_plot , {
    shinyjs::toggleState("download_dialog", condition = values$has_plot)
  })

  # generate plot ====
  generate_full_plot <- reactive({
    req(p <- generate_plot())
    # font size
    p <- p + theme(text = element_text(size = input$font_size))
    # legend position
    if (input$legend_position == "bottom") {
      p <- p + theme(legend.position = "bottom", legend.direction="vertical")
    } else if (input$legend_position == "hide") {
      p <- p + theme(legend.position = "none")
    }
    return(p)
  })

  # plot output =====
  output$plot <- renderPlot({
    # trigger new plot on reset and render
    values$reset_plot
    values$render_plot
    validate(need(values$has_plot, "Please select plot parameters and click on 'Plot' to generate the plot."))
    isolate(generate_full_plot())
  },
  height = reactive({
    # trigger plot height change only on render
    values$render_plot
    isolate(input$plot_height)
  }))

  # plot save dialog =====
  save_dialog <- reactive({
    file_name <- paste0(get_variable(), ".pdf")
    modalDialog(
      title = "Save plot", fade = FALSE, easyClose = TRUE, size = "s",
      textInput(ns("save_name"), "Filename:", file_name),
      numericInput(ns("save_width"), "Width [inches]:", 12),
      numericInput(ns("save_height"), "Height [inches]:", 8),
      footer =
        tagList(
          downloadButton(ns("download"), label = "Download", icon = icon("download")),
          modalButton("Close")
        )
    )})
  observeEvent(input$download_dialog, showModal(save_dialog()))

  # download handler =====
  output$download <- downloadHandler(
    filename = function() { isolate(input$save_name) },
    content = function(filename) {
      isolate({
        module_message(ns, "info", "PLOT saving ", input$save_name, " (", input$save_width, " by ", input$save_height, ")")
        ggsave(file = filename, plot = generate_full_plot(), width = input$save_width, height = input$save_height, device = "pdf")
      })
    })

  # legend position code ====
  get_theme_options <- reactive({
    theme_options <- list(text = rlang::expr(element_text(size = !!input$font_size)))
    if (input$legend_position == "bottom") {
      theme_options <- c(theme_options,
                        legend.position = "bottom",
                        legend.direction = "vertical")
    } else if (input$legend_position == "hide") {
      theme_options <- c(theme_options, legend.position = "none")
    }
    return(theme_options)
  })

  # return functions =====
  list(
    render_plot = render_plot,
    has_plot = reactive({ values$has_plot }),
    dblclick = reactive({ input$plot_dblclick }),
    click = reactive({ input$plot_click }),
    brush = reactive({ input$plot_brush }),
    get_theme_options = get_theme_options
  )
}


#' plot UI
#' @param brush_direction if NULL no brush, if "x", "y", or "xy", creates a brush
plot_ui <- function(
  id, min_height = "500px;",
  action_widths = c(3, 6, 3),
  left_actions = list(),  center_actions = list(), right_actions = list(),
  click = FALSE, dblclick = FALSE, brush_direction = NULL) {

  # namespace
  ns <- NS(id)

  if (!is.null(brush_direction)) {
    brush <- brushOpts(
      id = ns("plot_brush"),
      delay = 10000, # ms (basically let the user finish the brush themselves)
      delayType = "debounce",
      direction = brush_direction,
      resetOnNew = TRUE
    )
  } else {
    brush <- NULL
  }

  tagList(
      # plot actions
      div(id = ns("plot_actions"),
          fluidRow(
            column(width = action_widths[1], aligh = "left", left_actions),
            column(width = action_widths[2], align = "center", center_actions),
            column(width = action_widths[3], align = "right",
                   tooltipInput(actionButton, ns("render_plot"), "Plot", icon = icon("refresh"),
                                tooltip = "Render the plot with the selected files and parameters."),
                   spaces(1),
                   tooltipInput(actionButton, ns("download_dialog"), "Save", icon = icon("download"),
                                tooltip = "Download the plot as a PDF") %>%
                     shinyjs::disabled(),
                   right_actions
            )
          )
      ),
      div(id = ns("plot_div"), style = paste("min-height:", min_height),
          plotOutput(ns("plot"), height = "100%",
                     dblclick = if (dblclick) ns("plot_dblclick") else NULL,
                     click = if (click) ns("plot_click") else NULL,
                     brush = brush) %>%
            withSpinner(type = 5, proxy.height = min_height)
      )
  )
}

#' plot height ui
plot_height_ui <- function(id, label = "Plot height:", label_width = 4, input_width = 8) {
  ns <- NS(id)
  fluidRow(
    h4(label) %>% column(width = label_width),
    numericInput(ns("plot_height"), NULL, value = 500, min = 100, step = 50) %>%
      column(width = input_width)
  )
}

#' legend ui
plot_legend_ui <- function(id, label = "Legend:", label_width = 4, input_width = 8) {
  ns <- NS(id)
  fluidRow(
    h4(label) %>% column(width = label_width),
    selectInput(ns("legend_position"), NULL, choices = c("right", "bottom", "hide"), selected = "right") %>%
      column(width = input_width)
  )
}

#' font size
plot_font_size_ui <- function(id, label = "Font Size:", label_width = 4, input_width = 8) {
  ns <- NS(id)
  fluidRow(
    h4(label) %>% column(width = label_width),
    numericInput(ns("font_size"), NULL, value = 18) %>%
      column(width = input_width)
  )
}

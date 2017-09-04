# TODO: implement zooming on the chromatograms

#' Continuous Flow Raw Data
#' @inheritParams fileInfoServer
#' @family continuous flow raw data module functions
cfRawDataServer <- function(input, output, session, isofiles, dataset_name, visible = NULL) {

  # namespace
  ns <- session$ns

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

  # visibility of plot buttons  ====
  observe({
    toggle("plot_actions", condition =
             length(isofiles()) > 0 & length(mass_ratio_selector$get_selected()) > 0)
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

  generate_plot <- reactive({
    # immediate updates
    validate(
      need(length(isofiles()) > 0, "Please select a dataset and at least one data file.") %then%
        need(length(mass_ratio_selector$get_selected()) > 0, "Please select at least one mass or ratio.")
    )

    # onclick updates
    input$refresh
    input$scale_signal
    input$scale_time

    # rest is isolated
    isolate({
      module_message(ns, "debug", "rendering continuous flow raw data plot")

      # prep data
      plot_isofiles <- isofiles()
      if (input$scale_signal != "<NONE>") {
        plot_isofiles <- convert_signals(plot_isofiles, to = input$scale_signal)
      }
      if (input$scale_time != "<NONE>") {
        plot_isofiles <- convert_time(plot_isofiles, to = input$scale_time)
      }
      if (length(get_ratios()) > 0) {
        plot_isofiles <- calculate_ratios(plot_isofiles, get_ratios())
      }

      # plot data
      p <- do.call(plot_raw_data, args =
                c(list(isofiles = plot_isofiles, data = mass_ratio_selector$get_selected()),
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
    height = reactive({ input$refresh; isolate(input$plot_height) }))

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
          plot_params = get_plot_params(),
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
      div(align = "right", id = ns("plot_actions"),
          plotDownloadLink(ns("plot_download"))
      ) %>% hidden(),
      plotOutput(ns("plot"), height = "100%") %>%
        withSpinner(type = 5, proxy.height = "450px")
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
          selectInput(ns("scale_time"), NULL, choices = time_options) %>% column(width = 8)),
        selectorTableUI(ns("selector"), height = selector_height),
        footer = div(style = "height: 35px;", selectorTableButtons(ns("selector")))
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
        footer = tooltipInput(actionButton, ns("refresh"), label = "Apply", icon = icon("refresh"),
                              tooltip = "Refresh plot with new plot settings.")
      )
  )%>% hidden()
}

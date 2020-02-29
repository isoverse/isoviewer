#' function plot parameter server
#' @param get_variable reactive function for the variable
#' @param type what type of parameter it is
#' @param get_value_options reactive function with available value options
#' @param func_formals formals of the relevant function (to determine default)
#' @param reset_trigger reactive function to trigger a reset to the default value (if necessary)
function_plot_param_server <- function(
  input, output, session, get_variable, type = c("expression", "logical"),
  get_value_options, default_value, reset_trigger = reactive({})) {

  # namespace
  ns <- session$ns

  # default value =====
  if (type == "expression") {
    default_value <- rlang::quo_text(rlang::expr(!!default_value))
  } else if (type == "logical") {
    stopifnot(is.logical(default_value))
  } else {
    stop("unsupported type: ", type, call. = FALSE)
  }

  # reactive values =====
  values <- reactiveValues(
    value = default_value,
    is_default = TRUE
  )

  # update value when switching dataset ====
  observeEvent(get_variable(), {
    if (type == "expression") {
      updateSelectInput(
        session, "value", choices = get_value_options(),
        selected = get_gui_setting(ns(get_variable()), default = default_value)
      )
    } else if (type == "logical") {
      updateCheckboxInput(
        session, "value",
        value = get_gui_setting(ns(get_variable()), default = default_value)
      )
    }
  })

  # update options if they change =====
  observeEvent(get_value_options(), {
    if (type == "expression") {
      updateSelectInput(
        session, "value", choices = get_value_options(),
        selected = get_gui_setting(ns(get_variable()), default = default_value)
      )
    }
  })

  # reset value ====
  observeEvent(reset_trigger(), {
    if (type == "expression") {
      updateSelectInput(session, "value", selected = default_value)
    } else if (type == "logical") {
      updateCheckboxInput(session, "value", value = default_value)
    }
  })

  # store value =====
  observeEvent(input$value, {
    req(!is.null(input$value))
    # update message
    module_message(
      ns, "info",
      sprintf("PLOT PARAM set to '%s' for '%s'", input$value, get_variable())
    )
    # update settings
    set_gui_setting(ns(get_variable()), input$value)

    # process & store value
    value <- input$value
    values$is_default <- default_value == value
    if (type == "expression") {
      # deal with expression
      if (value == "NULL") {
        value <- rlang::expr(NULL)
      } else {
        value <- rlang::sym(value)
      }
    }
    values$value <- value
  })

  # generate ui output =====
  output$ui <- renderUI({
    if (type == "expression") {
      selectInput(
        ns("value"), NULL,
        choices = get_value_options(),
        selected = get_gui_setting(ns(get_variable()), default = default_value)
      )
    } else if (type == "logical") {
      checkboxInput(
        ns("value"), NULL,
        value = get_gui_setting(ns(get_variable()), default = default_value)
      )
    }
  })

  # return functions =====
  list(
    is_default = reactive({ values$is_default }),
    get_value = reactive({ values$value })
  )

}


#' function plot parameter UI
function_plot_param_ui <- function(id, label, label_width = 4, value_width = 8) {
  ns <- NS(id)
  fluidRow(
    h4(label) %>% column(width = label_width),
    uiOutput(ns("ui")) %>% column(width = value_width)
  )
}

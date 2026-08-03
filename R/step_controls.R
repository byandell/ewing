#' Simulation Step Size Slider and Parsing Controls
#'
#' Centralized utilities for discrete simulation step controls offering logarithmic
#' step choices (1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000), axis units selection modules (Steps vs Time/Days),
#' and Age Classes display control modules.
#'
#' @param inputId Shiny input ID string
#' @param id Module ID string
#' @param label Slider label string (default: `"Steps per click:"`)
#' @param time_label Time unit label string for radio button display (default: `"Time"`, e.g. `"Days"`)
#' @param selected Default selected step value or choice
#' @param val Input value from slider to parse
#'
#' @export
#' @rdname step_controls
#' @importFrom shiny sliderInput radioButtons checkboxInput tagList NS moduleServer reactive
step_size_choices <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)

#' @export
#' @rdname step_controls
step_size_slider <- function(inputId, label = "Steps per click:", selected = 50) {
  idx <- match(selected, step_size_choices)
  if (is.na(idx)) idx <- 6
  sl <- shiny::sliderInput(inputId, label, min = 1, max = length(step_size_choices), value = idx, step = 1, ticks = TRUE)
  sl$children[[2]]$attribs[['data-values']] <- paste(step_size_choices, collapse = ",")
  sl
}

#' @export
#' @rdname step_controls
parse_step_size <- function(val) {
  if (is.null(val)) return(50)
  num <- round(as.numeric(val))
  if (is.na(num)) return(50)
  
  # Direct large values (e.g., 20, 50, 100, 200, 500, 1000, 2000)
  if (num %in% step_size_choices && num > 10) {
    return(num)
  }
  
  # 0-based JavaScript index from ion.rangeSlider (0 to 10)
  if (num >= 0 && num < length(step_size_choices)) {
    return(step_size_choices[num + 1])
  }
  
  # Fallback for direct value
  if (num %in% step_size_choices) {
    return(num)
  }
  
  50
}

#' Axis Units Selector UI Module
#' @export
#' @rdname step_controls
axisUnitInput <- function(id, time_label = "Time", selected = "step") {
  ns <- shiny::NS(id)
  choices <- c("Steps" = "step")
  choices[time_label] <- "time"
  shiny::radioButtons(ns("x_var"), "Display Units:",
                      choices = choices,
                      selected = selected, inline = TRUE)
}

#' Axis Units Selector Server Module
#' @export
#' @rdname step_controls
axisUnitServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({ if (!is.null(input$x_var)) input$x_var else "step" })
  })
}

#' Age Classes Display Controls UI Module
#' @export
#' @rdname step_controls
ageClassControlInput <- function(id, time_label = "Time") {
  ns <- shiny::NS(id)
  shiny::tagList(
    axisUnitInput(ns("axis_unit"), time_label = time_label),
    shiny::checkboxInput(ns("norm"), "Normalize Plot", TRUE),
    shiny::checkboxInput(ns("total"), "Include Total in Plot", TRUE)
  )
}

#' Age Classes Display Controls Server Module
#' @export
#' @rdname step_controls
ageClassControlServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    x_var <- axisUnitServer("axis_unit")
    list(
      x_var = x_var,
      norm = shiny::reactive({ if (!is.null(input$norm)) input$norm else TRUE }),
      total = shiny::reactive({ if (!is.null(input$total)) input$total else TRUE })
    )
  })
}

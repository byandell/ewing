#' Interactive Daily Temperature Design Explorer App
#' 
#' Shiny application to interactively adjust daily high and low temperature splines
#' and visualize degree-day accumulation dynamics for an \code{ewing} simulation.
#' 
#' @param community Simulation object of class \code{ewing} (default: \code{init.simulation()})
#' @param title Application title
#' @export
#' @importFrom shiny shinyApp fluidPage tags div h3 h4 p fluidRow column hr
#' @importFrom shiny reactive renderPlot renderText req selectizeInput sliderInput numericInput textInput textOutput verbatimTextOutput observeEvent updateSliderInput updateNumericInput validate need uiOutput renderUI tagList updateTextInput radioButtons actionButton reactiveVal reactiveValues
#' @importFrom bslib page_sidebar sidebar card card_body card_header bs_theme
#' @importFrom splines interpSpline
#' @importFrom stats predict
tempApp <- function(community = NULL, title = "Daily Temperature Design Explorer") {
  
  # Initialize baseline community simulation if not supplied
  if (is.null(community)) {
    community <- init.simulation(messages = FALSE)
  }
  
  # Curated modern light color scheme
  app_theme <- bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#212529",
    primary = "#1a73e8",
    secondary = "#7209b7",
    success = "#2ec4b6"
  )
  
  # Helper to extract knots and y-values from a spline object
  get_spline_coords <- function(comm, element = "High") {
    sp <- getTemp(comm, element)
    if (is.null(sp)) {
      tmp <- seq(0, 60, length = 8)
      sp <- if (element == "Low") {
        splines::interpSpline(tmp, 60 + 0.125 * tmp + sin(0.25 * tmp))
      } else {
        splines::interpSpline(tmp, 70 + 0.15 * tmp + sin((pi / 8) + 0.225 * tmp))
      }
    }
    knots <- sp$knots
    yvals <- stats::predict(sp, knots)$y
    list(x = knots, y = yvals, spline = sp)
  }
  
  init_high <- get_spline_coords(community, "High")
  init_low  <- get_spline_coords(community, "Low")
  init_min  <- getTemp(community, "Min")
  if (is.null(init_min) || is.na(init_min)) init_min <- 50
  
  ui <- bslib::page_sidebar(
    title = title,
    theme = app_theme,
    
    sidebar = bslib::sidebar(
      width = 300,
      shiny::div(
        style = "font-size: 0.85rem; padding: 5px 0;",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
          shiny::span("Active Curve:", style = "font-weight: bold; color: #1a73e8;"),
          shiny::radioButtons(
            "active_curve", NULL,
            choices = c("High" = "High", "Low" = "Low"),
            selected = "High",
            inline = TRUE
          )
        ),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::div(
          style = "margin-bottom: 6px;",
          shiny::span("Day Knots (X):", style = "font-weight: 600; color: #7209b7; display: block; margin-bottom: 2px;"),
          shiny::textInput("x_coords", NULL, value = paste(round(init_high$x, 1), collapse = ", "))
        ),
        shiny::div(
          style = "margin-bottom: 6px;",
          shiny::span("Temperatures °F (Y):", style = "font-weight: 600; color: #7209b7; display: block; margin-bottom: 2px;"),
          shiny::textInput("y_coords", NULL, value = paste(round(init_high$y, 1), collapse = ", "))
        ),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::div(
          style = "display: flex; gap: 8px; align-items: flex-end; margin-top: 6px;",
          shiny::div(
            style = "flex: 1;",
            shiny::span("Min Temp:", style = "font-weight: 600; color: #2ec4b6; display: block; margin-bottom: 2px;"),
            shiny::numericInput("min_temp", NULL, value = init_min, min = 0, max = 120, step = 1)
          ),
          shiny::actionButton("reset_btn", "Reset", class = "btn-outline-secondary btn-sm", style = "height: 38px; margin-bottom: 0px;")
        )
      )
    ),
    
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600;700&display=swap"),
      shiny::tags$style(shiny::HTML("
        body {
          background-color: #f8f9fa;
          font-family: 'Outfit', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        }
        .card {
          background: #ffffff !important;
          border: 1px solid rgba(0, 0, 0, 0.08) !important;
          border-radius: 12px !important;
          box-shadow: 0 4px 20px 0 rgba(0, 0, 0, 0.05);
          transition: all 0.3s ease;
          margin-bottom: 20px;
        }
        .card:hover {
          border-color: rgba(26, 115, 232, 0.3) !important;
        }
        .card-header {
          background: rgba(0, 0, 0, 0.02) !important;
          border-bottom: 1px solid rgba(0, 0, 0, 0.08) !important;
          font-weight: bold;
        }
        .sidebar {
          background: #ffffff !important;
          border-right: 1px solid rgba(0, 0, 0, 0.08) !important;
        }
      "))
    ),
    
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Daily High & Low Temperature Splines (Click plot to move nodes)"),
          bslib::card_body(
            shiny::plotOutput("plot_temp", height = "500px", click = "temp_click")
          )
        )
      ),
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Degree-Day & Temperature Dynamics: temp.plot()"),
          bslib::card_body(
            shiny::plotOutput("plot_degreeday", height = "500px")
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        bslib::card(
          bslib::card_header("Instructions & Temperature Regime Overview"),
          bslib::card_body(
            shiny::HTML("
              <p><strong>Instructions:</strong> Select the curve you wish to modify (<em>High Temperature</em> or <em>Low Temperature</em>) from the sidebar. Click directly on the <em>Daily High & Low Temperature Splines</em> plot to move individual knot coordinates. The coordinates will update in real time while maintaining strict day monotonicity.</p>
              <p><strong>Background:</strong> In the <strong>ewing</strong> Quantitative Population Ethology framework, stage transitions and rate dynamics depend on degree-days (DD). Daily high and low temperature curves are modeled as cubic splines over time. Updating high/low temperatures dynamically recalculates degree-day accumulation via <code>activeTemp()</code>.</p>
              <ul style='padding-left: 20px; font-size: 0.9em;'>
                <li><strong style='color: #dc3545;'>High Temperature (Red):</strong> Peak temperature expected each day.</li>
                <li><strong style='color: #0d6efd;'>Low Temperature (Blue):</strong> Minimum temperature expected each day.</li>
                <li><strong>Min Temp Threshold:</strong> Base thermal threshold below which degree-days do not accumulate.</li>
              </ul>
            ")
          )
        )
      )
    )
  )
  
  server = function(input, output, session) {
    
    # Store state for High and Low curves
    state <- shiny::reactiveValues(
      high_x = init_high$x,
      high_y = init_high$y,
      low_x  = init_low$x,
      low_y  = init_low$y,
      current_active = "High"
    )
    
    # Sync text inputs when active curve changes or state is updated
    sync_text_inputs <- function() {
      if (state$current_active == "High") {
        shiny::updateTextInput(session, "x_coords", value = paste(round(state$high_x, 2), collapse = ", "))
        shiny::updateTextInput(session, "y_coords", value = paste(round(state$high_y, 2), collapse = ", "))
      } else {
        shiny::updateTextInput(session, "x_coords", value = paste(round(state$low_x, 2), collapse = ", "))
        shiny::updateTextInput(session, "y_coords", value = paste(round(state$low_y, 2), collapse = ", "))
      }
    }
    
    # Observe active curve toggle
    shiny::observeEvent(input$active_curve, {
      state$current_active <- input$active_curve
      sync_text_inputs()
    }, ignoreInit = TRUE)
    
    # Reset button handler
    shiny::observeEvent(input$reset_btn, {
      state$high_x <- init_high$x
      state$high_y <- init_high$y
      state$low_x  <- init_low$x
      state$low_y  <- init_low$y
      shiny::updateNumericInput(session, "min_temp", value = init_min)
      sync_text_inputs()
    })
    
    # Handle click on temperature plot to reposition closest node
    shiny::observeEvent(input$temp_click, {
      cx <- input$temp_click$x
      cy <- input$temp_click$y
      
      x_curr <- if (state$current_active == "High") state$high_x else state$low_x
      y_curr <- if (state$current_active == "High") state$high_y else state$low_y
      
      n <- length(x_curr)
      if (n < 3) return()
      
      # Find nearest node in normalized distance
      xr <- max(x_curr) - min(x_curr)
      yr <- max(y_curr) - min(y_curr)
      if (xr == 0) xr <- 1
      if (yr == 0) yr <- 1
      
      dists <- ((x_curr - cx) / xr)^2 + ((y_curr - cy) / yr)^2
      idx <- which.min(dists)
      
      # Monotonicity bounds for Day (X)
      min_x <- if (idx == 1) x_curr[1] else x_curr[idx - 1] + 0.01
      max_x <- if (idx == n) x_curr[n] else x_curr[idx + 1] - 0.01
      new_x <- max(min_x, min(max_x, cx))
      
      x_curr[idx] <- new_x
      y_curr[idx] <- cy
      
      if (state$current_active == "High") {
        state$high_x <- x_curr
        state$high_y <- y_curr
      } else {
        state$low_x  <- x_curr
        state$low_y  <- y_curr
      }
      
      sync_text_inputs()
    })
    
    # Parse text inputs when user edits text directly
    shiny::observeEvent(list(input$x_coords, input$y_coords), {
      shiny::req(input$x_coords, input$y_coords)
      
      x_vals <- tryCatch(as.numeric(trimws(strsplit(input$x_coords, ",")[[1]])), error = function(e) NA)
      y_vals <- tryCatch(as.numeric(trimws(strsplit(input$y_coords, ",")[[1]])), error = function(e) NA)
      
      if (length(x_vals) >= 3 && length(x_vals) == length(y_vals) && 
          !any(is.na(x_vals)) && !any(is.na(y_vals)) && all(diff(x_vals) > 0)) {
        if (state$current_active == "High") {
          state$high_x <- x_vals
          state$high_y <- y_vals
        } else {
          state$low_x  <- x_vals
          state$low_y  <- y_vals
        }
      }
    })
    
    # Rebuild simulation community with active splines
    sim_reactive <- shiny::reactive({
      shiny::req(input$min_temp)
      
      # Validate coordinate lengths and monotonicity
      shiny::validate(
        shiny::need(length(state$high_x) == length(state$high_y), "High temp X & Y must have equal length."),
        shiny::need(length(state$low_x)  == length(state$low_y),  "Low temp X & Y must have equal length."),
        shiny::need(length(state$high_x) >= 3, "High temp spline requires at least 3 nodes."),
        shiny::need(length(state$low_x)  >= 3, "Low temp spline requires at least 3 nodes."),
        shiny::need(all(diff(state$high_x) > 0), "High temp Day knots must be strictly increasing."),
        shiny::need(all(diff(state$low_x)  > 0), "Low temp Day knots must be strictly increasing.")
      )
      
      tryCatch({
        sp_high <- splines::interpSpline(state$high_x, state$high_y)
        sp_low  <- splines::interpSpline(state$low_x,  state$low_y)
        
        comm <- community
        comm <- setTemp(comm, "High", sp_high)
        comm <- setTemp(comm, "Low",  sp_low)
        comm <- setTemp(comm, "Min",  input$min_temp)
        
        # Calculate active temperature degree-day integration
        comm <- activeTemp(comm, messages = FALSE)
        
        list(community = comm, high_spline = sp_high, low_spline = sp_low)
      }, error = function(e) {
        shiny::validate(paste("Error constructing temperature splines:", e$message))
      })
    })
    
    # Render daily high & low temperature splines plot
    output$plot_temp <- shiny::renderPlot({
      res <- sim_reactive()
      shiny::req(res)
      
      high_sp <- res$high_spline
      low_sp  <- res$low_spline
      
      x_range <- range(c(state$high_x, state$low_x))
      pred_x  <- seq(x_range[1], x_range[2], length.out = 200)
      
      pred_high <- stats::predict(high_sp, pred_x)$y
      pred_low  <- stats::predict(low_sp,  pred_x)$y
      
      y_range <- range(c(pred_high, pred_low, state$high_y, state$low_y))
      
      graphics::par(bg = "white", col.axis = "#495057", col.lab = "#212529", col.main = "#1a73e8", fg = "#cccccc")
      graphics::plot(pred_x, pred_high, type = "l", col = "#dc3545", lwd = 3,
                     xlim = x_range, ylim = y_range,
                     xlab = "Day", ylab = "Temperature (°F)",
                     main = "Daily High & Low Temperature Splines",
                     panel.first = graphics::grid(col = "#e9ecef", lty = 1))
      
      graphics::lines(pred_x, pred_low, col = "#0d6efd", lwd = 3)
      
      # Plot nodes
      graphics::points(state$high_x, state$high_y, col = "#dc3545", pch = 19, cex = 1.6)
      graphics::points(state$low_x,  state$low_y,  col = "#0d6efd", pch = 19, cex = 1.6)
      
      # Highlight nodes for currently active curve
      if (state$current_active == "High") {
        graphics::points(state$high_x, state$high_y, col = "#7209b7", pch = 1, cex = 2.6, lwd = 2)
      } else {
        graphics::points(state$low_x,  state$low_y,  col = "#7209b7", pch = 1, cex = 2.6, lwd = 2)
      }
      
      graphics::legend("topright", legend = c("High Temp", "Low Temp", "Active Knots"),
                       col = c("#dc3545", "#0d6efd", "#7209b7"),
                       lty = c(1, 1, NA), pch = c(19, 19, 1), lwd = c(2, 2, 2), bty = "n")
    })
    
    # Render degree-day calculation plot using temp.plot()
    output$plot_degreeday <- shiny::renderPlot({
      res <- sim_reactive()
      shiny::req(res)
      
      graphics::par(bg = "white", col.axis = "#495057", col.lab = "#212529", col.main = "#2ec4b6", fg = "#cccccc")
      temp.plot(res$community, main = "Degree-Day Dynamics over Time")
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

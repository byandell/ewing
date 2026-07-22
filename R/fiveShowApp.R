#' Interactive Spline Goal Explorer App
#' 
#' Shiny application to interactively explore and search for parameter adjustments using \code{five.show}.
#' 
#' @param title Application title
#' @export
#' @importFrom shiny shinyApp fluidPage tags div h3 h4 p fluidRow column hr
#' @importFrom shiny reactive renderPlot renderText req selectizeInput sliderInput numericInput textInput textOutput verbatimTextOutput observeEvent updateSliderInput updateNumericInput validate need uiOutput renderUI tagList updateTextInput
#' @importFrom bslib page_sidebar sidebar navset_tab nav_panel card card_body card_header bs_theme font_google
#' @importFrom splines interpSpline backSpline
#' @importFrom stats predict
#' @importFrom grDevices pdf dev.off
#' @importFrom utils capture.output
fiveShowApp <- function(title = "Spline 5-Parameter Goal Explorer") {
  
  # Curated modern light color scheme
  app_theme <- bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#212529",
    primary = "#1a73e8",
    secondary = "#7209b7",
    success = "#2ec4b6"
  )
  
  ui <- bslib::page_sidebar(
    title = title,
    theme = app_theme,
    
    sidebar = bslib::sidebar(
      width = 350,
      shiny::h4("1. Baseline Spline", style = "color: #1a73e8; font-weight: bold; margin-bottom: 15px;"),
      shiny::p("Click directly on the left plot to adjust the nodes of the baseline curve. You can also manually edit the coordinate numbers below.",
               style = "font-size: 0.9em; color: #495057; margin-bottom: 15px;"),
      shiny::textInput("x_coords", "X Coordinates (comma separated):", 
                       value = "0.000, 0.153, 0.334, 0.555, 0.838, 1.236, 1.906, 5.000"),
      shiny::textInput("y_coords", "Y Coordinates (comma separated):", 
                       value = "0.000, 0.200, 0.400, 0.700, 1.100, 1.600, 2.400, 5.000"),
      
      shiny::hr(style = "border-top: 1px solid rgba(0, 0, 0, 0.1);"),
      
      shiny::h4("2. Goal Target", style = "color: #7209b7; font-weight: bold; margin-bottom: 15px;"),
      shiny::sliderInput("goal", "Target Relative Mean (five.show):", 
                         min = 0.5, max = 1.5, value = 0.9, step = 0.05),
      
      shiny::hr(style = "border-top: 1px solid rgba(0, 0, 0, 0.1);"),
      shiny::p("Ewing QPE Simulation Package", style = "font-size: 0.85em; color: rgba(33, 37, 41, 0.5);")
    ),
    
    # Custom CSS style block for premium light aesthetics
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;600;700&display=swap"),
      shiny::tags$style(shiny::HTML("
        body {
          background-color: #f8f9fa;
          color: #212529;
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
        .control-label {
          font-weight: 500;
          color: #495057;
        }
        .form-control, .selectize-input {
          background-color: #ffffff !important;
          border: 1px solid rgba(0, 0, 0, 0.15) !important;
          color: #212529 !important;
        }
        .form-control:focus, .selectize-input.focus {
          border-color: #1a73e8 !important;
          box-shadow: 0 0 0 0.25rem rgba(26, 115, 232, 0.25) !important;
        }
        pre {
          background: #f1f3f4 !important;
          border: 1px solid rgba(0, 0, 0, 0.08);
          border-radius: 8px;
          color: #7209b7 !important;
          padding: 15px;
          font-family: 'Courier New', Courier, monospace;
          white-space: pre-wrap;
          font-size: 0.9em;
        }
      "))
    ),
    
    # Main UI body - two plots side-by-side
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Interactive Baseline Spline (Click plot to move nodes)"),
          bslib::card_body(
            shiny::plotOutput("plot_baseline", height = "500px", click = "baseline_click")
          )
        )
      ),
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Goal Comparison Plot: five.show()"),
          bslib::card_body(
            shiny::plotOutput("plot_goal", height = "500px")
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Binary Search Console Output"),
          bslib::card_body(
            shiny::verbatimTextOutput("show_console")
          )
        )
      ),
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Goal Search Explanation & Parameters"),
          bslib::card_body(
            shiny::HTML("
              <p><strong>Goal Search:</strong> The <code>five.show()</code> function performs a separate binary search for each of the 5 parameters to find the value that yields the target relative mean time compared to the baseline spline (e.g. 90% or 110% of baseline mean).</p>
              <p>The console output shows the values found during this search:</p>
              <ul style='padding-left: 20px; font-size: 0.95em; line-height: 1.5em;'>
                <li><strong>dispersion:</strong> Stretches/compresses the time axis to match the target.</li>
                <li><strong>location:</strong> Shifts the time axis additively to match the target.</li>
                <li><strong>intensity:</strong> Modifies event velocity to match the target (shown as inverse velocity).</li>
                <li><strong>truncation:</strong> Crops early-stage probability to match the target.</li>
                <li><strong>rejection:</strong> Disallows late-stage transitions to match the target.</li>
              </ul>
              <p>If a parameter cannot achieve the target mean (e.g. a rejection threshold of 1 still yields a mean higher than the target), it returns <code>NA</code>.</p>
            ")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Reactive fit object from input coordinates
    fit_reactive <- shiny::reactive({
      shiny::req(input$x_coords, input$y_coords)
      
      # Parse text inputs
      x_vals <- as.numeric(trimws(strsplit(input$x_coords, ",")[[1]]))
      y_vals <- as.numeric(trimws(strsplit(input$y_coords, ",")[[1]]))
      
      # Validations
      shiny::validate(
        shiny::need(length(x_vals) == length(y_vals), "Error: X and Y coordinate lists must be of equal length."),
        shiny::need(length(x_vals) >= 3, "Error: Please specify at least 3 coordinate points."),
        shiny::need(!any(is.na(x_vals)) && !any(is.na(y_vals)), "Error: All coordinates must be numeric values."),
        shiny::need(all(diff(x_vals) > 0), "Error: X coordinates must be strictly increasing."),
        shiny::need(all(diff(y_vals) > 0), "Error: Y coordinates must be strictly increasing.")
      )
      
      # Try building interpSpline and backSpline
      tryCatch({
        meanvalue <- splines::interpSpline(x_vals, y_vals)
        invmvalue <- splines::backSpline(meanvalue)
        list(meanvalue = meanvalue, invmvalue = invmvalue, xy = data.frame(x = x_vals, y = y_vals))
      }, error = function(e) {
        shiny::validate(
          paste("Error building spline:", e$message, 
                "\nNote: The spline must be strictly monotonic (always increasing) to calculate its backspline.")
        )
      })
    })
    
    # Handle click on baseline plot to move closest point
    shiny::observeEvent(input$baseline_click, {
      cx <- input$baseline_click$x
      cy <- input$baseline_click$y
      
      # Parse current inputs to find closest point
      x_current <- as.numeric(trimws(strsplit(input$x_coords, ",")[[1]]))
      y_current <- as.numeric(trimws(strsplit(input$y_coords, ",")[[1]]))
      
      if (length(x_current) < 3 || any(is.na(x_current)) || any(is.na(y_current))) return()
      
      n <- length(x_current)
      
      # Calculate closest point index in normalized Euclidean space
      x_range <- max(x_current) - min(x_current)
      y_range <- max(y_current) - min(y_current)
      if (x_range == 0) x_range <- 1
      if (y_range == 0) y_range <- 1
      
      dists <- ((x_current - cx) / x_range)^2 + ((y_current - cy) / y_range)^2
      closest_idx <- which.min(dists)
      
      # Determine monotonicity boundaries for selected node
      # Bound X:
      min_x <- if (closest_idx == 1) x_current[1] else x_current[closest_idx - 1] + 0.005
      max_x <- if (closest_idx == n) x_current[n] else x_current[closest_idx + 1] - 0.005
      new_x <- max(min_x, min(max_x, cx))
      
      # Bound Y:
      min_y <- if (closest_idx == 1) y_current[1] else y_current[closest_idx - 1] + 0.005
      max_y <- if (closest_idx == n) y_current[n] else y_current[closest_idx + 1] - 0.005
      new_y <- max(min_y, min(max_y, cy))
      
      # Update coordinates array
      x_current[closest_idx] <- new_x
      y_current[closest_idx] <- new_y
      
      # Update text inputs
      shiny::updateTextInput(session, "x_coords", value = paste(round(x_current, 3), collapse = ", "))
      shiny::updateTextInput(session, "y_coords", value = paste(round(y_current, 3), collapse = ", "))
    })
    
    # Render baseline spline preview
    output$plot_baseline <- shiny::renderPlot({
      fit_obj <- fit_reactive()
      shiny::req(fit_obj)
      
      # Generate predictions for smooth curve plotting
      pred_x <- seq(min(fit_obj$xy$x), max(fit_obj$xy$x), length.out = 150)
      pred_y <- stats::predict(fit_obj$meanvalue, pred_x)$y
      
      # Custom light plot styling
      graphics::par(bg = "white", col.axis = "#495057", col.lab = "#212529", col.main = "#1a73e8", fg = "#cccccc")
      graphics::plot(pred_x, pred_y, type = "l", col = "#1a73e8", lwd = 3,
                     xlab = "Time (X)", ylab = "Probability scale (Y)", 
                     main = "Interactive Baseline Mean-Value Spline",
                     panel.first = graphics::grid(col = "#e9ecef", lty = 1))
      
      # Plot nodes
      graphics::points(fit_obj$xy$x, fit_obj$xy$y, col = "#7209b7", pch = 19, cex = 1.8)
      # Draw outer circles as handles
      graphics::points(fit_obj$xy$x, fit_obj$xy$y, col = "#7209b7", pch = 1, cex = 2.8, lwd = 1.5)
    })
    
    # Render multi-parameter goal comparison plot (five.show)
    output$plot_goal <- shiny::renderPlot({
      fit_obj <- fit_reactive()
      shiny::req(fit_obj, input$goal)
      
      # Plotting
      graphics::par(bg = "white", col.axis = "#495057", col.lab = "#212529", col.main = "#7209b7", fg = "#cccccc")
      
      # Run five.show directly
      five.show(fit = fit_obj, goal = input$goal)
    })
    
    # Capture console output of five.show binary search
    output$show_console <- shiny::renderText({
      fit_obj <- fit_reactive()
      shiny::req(fit_obj, input$goal)
      
      # Redirect plots to null device during text capture
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off())
      
      output_lines <- utils::capture.output({
        five.show(fit = fit_obj, goal = input$goal)
      })
      
      paste(output_lines, collapse = "\n")
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

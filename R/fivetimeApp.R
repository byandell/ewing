#' Interactive Spline and Five-Parameter Explorer App
#' 
#' Shiny application to interactively explore and visualize the spline-based time-temperature 
#' relationship and the 5-parameter curve adjustments using \code{five.plot} and \code{five.show}.
#' 
#' @param title Application title
#' @export
#' @importFrom shiny shinyApp fluidPage tags div h3 h4 p fluidRow column hr
#' @importFrom shiny reactive renderPlot renderText req selectizeInput sliderInput numericInput textInput textOutput verbatimTextOutput observeEvent updateSliderInput updateNumericInput validate need
#' @importFrom bslib page_sidebar sidebar navset_tab nav_panel card card_body card_header bs_theme font_google
#' @importFrom splines interpSpline backSpline
#' @importFrom stats predict
#' @importFrom grDevices pdf dev.off
#' @importFrom utils capture.output
fivetimeApp <- function(title = "Spline Time Parameter Explorer") {
  
  # Curated modern light color scheme
  app_theme <- bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#212529",
    primary = "#1a73e8",
    secondary = "#7209b7",
    success = "#2ec4b6",
    base_font = bslib::font_google("Outfit")
  )
  
  ui <- bslib::page_sidebar(
    title = title,
    theme = app_theme,
    
    sidebar = bslib::sidebar(
      width = 350,
      shiny::h4("1. Baseline Spline", style = "color: #1a73e8; font-weight: bold; margin-bottom: 15px;"),
      shiny::textInput("x_coords", "X Coordinates (comma separated):", 
                       value = "0.000, 0.153, 0.334, 0.555, 0.838, 1.236, 1.906, 5.000"),
      shiny::textInput("y_coords", "Y Coordinates (comma separated):", 
                       value = "0.000, 0.153, 0.334, 0.555, 0.838, 1.236, 1.906, 5.000"),
      
      shiny::hr(style = "border-top: 1px solid rgba(0, 0, 0, 0.1);"),
      
      shiny::h4("2. Parameter Sensitivity", style = "color: #2ec4b6; font-weight: bold; margin-bottom: 15px;"),
      shiny::selectizeInput("pick", "Select Parameter (five.plot):", 
                            choices = c("dispersion", "location", "intensity", "truncation", "rejection"),
                            selected = "dispersion"),
      
      shiny::uiOutput("param_value_ui"),
      
      shiny::hr(style = "border-top: 1px solid rgba(0, 0, 0, 0.1);"),
      
      shiny::h4("3. Goal Comparison", style = "color: #7209b7; font-weight: bold; margin-bottom: 15px;"),
      shiny::sliderInput("goal", "Target Relative Mean (five.show):", 
                         min = 0.5, max = 1.5, value = 0.9, step = 0.05),
      
      shiny::hr(style = "border-top: 1px solid rgba(0, 0, 0, 0.1);"),
      shiny::p("Ewing QPE Simulation Package", style = "font-size: 0.85em; color: rgba(33, 37, 41, 0.5);")
    ),
    
    # Custom CSS style block for premium light aesthetics
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body {
          background-color: #f8f9fa;
          color: #212529;
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
        pre {
          background: #f1f3f4 !important;
          border: 1px solid rgba(0, 0, 0, 0.08);
          border-radius: 8px;
          color: #1a73e8 !important;
          padding: 15px;
          font-family: 'Courier New', Courier, monospace;
        }
        .nav-tabs .nav-link {
          color: #495057 !important;
          border: none !important;
          padding: 10px 20px;
          transition: all 0.2s ease;
        }
        .nav-tabs .nav-link.active {
          background-color: rgba(26, 115, 232, 0.08) !important;
          border-bottom: 3px solid #1a73e8 !important;
          color: #1a73e8 !important;
          border-radius: 8px 8px 0 0;
        }
        .nav-tabs .nav-link:hover:not(.active) {
          background-color: rgba(0, 0, 0, 0.04) !important;
          color: #000 !important;
          border-radius: 8px;
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
      "))
    ),
    
    # Main UI body
    bslib::navset_tab(
      bslib::nav_panel(
        "Single Parameter Sensitivity",
        bslib::card(
          bslib::card_header("Sensitivity Analysis: five.plot()"),
          bslib::card_body(
            shiny::plotOutput("plot_sensitivity", height = "500px")
          )
        ),
        bslib::card(
          bslib::card_header("Baseline Spline Curve"),
          bslib::card_body(
            shiny::plotOutput("plot_baseline", height = "300px")
          )
        )
      ),
      bslib::nav_panel(
        "Goal Comparison",
        shiny::fluidRow(
          shiny::column(
            width = 7,
            bslib::card(
              bslib::card_header("Goal Comparison Plot: five.show()"),
              bslib::card_body(
                shiny::plotOutput("plot_goal", height = "500px")
              )
            )
          ),
          shiny::column(
            width = 5,
            bslib::card(
              bslib::card_header("Binary Search Solutions"),
              bslib::card_body(
                shiny::verbatimTextOutput("show_console")
              )
            )
          )
        )
      ),
      bslib::nav_panel(
        "Interactive Help",
        bslib::card(
          bslib::card_header("Understanding the 5 Parameters"),
          bslib::card_body(
            shiny::HTML("
              <p>The spline fit models the base relationship of time with transition probability. The five parameters scale and transform this curve as follows:</p>
              <ul>
                <li><strong>dispersion:</strong> Stretches/compresses the time axis multiplicatively. Larger values stretch the curve, increasing overall variation.</li>
                <li><strong>location:</strong> Shifts the time axis additively (delay or advance). Adds a constant minimum time offset before transitions can occur.</li>
                <li><strong>intensity:</strong> Accelerates or decelerates the event rate. Represents the clock speed or process velocity.</li>
                <li><strong>truncation:</strong> Crops the early part of the probability curve. Transitions cannot happen before this percentile threshold.</li>
                <li><strong>rejection:</strong> Cuts off the late part of the curve. Any individual who hasn't transitioned by this point is 'rejected' or reaches a maximum span.</li>
              </ul>
              <p>The <code>five.show()</code> function performs a binary search on each parameter individually to find the setting that yields the specified target relative mean time.</p>
            ")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Dynamic parameter UI based on selectize input
    output$param_value_ui <- shiny::renderUI({
      shiny::req(input$pick)
      
      # Determine default range based on parameter
      rng <- switch(input$pick,
                    dispersion = list(min = 1, max = 150, val = c(5, 50)),
                    location = list(min = 0, max = 1000, val = c(50, 500)),
                    intensity = list(min = 0.1, max = 10, val = c(0.5, 5)),
                    truncation = list(min = 0.0, max = 0.9, val = c(0.1, 0.6)),
                    rejection = list(min = 0.1, max = 1.0, val = c(0.4, 0.95))
      )
      
      shiny::tagList(
        shiny::sliderInput("param_range", "Value Range:", 
                           min = rng$min, max = rng$max, value = rng$val, step = if(input$pick %in% c("truncation", "rejection")) 0.05 else 1),
        shiny::sliderInput("line_count", "Number of curves to draw:", 
                           min = 2, max = 10, value = 5, step = 1)
      )
    })
    
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
        shiny::need(all(diff(x_vals) > 0), "Error: X coordinates must be strictly increasing.")
      )
      
      # Try building interpSpline and backSpline
      tryCatch({
        meanvalue <- splines::interpSpline(x_vals, y_vals)
        invmvalue <- splines::backSpline(meanvalue)
        list(meanvalue = meanvalue, invmvalue = invmvalue, xy = data.frame(x = x_vals, y = y_vals))
      }, error = function(e) {
        shiny::validate(
          paste("Error building spline:", e$message, 
                "\nNote: The spline must be strictly monotonic (always increasing/decreasing) to calculate its backspline.")
        )
      })
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
                     main = "Baseline Mean-Value Spline",
                     panel.first = graphics::grid(col = "#e9ecef", lty = 1))
      graphics::points(fit_obj$xy$x, fit_obj$xy$y, col = "#7209b7", pch = 19, cex = 1.8)
    })
    
    # Render single parameter sensitivity plot (five.plot)
    output$plot_sensitivity <- shiny::renderPlot({
      fit_obj <- fit_reactive()
      shiny::req(fit_obj, input$param_range, input$line_count)
      
      # Generate sequence of values
      vals <- seq(input$param_range[1], input$param_range[2], length.out = input$line_count)
      
      # Plotting
      graphics::par(bg = "white", col.axis = "#495057", col.lab = "#212529", col.main = "#2ec4b6", fg = "#cccccc")
      
      # Run five.plot directly
      five.plot(fit = fit_obj, pick = input$pick, vals = vals)
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

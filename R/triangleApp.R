#' Interactive Tridiagonal Substrate Network Explorer App
#' 
#' Shiny application to interactively build, visualize, and inspect tridiagonal
#' geometric substrate spatial networks (\code{substrate_topology}, \code{create_substrate}, \code{autoplot.substrate}).
#' 
#' @param width Integer radius size limit of spatial components (default: 10)
#' @param step Numeric grid density spacing interval (default: 1)
#' @param title Application title
#' @export
#' @importFrom shiny shinyApp fluidPage tags div h3 h4 p fluidRow column hr
#' @importFrom shiny reactive renderPlot renderText req selectizeInput sliderInput numericInput textInput textOutput verbatimTextOutput observeEvent updateSliderInput updateNumericInput validate need uiOutput renderUI tagList updateTextInput radioButtons actionButton checkboxGroupInput updateCheckboxGroupInput updateSelectizeInput
#' @importFrom bslib page_sidebar sidebar card card_body card_header bs_theme
#' @importFrom ggplot2 ggplot geom_polygon aes geom_point geom_text theme_void coord_fixed ggtitle
#' @importFrom stats aggregate
triangleApp <- function(width = 10, step = 1, title = "Tridiagonal Substrate Network Explorer") {
  
  app_theme <- bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#212529",
    primary = "#1a73e8",
    secondary = "#7209b7",
    success = "#2ec4b6"
  )
  
  all_modules <- c("fr1", "fr2", "fr3", "fr4", "tw1", "tw2", "lftop", "lfbot")
  
  ui <- bslib::page_sidebar(
    title = title,
    theme = app_theme,
    
    sidebar = bslib::sidebar(
      width = 300,
      shiny::div(
        style = "font-size: 0.85rem; padding: 5px 0;",
        shiny::div(
          style = "display: flex; gap: 8px; align-items: flex-end; margin-bottom: 6px;",
          shiny::div(
            style = "flex: 1;",
            shiny::span("Width (Radius):", style = "font-weight: 600; color: #1a73e8; display: block; margin-bottom: 2px;"),
            shiny::numericInput("width_in", NULL, value = width, min = 2, max = 30, step = 1)
          ),
          shiny::div(
            style = "flex: 1;",
            shiny::span("Step Density:", style = "font-weight: 600; color: #1a73e8; display: block; margin-bottom: 2px;"),
            shiny::numericInput("step_in", NULL, value = step, min = 0.5, max = 5, step = 0.5)
          )
        ),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::div(
          style = "margin-bottom: 6px;",
          shiny::span("Substrate Modules:", style = "font-weight: 600; color: #7209b7; display: block; margin-bottom: 2px;"),
          shiny::selectizeInput("modules_in", NULL,
                                choices = all_modules,
                                selected = all_modules,
                                multiple = TRUE,
                                options = list(plugins = list("remove_button")))
        ),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::div(
          style = "margin-bottom: 6px;",
          shiny::span("Display Layers:", style = "font-weight: 600; color: #2ec4b6; display: block; margin-bottom: 2px;"),
          shiny::checkboxGroupInput("layers_in", NULL,
                                    choices = c("Boundaries" = "poly", "Dots" = "points", "Labels" = "centers", "Numbers" = "labels"),
                                    selected = c("poly", "points", "centers", "labels"),
                                    inline = TRUE)
        ),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::actionButton("reset_btn", "Reset Defaults", class = "btn-outline-secondary btn-sm w-100 mt-1")
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
        .stat-badge {
          background: #f1f3f5;
          padding: 8px 12px;
          border-radius: 8px;
          margin-bottom: 8px;
        }
      "))
    ),
    
    shiny::fluidRow(
      shiny::column(
        width = 8,
        bslib::card(
          bslib::card_header("Tridiagonal Substrate Spatial Network Mapping"),
          bslib::card_body(
            shiny::plotOutput("plot_substrate", height = "520px")
          )
        )
      ),
      shiny::column(
        width = 4,
        bslib::card(
          bslib::card_header("Substrate Grid Statistics & Topology"),
          bslib::card_body(
            shiny::uiOutput("summary_stats")
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        bslib::card(
          bslib::card_header("Instructions & Substrate Network Background"),
          bslib::card_body(
            shiny::HTML("
              <p><strong>Instructions:</strong> Adjust the <em>Width (Radius)</em> and <em>Step Density</em> to dynamically scale the tridiagonal coordinate system. Use the module selector to filter specific plant components (fruits <code>fr1..fr4</code>, twigs <code>tw1..tw2</code>, leaves <code>lftop..lfbot</code>) or toggle layer visibility.</p>
              <p><strong>Background:</strong> In Bland Ewing's population ethology model, individuals navigate a triangular substrate network represented by 3D isometric coordinates <code>(a, b, c)</code> where <code>c = -(a + b)</code>. Geometric patch topologies seamlessly map connectivity between plant micro-habitats.</p>
            ")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    shiny::observeEvent(input$reset_btn, {
      shiny::updateNumericInput(session, "width_in", value = width)
      shiny::updateNumericInput(session, "step_in", value = step)
      shiny::updateSelectizeInput(session, "modules_in", selected = all_modules)
      shiny::updateCheckboxGroupInput(session, "layers_in", selected = c("poly", "points", "centers", "labels"))
    })
    
    substrate_data <- shiny::reactive({
      shiny::req(input$width_in, input$step_in)
      
      shiny::validate(
        shiny::need(input$width_in >= 2, "Width must be at least 2."),
        shiny::need(input$step_in > 0, "Step must be greater than 0."),
        shiny::need(input$width_in > input$step_in, "Width must be greater than Step density.")
      )
      
      tryCatch({
        topo <- substrate_topology(width = input$width_in, step = input$step_in)
        sub_obj <- create_substrate(topo, width = input$width_in, step = input$step_in)
        sub_obj
      }, error = function(e) {
        shiny::validate(paste("Error constructing substrate topology:", e$message))
      })
    })
    
    output$plot_substrate <- shiny::renderPlot({
      sub_obj <- substrate_data()
      shiny::req(sub_obj)
      
      mods <- input$modules_in
      layers <- input$layers_in
      
      if (!is.null(mods) && length(mods) > 0) {
        sub_obj$points  <- sub_obj$points[sub_obj$points$substrate %in% mods, ]
        sub_obj$poly    <- sub_obj$poly[sub_obj$poly$substrate %in% mods, ]
        sub_obj$labels  <- sub_obj$labels[sub_obj$labels$substrate %in% mods, ]
        sub_obj$centers <- sub_obj$centers[sub_obj$centers$substrate %in% mods, ]
      } else {
        sub_obj$points  <- sub_obj$points[0, ]
        sub_obj$poly    <- sub_obj$poly[0, ]
        sub_obj$labels  <- sub_obj$labels[0, ]
        sub_obj$centers <- sub_obj$centers[0, ]
      }
      
      p <- ggplot2::ggplot()
      if ("poly" %in% layers && nrow(sub_obj$poly) > 0) {
        p <- p + ggplot2::geom_polygon(data = sub_obj$poly, ggplot2::aes(x = x, y = y, group = substrate), 
                                      fill = NA, color = "black", linewidth = 0.7)
      }
      if ("points" %in% layers && nrow(sub_obj$points) > 0) {
        p <- p + ggplot2::geom_point(data = sub_obj$points, ggplot2::aes(x = x, y = y, color = substrate), size = 1.6)
      }
      if ("centers" %in% layers && nrow(sub_obj$centers) > 0) {
        p <- p + ggplot2::geom_text(data = sub_obj$centers, ggplot2::aes(x = x, y = y, label = substrate), 
                                    color = "black", fontface = "bold", size = 4.5)
      }
      if ("labels" %in% layers && nrow(sub_obj$labels) > 0) {
        p <- p + ggplot2::geom_text(data = sub_obj$labels, ggplot2::aes(x = x, y = y, label = label), 
                                    color = "darkred", fontface = "bold", size = 3.5)
      }
      
      p + ggplot2::theme_void() + 
        ggplot2::coord_fixed() + 
        ggplot2::ggtitle(paste0("Substrate Network Topology (Width = ", input$width_in, ", Step = ", input$step_in, ")"))
    })
    
    output$summary_stats <- shiny::renderUI({
      sub_obj <- substrate_data()
      shiny::req(sub_obj)
      
      mods <- input$modules_in
      pts <- if (!is.null(mods)) sub_obj$points[sub_obj$points$substrate %in% mods, ] else sub_obj$points[0, ]
      
      total_pts <- nrow(pts)
      active_mods_count <- if (!is.null(mods)) length(mods) else 0
      
      counts <- if (total_pts > 0) table(pts$substrate) else numeric()
      counts_html <- if (length(counts) > 0) {
        paste0("<li><strong>", names(counts), ":</strong> ", counts, " points</li>", collapse = "")
      } else {
        "<li><em>No active modules selected</em></li>"
      }
      
      x_bounds <- if (total_pts > 0) paste0(round(range(pts$x), 2), collapse = " to ") else "N/A"
      y_bounds <- if (total_pts > 0) paste0(round(range(pts$y), 2), collapse = " to ") else "N/A"
      
      shiny::HTML(paste0("
        <div class='stat-badge'>
          <strong style='color: #1a73e8;'>Active Modules:</strong> ", active_mods_count, " / ", length(all_modules), "
        </div>
        <div class='stat-badge'>
          <strong style='color: #7209b7;'>Total Coordinate Dots:</strong> ", total_pts, "
        </div>
        <div class='stat-badge'>
          <strong style='color: #2ec4b6;'>X Span:</strong> ", x_bounds, "<br/>
          <strong style='color: #2ec4b6;'>Y Span:</strong> ", y_bounds, "
        </div>
        <hr style='margin: 10px 0; border-top: 1px solid rgba(0,0,0,0.1);'/>
        <h5 style='font-size: 0.95em; font-weight: bold;'>Points per Substrate:</h5>
        <ul style='padding-left: 18px; font-size: 0.85em; line-height: 1.5em;'>
          ", counts_html, "
        </ul>
      "))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

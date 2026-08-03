#' Systems Ethology App
#' 
#' Interactive Systems Ethology platform orchestrating host-parasite individual-based simulations,
#' age class distributions, hexagonal spatial substrate networks, variance envelopes, and input data tables.
#' 
#' @param title Application title
#' @param id module ID string
#' @export
#' @importFrom shiny actionButton checkboxInput checkboxGroupInput column div HTML h4 isTruthy moduleServer NS observeEvent plotOutput radioButtons reactive reactiveVal renderPlot renderTable renderUI req selectInput selectizeInput sidebarPanel sliderInput tableOutput tagList textInput uiOutput
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' @importFrom cowplot plot_grid
#' @importFrom bslib page_sidebar sidebar navset_tab nav_panel card card_header card_body bs_theme
sysetholApp <- function(title = "Systems Ethology Platform") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      width = 320,
      sysetholInput("sysethol")
    ),
    sysetholOutput("sysethol")
  )
  
  server <- function(input, output, session) {
    sysetholServer("sysethol")
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

#' Systems Ethology Input Controls Module
#' @export
#' @rdname sysetholApp
sysetholInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "font-size: 0.85rem;",
      shiny::h4("Simulation Setup", style = "font-size: 0.95rem; font-weight: 600; margin-bottom: 6px;"),
      shiny::sliderInput(ns("n_host"), "Number of hosts:", min = 0, max = 500, value = 200, step = 20),
      shiny::sliderInput(ns("n_parasite"), "Number of parasites:", min = 0, max = 500, value = 100, step = 20),
      shiny::radioButtons(ns("nsim"), "Number of Simulations:", choices = c(1, 10, 20, 50, 100, 200), selected = 1, inline = TRUE),
      
      # Conditional Control: Steps per click ONLY shown when nsim == 1 (Geometric log scale choices 1..2000)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == '1'", ns("nsim")),
        step_size_slider(ns("step_size"), "Steps per click:", selected = 50)
      ),
      
      # Conditional Control: Total Simulation steps ONLY shown when nsim > 1
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] != '1'", ns("nsim")),
        shiny::sliderInput(ns("steps"), "Total Simulation steps:", min = 500, max = 5000, value = 1000, step = 500)
      ),
      
      shiny::div(
        style = "display: flex; gap: 6px; margin: 10px 0 8px 0;",
        shiny::actionButton(ns("run_engine"), "Run Engine", class = "btn-sm btn-primary flex-fill", style = "font-weight: 600;"),
        shiny::actionButton(ns("reset_engine"), "Reset", class = "btn-sm btn-outline-secondary")
      ),
      
      # Conditional Controls for Substrate Plots (shown ONLY on Substrate Plots tab)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Substrate Plots'", ns("tabset")),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::h4("Substrate Display Options", style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 4px;"),
        axisUnitInput(ns("substrate_axis")),
        substrateInput(ns("substrate"))
      ),
      
      # Conditional Controls for Age Classes (shown ONLY on Age Classes tab)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Age Classes'", ns("tabset")),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::h4("Age Classes Display Options", style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 4px;"),
        ageClassControlInput(ns("age_ctrls"))
      ),
      
      # Conditional Controls for Envelope Plots (shown ONLY on Envelope Plots tab when nsim > 1)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Envelope Plots' && input['%s'] != '1'", ns("tabset"), ns("nsim")),
        shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
        shiny::h4("Envelope Display Options", style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 4px;"),
        shiny::checkboxInput(ns("confidence"), "Confidence Band Envelope", TRUE)
      )
    )
  )
}

#' Systems Ethology Output Display Module
#' @export
#' @rdname sysetholApp
sysetholOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sysethol_tabs"))
}

#' Systems Ethology Server Module
#' @param id Module ID string
#' @export
#' @rdname sysetholApp
sysetholServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_sim <- shiny::reactiveVal(NULL)
    sub_x_var <- axisUnitServer("substrate_axis")
    age_ctrls <- ageClassControlServer("age_ctrls")
    
    # Initialize simulation on start
    shiny::observe({
      if (is.null(current_sim())) {
        nh <- input$n_host %||% 200
        np <- input$n_parasite %||% 100
        sim <- init.simulation(count = c(nh, np))
        sz <- parse_step_size(input$step_size %||% 5)
        sim <- future.events(sim, nstep = sz, plotit = FALSE)
        current_sim(sim)
      }
    })
    
    # Run Engine button
    shiny::observeEvent(input$run_engine, {
      nsim_val <- as.numeric(input$nsim %||% 1)
      nh <- input$n_host %||% 200
      np <- input$n_parasite %||% 100
      
      withProgress(message = "Executing Simulation Engine...", value = 0.2, {
        if (nsim_val == 1) {
          # Single run mode
          sz <- parse_step_size(input$step_size %||% 50)
          sim <- current_sim()
          if (is.null(sim)) sim <- init.simulation(count = c(nh, np))
          sim <- future.events(sim, nstep = sz, plotit = FALSE)
          current_sim(sim)
        } else {
          # Multi-run discrete envelope mode
          tot_steps <- as.numeric(input$steps %||% 1000)
          sims <- ewing_discrete(nsim = nsim_val, nstep = tot_steps, count = c(nh, np), verbose = FALSE)
          current_sim(sims)
        }
        incProgress(0.8, detail = "Done")
      })
    })
    
    # Reset Engine button
    shiny::observeEvent(input$reset_engine, {
      nh <- input$n_host %||% 200
      np <- input$n_parasite %||% 100
      sim <- init.simulation(count = c(nh, np))
      current_sim(sim)
    })
    
    # Input Data App Server
    inputAppServer("input_app", simres = current_sim)
    
    # Compose Dist Plot Module for Age Classes
    distPlotServer("dist_plot", simres = current_sim, x_var = age_ctrls$x_var, total = age_ctrls$total, norm = age_ctrls$norm)
    
    # Dynamic Tabs (Envelope Plots shown ONLY when nsim > 1)
    output$sysethol_tabs <- shiny::renderUI({
      nsim_val <- as.numeric(input$nsim %||% 1)
      if (nsim_val == 1) {
        bslib::navset_tab(
          id = ns("tabset"),
          bslib::nav_panel("Substrate Plots", bslib::card(substrateOutput(ns("substrate")))),
          bslib::nav_panel("Age Classes", bslib::card(distPlotOutput(ns("dist_plot")))),
          bslib::nav_panel("Input Data", bslib::card(
            inputAppInput(ns("input_app")),
            inputAppOutput(ns("input_app"))
          ))
        )
      } else {
        bslib::navset_tab(
          id = ns("tabset"),
          bslib::nav_panel("Substrate Plots", bslib::card(substrateOutput(ns("substrate")))),
          bslib::nav_panel("Age Classes", bslib::card(distPlotOutput(ns("dist_plot")))),
          bslib::nav_panel("Envelope Plots", bslib::card(shiny::plotOutput(ns("env_plot"), height = "500px"))),
          bslib::nav_panel("Input Data", bslib::card(
            inputAppInput(ns("input_app")),
            inputAppOutput(ns("input_app"))
          ))
        )
      }
    })
    
    # Substrate Plots Tab Module
    substrateServer("substrate", simres = current_sim)
    
    # Envelope Plots Tab
    output$env_plot <- shiny::renderPlot({
      sim <- current_sim()
      conf_val <- input$confidence %||% TRUE
      if (inherits(sim, "ewing_discrete")) {
        ggplot_ewing_envelopes(sim, confidence = conf_val)
      } else if (!is.null(sim)) {
        # Single run fallback: build envelope from current sim
        env <- ewing_envelopes(sim)
        ggplot_ewing_envelopes(env, confidence = conf_val)
      } else {
        ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No active simulation for envelope plot")
      }
    })
    
    # Return active simulation state
    current_sim
  })
}

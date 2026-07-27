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
      
      # Conditional Control: Steps per click ONLY shown when nsim == 1
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == '1'", ns("nsim")),
        shiny::sliderInput(ns("step_size"), "Steps per click:", min = 10, max = 500, value = 50, step = 10)
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
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
      
      shiny::h4("Display & Plot Options", style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 4px;"),
      shiny::checkboxInput(ns("norm"), "Normalize Dist Plot", TRUE),
      shiny::checkboxInput(ns("total"), "Include Total in Dist", TRUE),
      
      # Conditional Control: Confidence Band ONLY shown when nsim > 1
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] != '1'", ns("nsim")),
        shiny::checkboxInput(ns("confidence"), "Confidence Band Envelope", TRUE)
      ),
      
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
      shiny::checkboxGroupInput(ns("show_species"), "Species to Display:",
                                choices = c("Host" = "host", "Parasite" = "parasite"),
                                selected = c("host", "parasite"), inline = TRUE),
      shiny::radioButtons(ns("species_mode"), "Species Mode:",
                          choices = c("Overlay (1 Map)" = "overlay", "Separate (Stacked Maps)" = "separate"),
                          selected = "overlay", inline = TRUE)
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
    
    # Initialize simulation on start
    shiny::observe({
      if (is.null(current_sim())) {
        nh <- input$n_host %||% 200
        np <- input$n_parasite %||% 100
        sim <- init.simulation(count = c(nh, np))
        sim <- future.events(sim, nstep = 50, plotit = FALSE)
        current_sim(sim)
      }
    })
    
    # Run Engine button
    shiny::observeEvent(input$run_engine, {
      nsim_val <- as.numeric(input$nsim %||% 1)
      nh <- input$n_host %||% 200
      np <- input$n_parasite %||% 100
      
      if (nsim_val == 1) {
        sim <- current_sim()
        if (is.null(sim)) {
          sim <- init.simulation(count = c(nh, np))
        }
        sz <- input$step_size %||% 50
        new_state <- future.events(sim, nstep = sz, plotit = FALSE)
        current_sim(new_state)
      } else {
        tot_steps <- input$steps %||% 1000
        res <- ewing_discrete(nsim = nsim_val, nstep = tot_steps, count = c(nh, np), verbose = FALSE)
        current_sim(res)
      }
    })
    
    # Reset button
    shiny::observeEvent(input$reset_engine, {
      nh <- input$n_host %||% 200
      np <- input$n_parasite %||% 100
      sim <- init.simulation(count = c(nh, np))
      sim <- future.events(sim, nstep = 50, plotit = FALSE)
      current_sim(sim)
    })
    
    # Dynamic Tabs (Envelope Plots shown ONLY when nsim > 1)
    output$sysethol_tabs <- shiny::renderUI({
      nsim_val <- as.numeric(input$nsim %||% 1)
      if (nsim_val == 1) {
        bslib::navset_tab(
          bslib::nav_panel("Dist Plots", bslib::card(shiny::plotOutput(ns("dist_plot"), height = "500px"))),
          bslib::nav_panel("Substrate Plots", bslib::card(shiny::uiOutput(ns("substrate_ui")))),
          bslib::nav_panel("Input Data", bslib::card(
            shiny::selectInput(ns("dataname"), "Select Dataset Table:", 
                               choices = c("organism.features", "future.host", "future.parasite", 
                                           "substrate.host", "substrate.parasite", "substrate.substrate", 
                                           "host.parasite", "temperature.par", "temperature.base"), 
                               selected = "organism.features"),
            shiny::tableOutput(ns("org_table"))
          ))
        )
      } else {
        bslib::navset_tab(
          bslib::nav_panel("Dist Plots", bslib::card(shiny::plotOutput(ns("dist_plot"), height = "500px"))),
          bslib::nav_panel("Substrate Plots", bslib::card(shiny::uiOutput(ns("substrate_ui")))),
          bslib::nav_panel("Envelope Plots", bslib::card(shiny::plotOutput(ns("env_plot"), height = "500px"))),
          bslib::nav_panel("Input Data", bslib::card(
            shiny::selectInput(ns("dataname"), "Select Dataset Table:", 
                               choices = c("organism.features", "future.host", "future.parasite", 
                                           "substrate.host", "substrate.parasite", "substrate.substrate", 
                                           "host.parasite", "temperature.par", "temperature.base"), 
                               selected = "organism.features"),
            shiny::tableOutput(ns("org_table"))
          ))
        )
      }
    })
    
    # Dist Plots Tab
    output$dist_plot <- shiny::renderPlot({
      sim <- current_sim()
      shiny::req(sim)
      if (inherits(sim, "ewing_discrete")) {
        autoplot(sim)
      } else {
        norm_val <- input$norm %||% TRUE
        tot_val <- input$total %||% TRUE
        ac <- ewing_ageclass(sim, total = tot_val, normalize = norm_val)
        ggplot2::autoplot(ac)
      }
    })
    
    # Substrate Plots Tab
    selected_species <- shiny::reactive({
      sel <- input$show_species
      if (is.null(sel) || length(sel) == 0) c("host", "parasite") else sel
    })
    
    sppplot <- shiny::reactive({
      sim <- current_sim()
      shiny::req(sim)
      spp <- selected_species()
      mode_val <- input$species_mode %||% "overlay"
      
      # Extract single simulation object if discrete
      sim_single <- if (inherits(sim, "ewing_discrete")) sim[[1]] else sim
      if (!inherits(sim_single, "ewing")) return(list())
      
      if (mode_val == "overlay") {
        sub_data <- ewing_substrate(sim_single, spp, layout = "hex", width = 10, step_density = 1)
        if (!is.null(sub_data)) list(ggplot_ewing_substrate(sub_data, layout = "hex")) else list()
      } else {
        plots <- lapply(spp, function(x) {
          sub_data <- ewing_substrate(sim_single, x, layout = "hex", width = 10, step_density = 1)
          if (!is.null(sub_data)) ggplot_ewing_substrate(sub_data, layout = "hex") else NULL
        })
        plots[!sapply(plots, is.null)]
      }
    })
    
    output$sppPlot <- shiny::renderPlot({
      plots <- sppplot()
      if (!is.null(plots) && length(plots) > 0) {
        cowplot::plot_grid(plotlist = plots, nrow = length(plots), align = "v")
      } else {
        ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No species selected to plot")
      }
    })
    
    output$substrate_ui <- shiny::renderUI({
      plots <- sppplot()
      n_plots <- if (!is.null(plots)) max(1, length(plots)) else 1
      h_in <- 4.5 * n_plots
      shiny::plotOutput(ns("sppPlot"), height = paste0(h_in, "in"))
    })
    
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
    
    # Input Data Tab
    output$org_table <- shiny::renderTable({
      name <- input$dataname %||% "organism.features"
      sim <- current_sim()
      sim_single <- if (inherits(sim, "ewing_discrete")) sim[[1]] else sim
      
      # Extract dataset table from package/simulation environment
      switch(name,
        "organism.features" = getOrgFeature(sim_single, "host", "features"),
        "future.host"       = getOrgFuture(sim_single, "host"),
        "future.parasite"   = getOrgFuture(sim_single, "parasite"),
        "substrate.host"    = getOrgInteract(sim_single, "substrate", "host"),
        "substrate.parasite"= getOrgInteract(sim_single, "substrate", "parasite"),
        "substrate.substrate"= getOrgInteract(sim_single, "substrate", "substrate"),
        "host.parasite"     = getOrgInteract(sim_single, "host", "parasite"),
        data.frame(Info = paste("Dataset", name, "selected"))
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Return active simulation state
    current_sim
  })
}

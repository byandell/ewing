#' Substrate Plot App
#' 
#' A focused graphical module aggregating structural species progression separated distinctly across
#' physical environment thresholds mapped by evaluating top-level topological definitions.
#' Includes support for both hexagonal grid overlay mapping and classic faceted panels, as well as simulation stepping,
#' multi-species overlays, and separate adjacent hex grid visualization modes.
#' 
#' @param title Application title
#' @param id module ID string
#' @param simres reactive object resolving simulation state (`ewing` community S3 object)
#' @export
#' @importFrom purrr map
#' @importFrom shiny actionButton bindCache bindEvent checkboxInput checkboxGroupInput column downloadButton downloadHandler fileInput fluidPage fluidRow HTML h4 incProgress isTruthy mainPanel moduleServer NS plotOutput reactive renderPlot renderUI req selectInput selectizeInput sidebarLayout sidebarPanel sliderInput numericInput radioButtons tagList textInput titlePanel withProgress uiOutput reactiveVal observeEvent
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' @importFrom cowplot plot_grid
#' @importFrom bslib page_sidebar sidebar
substrateApp <- function(title = "Substrate Organism Movement Explorer") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      initParInput("init_par"),
      shiny::hr(),
      substrateInput("substrate")),
    substrateOutput("substrate")
  )
  server <- function(input, output, server) {
    init_par <- initParServer("init_par")
    siminit <- initServer("init", init_par)
    substrateServer("substrate", siminit)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

#' Substrate Controls Input Module
#' @export
#' @rdname substrateApp
substrateInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "font-size: 0.85rem;",
      shiny::h4("Substrate Display & Stepping", style = "font-size: 1rem; font-weight: 600; margin-bottom: 8px;"),
      shiny::checkboxGroupInput(ns("show_species"), "Species to Display:",
                                choices = c("Host" = "host", "Parasite" = "parasite"),
                                selected = c("host", "parasite"),
                                inline = TRUE),
      shiny::radioButtons(ns("species_mode"), "Species Mode:",
                          choices = c("Overlay (1 Map)" = "overlay", "Separate (Adjacent Maps)" = "separate"),
                          selected = "overlay", inline = TRUE),
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
      shiny::radioButtons(ns("layout"), "Layout View:",
                          choices = c("Hex Substrate Overlay" = "hex", "Faceted Substrates" = "facet"),
                          selected = "hex", inline = TRUE),
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
      shiny::span("Simulation Stepping:", style = "font-weight: 600; color: #1a73e8; display: block; margin-bottom: 4px;"),
      shiny::div(
        style = "display: flex; gap: 4px; margin-bottom: 8px;",
        shiny::actionButton(ns("step1"), "+1 Step", class = "btn-sm btn-outline-primary flex-fill"),
        shiny::actionButton(ns("step10"), "+10 Steps", class = "btn-sm btn-outline-primary flex-fill"),
        shiny::actionButton(ns("step100"), "+100 Steps", class = "btn-sm btn-outline-primary flex-fill"),
        shiny::actionButton(ns("reset_sim"), "Reset", class = "btn-sm btn-outline-secondary")
      ),
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
      shiny::checkboxGroupInput(ns("layers"), "Display Layers:",
                                choices = c("Substrate Boundaries" = "poly",
                                            "Hex Grid Overlay" = "hex",
                                            "Organisms" = "organisms",
                                            "Substrate Names" = "centers",
                                            "Side Numbers" = "labels"),
                                selected = c("poly", "hex", "organisms", "centers", "labels")),
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 6px 0;"),
      shiny::div(
        style = "display: flex; gap: 8px;",
        shiny::numericInput(ns("width"), "Radius:", value = 10, min = 2, max = 30, step = 1),
        shiny::numericInput(ns("step_density"), "Step Density:", value = 1, min = 0.5, max = 5, step = 0.5)
      )
    )
  )
}

#' Substrate Server Module
#' @param id Module ID string
#' @param simres Reactive object returning an `ewing` simulation community object
#' @param width Default substrate radius limit (10)
#' @param step_density Default step density spacing interval (1)
#' @export
#' @rdname substrateApp
substrateServer <- function(id, simres, width = 10, step_density = 1) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    current_sim <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(simres(), {
      current_sim(simres())
    })
    
    shiny::observeEvent(input$step1, {
      sim <- current_sim()
      if (!is.null(sim)) {
        res <- future.events(sim, nstep = 1, plotit = FALSE)
        current_sim(res)
      }
    })
    
    shiny::observeEvent(input$step10, {
      sim <- current_sim()
      if (!is.null(sim)) {
        res <- future.events(sim, nstep = 10, plotit = FALSE)
        current_sim(res)
      }
    })
    
    shiny::observeEvent(input$step100, {
      sim <- current_sim()
      if (!is.null(sim)) {
        res <- future.events(sim, nstep = 100, plotit = FALSE)
        current_sim(res)
      }
    })
    
    shiny::observeEvent(input$reset_sim, {
      current_sim(simres())
    })
    
    available_species <- shiny::reactive({
      sim <- current_sim()
      if (!is.null(sim) && !is.null(sim$pop)) names(sim$pop) else NULL
    })
    
    selected_species <- shiny::reactive({
      avail <- available_species()
      if (is.null(avail)) return(NULL)
      sel <- input$show_species
      if (is.null(sel) || length(sel) == 0) avail else intersect(sel, avail)
    })
    
    sppplot <- shiny::reactive({
      spp <- selected_species()
      shiny::req(spp)
      sim <- current_sim()
      shiny::req(sim)
      
      layout_val <- if (!is.null(input$layout)) input$layout else "hex"
      mode_val <- if (!is.null(input$species_mode)) input$species_mode else "overlay"
      w_val <- if (!is.null(input$width)) input$width else width
      sd_val <- if (!is.null(input$step_density)) input$step_density else step_density
      layers_val <- if (!is.null(input$layers)) input$layers else c("poly", "hex", "organisms", "centers", "labels")
      
      sim_single <- if (inherits(sim, "ewing_discrete") && is.list(sim) && length(sim) > 0) sim[[1]] else sim
      if (inherits(sim_single, "ewing")) {
        if (mode_val == "overlay" && layout_val == "hex") {
          sub_data <- ewing_substrate(sim_single, spp, layout = layout_val, width = w_val, step_density = sd_val)
          if (!is.null(sub_data)) {
            p_obj <- ggplot_ewing_substrate(sub_data, layout = layout_val, width = w_val, step_density = sd_val, layers = layers_val)
            list(p_obj)
          } else {
            list()
          }
        } else {
          p <- lapply(spp, function(x) {
            sub_data <- ewing_substrate(sim_single, x, layout = layout_val, width = w_val, step_density = sd_val)
            if (!is.null(sub_data)) {
              p_obj <- ggplot_ewing_substrate(sub_data, layout = layout_val, width = w_val, step_density = sd_val, layers = layers_val)
              p_obj
            } else {
              NULL
            }
          })
          p[!sapply(p, is.null)]
        }
      } else {
        list()
      }
    })
    
    output$sppPlot <- shiny::renderPlot({
      plots <- sppplot()
      if (!is.null(plots) && length(plots) > 0) {
        cowplot::plot_grid(plotlist = plots, ncol = length(plots), align = "h")
      } else {
        ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No active species selected to plot")
      }
    })
    
    output$substrate_plot <- shiny::renderUI({
      plots <- sppplot()
      h_in <- 5.0
      shiny::plotOutput(ns("sppPlot"), height = paste0(h_in, "in"))
    })
    
    # Return current simulation state for downstream composition
    current_sim
  })
}

#' Substrate Output Module
#' @export
#' @rdname substrateApp
substrateOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("substrate_plot"))
}

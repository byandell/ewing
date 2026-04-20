#' Ewing App
#' 
#' Core `Systems Ethology` Shiny platform acting as the central interactive wrapper modularizing
#' age distributions, probability envelopes, and runtime data interactions.
#' 
#' @param title Application title
#' @param id module ID string
#' @export
#' @importFrom utils packageVersion
#' @importFrom shiny HTML NS fluidPage tagList titlePanel uiOutput renderText reactive
#' @importFrom bslib page_sidebar sidebar navset_tab nav_panel card
#' @importFrom cowplot plot_grid
ewingApp <- function(title = "Systems Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      ewingInput("ewing")
    ),
    ewingOutput("ewing")
  )
  
  server <- function(input, output, session) {
    ewingServer("ewing")
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

#' @export
#' @rdname ewingApp
ewingServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Initialize Parameters Table & Species Models
    init_par <- initParServer("init_par", simres = shiny::reactive({ tryCatch(sim_data$simres(), error=function(e) NULL) }))
    
    # 2. Main Run Simulation 
    sim_data <- simServer("sim", init_par)
    
    # 3. Dynamic Visualizations (Evaluating on active state)
    dist_plot <- distPlotServer("dist_plot", sim_data$simres)
    spp_plot <- substrateServer("substrate", sim_data$simres)
    env_plot <- envPlotServer("env_plot", sim_data$simres, sim_data$nsim)
    
    # 4. Bind the File Extractor
    downloadServer("download", sim_data = sim_data, distplot = dist_plot, sppplot = spp_plot, envplot = env_plot)
    
    # Render Version Number
    output$version <- shiny::renderText({
      paste("Ewing package version ", utils::packageVersion("ewing"))
    })
    
    # Dynamic "Seamless" Navigation Tabs based on nsim toggle!
    output$dynamic_tabs <- shiny::renderUI({
      sims <- shiny::req(sim_data$nsim())
      if (sims == 1) {
        bslib::navset_tab(
          bslib::nav_panel("Dist Plots", distPlotOutput(ns("dist_plot"))),
          bslib::nav_panel("Substrate Plots", bslib::card(substrateOutput(ns("substrate")))),
          bslib::nav_panel("Input Data", initParOutput(ns("init_par")))
        )
      } else {
        bslib::navset_tab(
          bslib::nav_panel("Envelope Plots", bslib::card(envPlotOutput(ns("env_plot")))),
          bslib::nav_panel("Input Data", initParOutput(ns("init_par")))
        )
      }
    })
  })
}

#' Ewing Input
#' @export
#' @rdname ewingApp
ewingInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
      initParInput(ns("init_par")),
      simInput(ns("sim")),
      shiny::HTML("<hr style='height:1px;border:none;color:#333;background-color:#333;' />"),
      distPlotInput(ns("dist_plot")),
      envPlotInput(ns("env_plot")),
      shiny::HTML("<hr style='height:1px;border:none;color:#333;background-color:#333;' />"),
      downloadInput(ns("download")),
      shiny::HTML("<hr style='height:1px;border:none;color:#333;background-color:#333;' />"),
      shiny::HTML("See <a href='https://github.com/byandell/ewing'>ewing package on github</a><br>"),
      shiny::uiOutput(ns("version"))
  )
}

#' Ewing Output
#' @export
#' @rdname ewingApp
ewingOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    simUI(ns("sim")),
    shiny::uiOutput(ns("dynamic_tabs"))
  )
}

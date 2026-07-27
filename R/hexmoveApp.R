#' Interactive Hex Substrate Organism Movement Application
#'
#' Shiny application built on `initParServer`, `initServer`, and `substrateServer`
#' that visualizes organism positions on a hexagonal substrate network using their global tridiagonal coordinates
#' and allows interactive stepping through simulation events (+1, +10, +100 steps).
#'
#' @param mysim Optional pre-initialized `ewing` simulation community object. If NULL, initializes default simulation.
#' @param width Substrate radius limit (default: 10).
#' @param step Numeric step density spacing interval (default: 1).
#' @param title Application title string.
#' @export
#' @importFrom shiny shinyApp reactiveVal reactive observeEvent req HTML h4 tagList div span actionButton numericInput radioButtons checkboxGroupInput uiOutput renderUI hr
#' @importFrom bslib page_sidebar sidebar card card_body card_header navset_tab nav_panel
hexmoveApp <- function(mysim = NULL, width = 10, step = 1, title = "Organism Movement on Hex Grid") {
  
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      width = 320,
      initParInput("init_par"),
      shiny::hr(),
      substrateInput("substrate")
    ),
    bslib::navset_tab(
      bslib::nav_panel("Hex Substrate Map", substrateOutput("substrate")),
      bslib::nav_panel("Simulation Summary", initOutput("init"))
    )
  )
  
  server <- function(input, output, session) {
    init_par <- initParServer("init_par")
    siminit <- initServer("init", init_par)
    
    # If pre-initialized simulation is passed in, run initial steps if requested, else use siminit
    initial_state <- shiny::reactive({
      if (!is.null(mysim)) {
        mysim
      } else {
        siminit()
      }
    })
    
    substrateServer("substrate", initial_state, width = width, step_density = step)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

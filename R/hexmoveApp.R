#' Hexmove Input Module
#' @param id Module ID string
#' @export
#' @rdname hexmoveApp
hexmoveAppInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    initParInput(ns("init_par")),
    shiny::hr(),
    substrateInput(ns("substrate"))
  )
}

#' Hexmove Output Module
#' @param id Module ID string
#' @export
#' @rdname hexmoveApp
hexmoveAppOutput <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_tab(
    bslib::nav_panel("Hex Substrate Map", substrateOutput(ns("substrate"))),
    bslib::nav_panel("Simulation Summary", initOutput(ns("init")))
  )
}

#' Hexmove Server Module
#' @param id Module ID string
#' @param mysim Optional pre-initialized simulation
#' @param width Substrate radius limit
#' @param step Step density spacing interval
#' @export
#' @rdname hexmoveApp
hexmoveAppServer <- function(id, mysim = NULL, width = 10, step = 1) {
  shiny::moduleServer(id, function(input, output, session) {
    init_par <- initParServer("init_par")
    siminit <- initServer("init", init_par)
    
    initial_state <- shiny::reactive({
      if (!is.null(mysim)) {
        mysim
      } else {
        siminit()
      }
    })
    
    substrateServer("substrate", initial_state, width = width, step_density = step)
  })
}

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
#' @importFrom shiny shinyApp reactiveVal reactive observeEvent req HTML h4 tagList div span actionButton numericInput radioButtons checkboxGroupInput uiOutput renderUI hr NS
#' @importFrom bslib page_sidebar sidebar card card_body card_header navset_tab nav_panel
#' @rdname hexmoveApp
hexmoveApp <- function(mysim = NULL, width = 10, step = 1, title = "Organism Movement on Hex Grid") {
  
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      width = 340,
      hexmoveAppInput("hexmove")
    ),
    hexmoveAppOutput("hexmove")
  )
  
  server <- function(input, output, session) {
    hexmoveAppServer("hexmove", mysim = mysim, width = width, step = step)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

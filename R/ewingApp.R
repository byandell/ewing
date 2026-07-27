#' Ewing App
#' 
#' Core `Systems Ethology` Shiny platform acting as the central interactive wrapper modularizing
#' age distributions, probability envelopes, runtime data interactions, and file downloads.
#' Composes elements from `sysetholApp` and `downloadApp`.
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
      width = 320,
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
    
    # 1. Run Core Systems Ethology Server
    current_sim <- sysetholServer("sysethol")
    
    # 2. Bind File Download Server
    downloadServer("download", sim_data = list(simres = current_sim, nsim = shiny::reactive({ 1 })))
    
    # Render Version Number
    output$version <- shiny::renderText({
      paste("Ewing package version ", utils::packageVersion("ewing"))
    })
    
    # Return simulation state
    current_sim
  })
}

#' Ewing Input
#' @export
#' @rdname ewingApp
ewingInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    sysetholInput(ns("sysethol")),
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
  sysetholOutput(ns("sysethol"))
}

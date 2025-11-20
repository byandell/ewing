#' Ewing App
#' 
#' @export
#' @importFrom utils write.csv
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom shiny actionButton bindCache bindEvent checkboxInput column
#'             downloadButton downloadHandler fileInput fluidPage fluidRow HTML
#'             h4 incProgress isTruthy mainPanel moduleServer NS plotOutput
#'             reactive renderPlot renderUI req selectInput sidebarLayout
#'             sidebarPanel sliderInput tagList textInput titlePanel
#'             withProgress uiOutput
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' @importFrom DT renderDataTable
#' @importFrom cowplot plot_grid
futureApp <- function(title = "Population Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      initParInput("init_par"),
      distPlotInput("dist_plot"),
      futureInput("future")),
    bslib::navset_tab(
      bslib::nav_panel("Dist", distPlotOutput("dist_plot")),
      bslib::nav_panel("Substrate",
        bslib::card(substrateOutput("substrate_init")),
        bslib::card(substrateOutput("substrate"))),
      bslib::nav_panel("Params", futureOutput("future")))
  )
  server <- function(input, output, server) {
    init_par <- initParServer("init_par")
    siminit <- initServer("init", init_par)
    simres <- futureServer("future", siminit)
    distPlotServer("dist_plot", simres)
    substrateServer("substrate_init", siminit)
    substrateServer("substrate", simres)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
#' @export
#' @rdname futureApp
futureServer <- function(id, siminit) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    simres <- shiny::reactive({
      future.events(shiny::req(siminit()), nstep = shiny::req(input$steps),
                    plotit = FALSE) # simulate future events
    })

    output$summary <- shiny::renderUI({
      out <- summary_simobj(summary(shiny::req(simres())))
      shiny::HTML(out)
    })
    
    # Result.
    simres
  })
}
#' Ewing Input
#' @export
#' @rdname futureApp
futureInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::sliderInput(ns("steps"),
                     label = "Simulation steps:",
                     min = 1000,
                     max = 10000,
                     value = 1000,
                     step = 500)
}
#' Ewing Output
#' @export
#' @rdname futureApp
futureOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("summary"))
}

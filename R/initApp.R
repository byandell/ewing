#' Ewing Initialization App
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
#' @importFrom bslib page_sidebar sidebar
#' @importFrom DT renderDataTable
#' @importFrom cowplot plot_grid
initApp <- function(title = "Population Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      initParInput("init_par")),
    initOutput("init"),
    substrateOutput("substrate")
  )
  server <- function(input, output, server) {
    init_par <- initParServer("init_par")
    siminit <- initServer("init", init_par)
    substrateServer("substrate", siminit)
  }
  shiny::shinyApp(ui = ui, server = server)
}
#' @export
#' @rdname initApp
initServer <- function(id, init_par) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    siminit <- shiny::reactive({
      init.simulation(count = as.numeric(c(shiny::req(init_par$host),
                                           shiny::req(init_par$parasite))))
    })
    
    output$init <- shiny::renderUI({
      out <- summary_simobj(shiny::req(siminit()))
      shiny::HTML(out)
    })

    # Return.
    siminit
  })
}
#' @export
#' @rdname initApp
initOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("init"))
}

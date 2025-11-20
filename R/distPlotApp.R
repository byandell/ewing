#' Distribution Plot App
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
distPlotApp <- function(title = "Population Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      initParInput("init_par"),
      distPlotInput("dist_plot"),
      futureInput("future")),
    distPlotOutput("dist_plot")
  )
  server <- function(input, output, server) {
    init_par <- initParServer("init_par")
    siminit <- initServer("init", init_par)
    simres <- futureServer("future", siminit)
    distPlotServer("dist_plot", simres)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
#' @export
#' @rdname distPlotApp
distPlotServer <- function(id, simres) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dist_plot <- shiny::reactive({
      shiny::req(simres(), input$total, input$norm)
      object <- if(inherits(simres(), "ewing")) {
        ewing_ageclass(simres(), total = input$total, normalize = input$norm)
      } else {
        NULL
      }
      if(is.null(object)) return(plot_null("no simulation"))
      ggplot2::autoplot(object)
    })
    output$dist_plot <- shiny::renderPlot({
      dist_plot()
    })
    
    # Return.
    dist_plot
  })
}
#' @export
#' @rdname distPlotApp
distPlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxInput(ns("norm"), "Normalize Plot", TRUE),
    shiny::checkboxInput(ns("total"), "Include Total", TRUE))
}
#' @export
#' @rdname distPlotApp
distPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("dist_plot"), height = "4in")
}

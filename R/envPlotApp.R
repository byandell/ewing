#' Envelope Plot App
#' 
#' Shiny application modules used strictly to render and wrap statistical envelope probability
#' intervals generated natively by active `nsim > 1` discrete operations.
#' 
#' @param title Application title
#' @param id module ID string
#' @param simres reactive holding evaluated multi-simulation responses
#' @param nsim reactive integer specifying iterations computed within the `simres` object
#' @export
#' @importFrom purrr map
#' @importFrom shiny checkboxInput moduleServer NS plotOutput reactive renderPlot req tagList
#' @importFrom ggplot2 autoplot ggplot
envPlotApp <- function(title = "Envelope Plots") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      envPlotInput("env_plot")
    ),
    envPlotOutput("env_plot")
  )
  server <- function(input, output, server) {
    # Dummy mock context
  }
  shiny::shinyApp(ui = ui, server = server)
}

#' @export
#' @rdname envPlotApp
envPlotServer <- function(id, simres, nsim) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    envdata <- shiny::reactive({
      res <- shiny::req(simres())
      if(inherits(res, "ewing_discrete")) {
        ewing_envelopes(res)
      } else {
        NULL
      }
    })
    
    envelopePlot <- shiny::reactive({
      shiny::req(envdata())
      sims <- shiny::req(nsim())
      conf <- (sims >= 10) & input$conf 
      if(inherits(simres(), "ewing_discrete")) {
        ggplot_ewing_envelopes(envdata(), conf)
      } else {
        NULL
      }
    })
    
    output$envPlot <- shiny::renderPlot({
      envelopePlot()
    })
    
    # Return outputs for download hooks
    list(
      envdata = envdata,
      envelopePlot = envelopePlot
    )
  })
}

#' @export
#' @rdname envPlotApp
envPlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::checkboxInput(ns("conf"), "Confidence band", FALSE)
}

#' @export
#' @rdname envPlotApp
envPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("envPlot"))
}

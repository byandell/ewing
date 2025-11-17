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
ewingFutureApp <- function(title = "Population Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      ewingInitInput("ewing_init"),
      ewingFutureInput("ewing_future")),
    ewingFutureOutput("ewing_future")
  )
  server <- function(input, output, server) {
    siminit <- ewingInitServer("ewing_init")
    ewingFutureServer("ewing_future", siminit)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
#' @export
#' @rdname ewingFutureApp
ewingFutureServer <- function(id, siminit) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    simres <- shiny::reactive({
      future.events(shiny::req(siminit()), nstep = shiny::req(input$steps),
                    plotit = FALSE) # simulate future events
    })
    distplot <- shiny::reactive({
      if(inherits(simres(), "ewing")) {
        ggplot2::autoplot(ewing_ageclass(simres(), total = input$total,
                                         normalize = input$norm))
      } else {
        NULL
      }
    })
    output$distPlot <- shiny::renderPlot({
      distplot()
    })
    species <- shiny::reactive({
      names(shiny::req(siminit())$pop)
    })
    substrates <- shiny::reactive({
      get.organisms()$substrates
    })
    sppplot <- shiny::reactive({
      shiny::req(species(), simres())
      if(inherits(simres(), "ewing")) {
        if(!is.null(simres())) {
          p <- lapply(species(), function(x) {
            p <- ggplot2::autoplot(ewing_substrate(simres(), x))
            if(inherits(p, "ggplot"))
              p <- p + ggplot2::ggtitle(paste(x, "on", substrates()[1]))
            p
          })
          if(any(unlist(purrr::map(p, is.null))))
            p <- NULL
          p
        }
      } else {
        ggplot2::ggplot()
      }
    })
    output$sppPlot <- shiny::renderPlot({
      if(!is.null(sppplot())) {
        spp <- length(species())
        p <- sppplot()[[1]]
        if(spp > 1) for(i in seq(2, spp)) {
          p <- p + sppplot()[[i]]
        }
        cowplot::plot_grid(plotlist = p, nrow = spp)
      } else {
        ggplot2::ggplot()
      }
    })
    output$plots <- shiny::renderUI({
      shiny::req(species())
      shiny::tagList(
        shiny::plotOutput(ns("distPlot"), height = "4in"),
        shiny::plotOutput(ns("sppPlot"), height = paste0(2 * length(species()), "in")))
    })
  })
}
#' Ewing Input
#' @export
#' @rdname ewingFutureApp
ewingFutureInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("steps"),
                     label = "Simulation steps:",
                     min = 1000,
                     max = 10000,
                     value = 1000,
                     step = 500),
    shiny::checkboxInput(ns("norm"), "Normalize Plot", TRUE),
    shiny::checkboxInput(ns("total"), "Include Total", TRUE))
}
#' Ewing Output
#' @export
#' @rdname ewingFutureApp
ewingFutureOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("plots"))
}

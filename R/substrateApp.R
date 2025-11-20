#' Substrate Plot App
#' 
#' @export
#' @importFrom purrr map
#' @importFrom shiny actionButton bindCache bindEvent checkboxInput column
#'             downloadButton downloadHandler fileInput fluidPage fluidRow HTML
#'             h4 incProgress isTruthy mainPanel moduleServer NS plotOutput
#'             reactive renderPlot renderUI req selectInput sidebarLayout
#'             sidebarPanel sliderInput tagList textInput titlePanel
#'             withProgress uiOutput
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' @importFrom cowplot plot_grid
substrateApp <- function() {
  ui <- bslib::page_sidebar(
    title = "Test Substrate",
    sidebar = bslib::sidebar(
      initParInput("init_par")),
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
#' @rdname substrateApp
substrateServer <- function(id, simres) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    species <- shiny::reactive({
      names(shiny::req(simres())$pop)
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
        plot_null()
      }
    })
    output$substrate_plot <- shiny::renderUI({
      shiny::req(species())
      shiny::plotOutput(ns("sppPlot"),
                        height = paste0(2 * length(species()), "in"))
    })
  })
}
#' @export
#' @rdname substrateApp
substrateOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("substrate_plot"))
}

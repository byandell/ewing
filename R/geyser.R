#' Shiny Server for geyser Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname geyser
#' @importFrom shiny bootstrapPage checkboxInput moduleServer NS plotOutput
#'             renderPlot renderUI selectInput shinyApp sliderInput uiOutput
#' @importFrom graphics hist lines rug
#' @importFrom stats density
geyserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Input Bandwidth Adjustment
    output$bw_adjust <- shiny::renderUI({
      if(input$density) {
        shiny::sliderInput(inputId = ns("bw_adjust"),
                           label = "Bandwidth adjustment:",
                           min = 0.2, max = 2, value = 1, step = 0.2)
      }
    })
    
    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      graphics::hist(faithful$eruptions,
                     probability = TRUE,
                     breaks = as.numeric(input$n_breaks),
                     xlab = "Duration (minutes)",
                     main = "Geyser eruption duration")
      
      if (input$individual_obs) {
        graphics::rug(faithful$eruptions)
      }
      if (input$density) {
        shiny::req(input$bw_adjust)
        dens <- stats::density(faithful$eruptions,
                               adjust = input$bw_adjust)
        graphics::lines(dens, col = "blue")
      }
    })
  })
}
#' Shiny Module Input for Geyser
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname geyser
#' @export
geyserInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(inputId = ns("n_breaks"),
                label = "Number of bins in histogram (approximate):",
                choices = c(10, 20, 35, 50),
                selected = 20),
    shiny::checkboxInput(inputId = ns("individual_obs"),
                  label = strong("Show individual observations"),
                  value = FALSE),
    
    shiny::checkboxInput(inputId = ns("density"),
                  label = strong("Show density estimate"),
                  value = FALSE))
}
#' Shiny Module UI for Geyser
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname geyser
#' @export
geyserUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("bw_adjust"))
}
#' Shiny Module Output for Geyser
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname geyser
#' @export
geyserOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("main_plot"), height = "300px")
}
#' Shiny Module App for Geyser
#' @return nothing returned
#' @rdname geyser
#' @export
geyserApp <- function() {
  ui <- shiny::bootstrapPage(
    geyserInput("geyser"), 
    geyserOutput("geyser"),
    # Display this only if the density is shown
    geyserUI("geyser")
  )
  server <- function(input, output, session) {
    geyserServer("geyser")
  }
  shiny::shinyApp(ui, server)
}

#' @export
#' @importFrom utils write.csv
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom shiny actionButton bindCache bindEvent checkboxInput column
#'             downloadButton downloadHandler fileInput fluidPage fluidRow HTML
#'             h2 h4 HTML incProgress isTruthy mainPanel moduleServer NS
#'             plotOutput radioButtons reactive renderPlot renderUI req
#'             selectInput shinyApp sidebarLayout sidebarPanel sliderInput
#'             tagList textInput titlePanel withProgress uiOutput
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' 
simpleServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}
#' simple Input
#' @export
#' @rdname simpleServer
simpleInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Simulation Settings"),
    shiny::sliderInput(ns("steps"),
                       label = "Simulation steps:",
                       min = 1000,
                       max = 10000,
                       value = 1000,
                       step = 500),
    shiny::radioButtons(ns("nsim"),
                        "Number of Simulations",
                        c(1,10,20,50,100,200),
                        1, inline = TRUE),
    shiny::actionButton(ns("go"), "Start Simulation"),
    
    shiny::HTML("<hr style='height:1px;border:none;color:#333;background-color:#333;' />"),
    shiny::h4("Save Files"),
    shiny::uiOutput(ns("version"))
  )
}
#' simple Output
#' @export
#' @rdname simpleServer
simpleOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("button"), "", c("Plots", "Input Data"),
                        "Plots", inline = TRUE),
  )
}
#' simple App
#' @export
#' @rdname simpleServer
simpleApp <- function(title = "Population Ethology") {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        simpleInput("simple")
      ),
      shiny::mainPanel(
        simpleOutput("simple")
    )))
  
  server <- function(input, output, server) {
    simpleServer("simple")
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
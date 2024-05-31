#devtools::install_github("myllym/GET") # need recent fixes not on CRAN yet
devtools::install_github("byandell/ewing")

title <- "Population Ethology"

ui <- shiny::fluidPage(
  shiny::titlePanel(title),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      ewing::simpleInput("ewing")
    ),
    shiny::mainPanel(
      ewing::simpleOutput("ewing")
    )))

server <- function(input, output, server) {
  ewing::simpleServer("ewing")
}

shiny::shinyApp(ui = ui, server = server)

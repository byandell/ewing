devtools::install_cran("GET")
devtools::install_cran("DT")
devtools::install_github("byandell/ewing")

title <- "Population Ethology"

ui <- shiny::fluidPage(
  shiny::titlePanel(title),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      ewing::ewingInput("ewing")
    ),
    shiny::mainPanel(
      ewing::ewingOutput("ewing")
    )
  )
)
server <- function(input, output, server) {
  ewing::ewingServer("ewing")
}

shiny::shinyApp(ui = ui, server = server)

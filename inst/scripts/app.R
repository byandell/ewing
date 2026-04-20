# inst/scripts/app.R
source("watershed.R")
source("watershedApp.R")

title <- "Ewing Watershed Projection"
ui <- shiny::fluidPage(
  shiny::titlePanel(title),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      watershedInput("watershed")
    ),
    shiny::mainPanel(
      watershedOutput("watershed")
    )
  )
)

server <- function(input, output, session) {
  watershedServer("watershed")
}

shiny::shinyApp(ui = ui, server = server)

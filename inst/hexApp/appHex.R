# Following needed to prepare for Posit Connect
# renv::install("byandell/ewing")
# renv::snapshot()
# renv::record("byandell/ewing")

library(leaflet)
library(sf)
library(terra)
library(ewing)

title <- "Hexagonal Watershed Map"

ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            ewing::hexmapInput("hexmap")
        ),
        shiny::mainPanel(
            ewing::hexmapOutput("hexmap")
        )
    )
)

server <- function(input, output, session) {
    ewing::hexmapServer("hexmap")
}

shiny::shinyApp(ui = ui, server = server)

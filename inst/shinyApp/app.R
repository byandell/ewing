devtools::install_github("byandell/ewing")

# Define UI for app that draws a histogram ----
ui <- ewing::ewingUI()

server <- function(input, output) {
  ewing::ewingServer(input, output)
} 

shiny::shinyApp(ui = ui, server = server)

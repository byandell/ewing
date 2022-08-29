library(shiny)
library(ewing)
library(tidyverse)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Population Ethology"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      tagList(
        # Input: Slider for the number of bins ----
        sliderInput(inputId = "host",
                    label = "Number of hosts:",
                    min = 100,
                    max = 500,
                    value = 200),
        sliderInput(inputId = "parasite",
                    label = "Number of parasite:",
                    min = 100,
                    max = 500,
                    value = 200)
      )      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tagList(
        # Output: Histogram ----
        plotOutput(outputId = "distPlot"),
        plotOutput(outputId = "hostPlot"),
        plotOutput(outputId = "parasitePlot")
      )
    )
  )
)
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  simres <- reactive({
    req(input$host, input$parasite)
    mysim <- init.simulation(count = as.numeric(c(input$host, input$parasite))) # initialize simulation
    future.events(mysim, "mysim.out", plotit = FALSE) # simulate future events
  })
  output$distPlot <- renderPlot({
    ggplot_ewing(simres())
  })
  output$hostPlot <- renderPlot({
    ggplot_current(simres(), "host") + 
      ggtitle(paste("host", "on substrate"))
  })
  output$parasitePlot <- renderPlot({
    ggplot_current(simres(), "parasite") + 
      ggtitle(paste("parasite", "on substrate"))
  })
  
}
shinyApp(ui = ui, server = server)

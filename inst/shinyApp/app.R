devtools::install_github("byandell/ewing")

# Define UI for app that draws a histogram ----
ui <- shiny::fluidPage(
  
  # App title ----
  shiny::titlePanel("Population Ethology"),
  
  # Sidebar layout with input and output definitions ----
  shiny::sidebarLayout(
    
    # Sidebar panel for inputs ----
    shiny::sidebarPanel(
      
      shiny::tagList(
        # Input: Slider for the number of bins ----
        shiny::sliderInput(inputId = "host",
                    label = "Number of hosts:",
                    min = 100,
                    max = 500,
                    value = 100,
                    step = 20),
        shiny::sliderInput(inputId = "parasite",
                    label = "Number of parasites:",
                    min = 100,
                    max = 500,
                    value = 100,
                    step = 20),
        shiny::sliderInput(inputId = "steps",
                    label = "Simulation steps:",
                    min = 1000,
                    max = 10000,
                    value = 1000,
                    step = 500),
        shiny::checkboxInput("norm",
                      "Normalize Plot",
                      TRUE),
        shiny::checkboxInput("total",
                      "Include Total",
                      TRUE),
        shiny::textInput("outfile", "Output File", "mysim.out"),
        shiny::textInput("plotfile", "Plot File", "myplot.pdf"),
        shiny::downloadButton("downloadPlot", "Plots")
      )      
    ),
    
    # Main panel for displaying outputs ----
    shiny::mainPanel(
      
      shiny::tagList(
        shiny::uiOutput("uifile"),
        shiny::plotOutput(outputId = "distPlot", height = "4in"),
        shiny::plotOutput(outputId = "hostPlot", height = "2in"),
        shiny::plotOutput(outputId = "parasitePlot", height = "2in")
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
  siminit <- shiny::reactive({
    shiny::req(input$host, input$parasite)
    mysim <- ewing::init.simulation(count = as.numeric(c(input$host, input$parasite))) # initialize simulation
  })
  simres <- shiny::reactive({
    # Ideally, would like to continue simulation. That would require
    # - feed simres() back into future.events, which requires some logic
    # - use option "append = TRUE" to append to outfile
    shiny::req(input$steps)
    future.events(siminit(), nstep = input$steps, plotit = FALSE) # simulate future events
  })
  distplot <- shiny::reactive({
    ewing::ggplot_ewing(simres(), total = input$total, normalize = input$norm)
  })
  output$distPlot <- shiny::renderPlot({
    distplot()
  })
  hostplot <- shiny::reactive({
    ewing::ggplot_current(simres(), "host") + 
      ggplot2::ggtitle(paste("host", "on substrate"))
  })
  output$hostPlot <- shiny::renderPlot({
    hostplot()
  })
  parasiteplot <- shiny::reactive({
    ewing::ggplot_current(simres(), "parasite") + 
      ggplot2::ggtitle(paste("parasite", "on substrate"))
  })
  output$parasitePlot <- shiny::renderPlot({
    parasiteplot()
  })

  output$uifile <- shiny::renderUI({
    out <- "nada"
    if(shiny::isTruthy(simres())) {
      out <- paste("Size of counts table:", dim(simres()$count$counts), collapse = " ")
    }
    out
  })
  
  output$downloadRun <- shiny::downloadHandler(
    filename = function() {
      file.path(req(input$outfile)) },
    content = function(file) {
      out <- ewing::readCount(simres())
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      file.path(req(input$plotfile)) },
    content = function(file) {
      grDevices::pdf(file, width = 9)
      print(distplot())
      print(hostplot())
      print(parasiteplot())
      grDevices::dev.off()
    }
  )
  
}
shiny::shinyApp(ui = ui, server = server)

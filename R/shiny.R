#' @export
#' @importFrom shiny column fluidPage fluidRow mainPanel sidebarLayout sidebarPanel tagList titlePanel 
#'                   checkboxInput selectInput sliderInput plotOutput textInput uiOutput
#'                   HTML
#'                   downloadButton
#'                   actionButton bindCache bindEvent
#' 
ewingUI <- function() {
  shiny::fluidPage(
    
    # App title ----
    shiny::titlePanel("Population Ethology"),
    
    # Sidebar layout with input and output definitions ----
    shiny::sidebarLayout(
      
      # Sidebar panel for inputs ----
      shiny::sidebarPanel(
        
        shiny::tagList(
          shiny::sliderInput(inputId = "host",
                             label = "Number of hosts:",
                             min = 0,
                             max = 500,
                             value = 100,
                             step = 20),
          shiny::sliderInput(inputId = "parasite",
                             label = "Number of parasites:",
                             min = 0,
                             max = 500,
                             value = 100,
                             step = 20),
          shiny::sliderInput(inputId = "steps",
                             label = "Simulation steps:",
                             min = 1000,
                             max = 10000,
                             value = 1000,
                             step = 500),
          shiny::actionButton("go", "Start Simulation"),
          shiny::HTML("<hr>"),
          shiny::checkboxInput("norm",
                               "Normalize Plot",
                               TRUE),
          shiny::checkboxInput("total",
                               "Include Total",
                               TRUE),
          shiny::fluidRow(
            shiny::column(6,
                          shiny::textInput("outfile", "Species Table", "mysim.csv")),
            shiny::column(3,
                          shiny::selectInput("species", "", c("host", "parasite"), "host")),
            shiny::column(3,
                          shiny::downloadButton("downloadRun", "Table"))),
          shiny::fluidRow(
            shiny::column(9,
                          shiny::textInput("plotfile", "Plot File", "myplot.pdf")),
            shiny::column(3,
                          shiny::downloadButton("downloadPlot", "Plots"))),
          shiny::uiOutput("uifile"),
          shiny::HTML("See <a href='https://github.com/byandell/ewing'>ewing package on github</a>")
        )      
      ),
      
      # Main panel for displaying outputs ----
      shiny::mainPanel(
        
        shiny::tagList(
          shiny::plotOutput(outputId = "distPlot", height = "4in"),
          shiny::plotOutput(outputId = "hostPlot", height = "2in"),
          shiny::plotOutput(outputId = "parasitePlot", height = "2in")
        )
      )
    )
  )
}

#' @export
#' @importFrom shiny isTruthy reactive req
#'                   renderPlot renderUI
#'                   downloadHandler
#'                   
ewingServer <- function(input, output) {
  
  simres <- shiny::bindEvent(
    shiny::bindCache(
      shiny::reactive({
        # Ideally, would like to continue simulation. That would require
        # - feed simres() back into future.events, which requires some logic
        # - use option "append = TRUE" to append to outfile
        siminit <- init.simulation(count = as.numeric(c(input$host, input$parasite))) # initialize simulation
        future.events(siminit, nstep = input$steps, plotit = FALSE) # simulate future events
      }),
      input$host, input$parasite, input$steps, input$go),
    input$go)
      
  distplot <- shiny::reactive({
    ggplot_ewing(simres(), total = input$total, normalize = input$norm)
  })
  output$distPlot <- shiny::renderPlot({
    distplot()
  })
  hostplot <- shiny::reactive({
    ggplot_current(simres(), "host") + 
      ggplot2::ggtitle(paste("host", "on substrate"))
  })
  output$hostPlot <- shiny::renderPlot({
    hostplot()
  })
  parasiteplot <- shiny::reactive({
    ggplot_current(simres(), "parasite") + 
      ggplot2::ggtitle(paste("parasite", "on substrate"))
  })
  output$parasitePlot <- shiny::renderPlot({
    parasiteplot()
  })

  output$uifile <- shiny::renderUI({
    out <- "nada"
    if(shiny::isTruthy(simres())) {
      out <- paste("Size of counts table:", 
                   paste(dim(simres()$count$counts), collapse = " "))
    }
    out
  })
  
  data <- reactive({
    readCount(simres())[[shiny::req(input$species)]]
  })
  output$downloadRun <- shiny::downloadHandler(
    filename = function() {
      paste(shiny::req(input$species), shiny::req(input$outfile), sep = ".") },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      file.path(shiny::req(input$plotfile)) },
    content = function(file) {
      grDevices::pdf(file, width = 9)
      print(distplot())
      print(hostplot())
      print(parasiteplot())
      grDevices::dev.off()
    }
  )
}

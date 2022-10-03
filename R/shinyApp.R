#' @export
#' @importFrom shiny column fluidPage fluidRow mainPanel sidebarLayout sidebarPanel tagList titlePanel 
#'                   checkboxInput selectInput sliderInput plotOutput textInput uiOutput
#'                   HTML h2
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
          shiny::h4("Simulation Settings"),
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
          shiny::selectInput("nsim",
                             "Number of Simulations",
                             c(1,2,5,10,20,50,100,200),
                             1),
          shiny::actionButton("go", "Start Simulation"),
          shiny::HTML("<hr  style='height:1px;border:none;color:#333;background-color:#333;' />"),
          shiny::conditionalPanel(
            condition = "input.nsim != '1'",
            shiny::checkboxInput("conf", "Confidence band (sim>=10)", FALSE)),
          shiny::conditionalPanel(
            condition = "input.go != '0'",
            shiny::tagList(
              shiny::fluidRow(
                shiny::checkboxInput("norm",
                                     "Normalize Plot",
                                     TRUE),
                shiny::checkboxInput("total",
                                     "Include Total",
                                     TRUE),
                shiny::h4("Save Files"),
                shiny::column(
                  6,
                  shiny::textInput("outfile", "Species Table", "mysim.csv")),
                shiny::column(
                  3,
                  shiny::selectInput("species", "", c("host", "parasite"), "host")),
                shiny::column(
                  3,
                  shiny::downloadButton("downloadRun", "Table"))),
              shiny::fluidRow(
                shiny::column(
                  9,
                  shiny::textInput("plotfile", "Plot File", "myplot.pdf")),
                shiny::column(
                  3,
                  shiny::downloadButton("downloadPlot", "Plots"))))),
          shiny::uiOutput("uifile"),
          shiny::HTML("See <a href='https://github.com/byandell/ewing'>ewing package on github</a>"),
          shiny::uiOutput("version")
        )      
      ),
      
      # Main panel for displaying outputs ----
      shiny::mainPanel(
        shiny::uiOutput("outs")
      )
    )
  )
}

#' @export
#' @importFrom shiny isTruthy reactive req
#'                   renderPlot renderUI
#'                   downloadHandler
#'                   incProgress withProgress
#' @importFrom utils write.csv
#'                   
ewingServer <- function(input, output) {
  
  simres <- shiny::bindEvent(
    shiny::bindCache(
      shiny::reactive({
        nsim <- as.integer(input$nsim)
        if(nsim == 1) {
          # Ideally, would like to continue simulation. That would require
          # - feed simres() back into future.events, which requires some logic
          # - use option "append = TRUE" to append to outfile
          siminit <- init.simulation(count = as.numeric(c(input$host, input$parasite))) # initialize simulation
          future.events(siminit, nstep = input$steps, plotit = FALSE) # simulate future events
        } else {
          shiny::withProgress(message = paste('Ewing Discrete', nsim,
                                              'Simulations ...'),
                              value = 0,
          {
            out <- as.list(seq_len(nsim))
            inc <- 1 / nsim
            for(i in seq_len(nsim)) {
              shiny::incProgress(inc)
              out[[i]] <- ewing_discrete1()
            }
            make_ewing_discrete(out)
          })
        }
      }),
      input$host, input$parasite, input$steps, input$nsim, input$go),
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
  envelopePlot <- shiny::reactive({
    shiny::req(simres())
    nsim <- as.integer(shiny::req(input$nsim))
    conf <- (nsim >= 10) & input$conf 
    if(inherits(simres(), "ewing_discrete")) {
      ggplot_ewing_envelopes(
        simres(),
        conf)
    } else {
      NULL
    }
  })
  output$envPlot <- shiny::renderPlot({
    envelopePlot()
  })
  output$plots <- shiny::renderUI({
    nsim <- as.integer(shiny::req(input$nsim), simres())
    if(nsim == 1) {
      shiny::tagList(
        shiny::plotOutput(outputId = "distPlot", height = "4in"),
        shiny::plotOutput(outputId = "hostPlot", height = "2in"),
        shiny::plotOutput(outputId = "parasitePlot", height = "2in")
      )
    } else {
      shiny::plotOutput(outputId = "envPlot")
    }
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
    nsim <- as.integer(shiny::req(input$nsim))
    species <- shiny::req(input$species)
    if(nsim == 1) {
      readCount(simres())[[species]]
    } else {
      if(nsim >= 10)
      summary(simres(), species = species)
    }
  })
  output$downloadRun <- shiny::downloadHandler(
    filename = function() {
      paste(shiny::req(input$species), shiny::req(input$outfile), sep = ".") },
    content = function(file) {
      utils::write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      file.path(shiny::req(input$plotfile)) },
    content = function(file) {
      grDevices::pdf(file, width = 9)
      nsim <- as.integer(shiny::req(input$nsim))
      if(nsim == 1) {
        print(distplot())
        print(hostplot())
        print(parasiteplot())
      } else {
        print(envelopePlot())
      }
      grDevices::dev.off()
    }
  )
  
  output$datafiles <- shiny::renderUI({
    shiny::tagList(
      shiny::selectInput("dataname", "",
                         c("organism.features", "future.host", "future.parasite",
                           "substrate.host", "substrate.parasite", "substrate.substrate"),
                         "organism.features"),
      shiny::renderDataTable({
        mydata(shiny::req(input$dataname), "ewing")
        out <- get(input$dataname)
        out
      }, escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10)))
#    showdata( "future.host")
#    showdata( "TemperaturePar")
#    showdata( "TemperatureBase")
  })
  
  output$outs <- shiny::renderUI({
    shiny::tagList(
      shiny::radioButtons("button", "", c("Plots", "Input Data"), "Plots", inline = TRUE),
      shiny::conditionalPanel(
        condition = "input.button == 'Plots'",
        shiny::uiOutput("plots")),
      shiny::conditionalPanel(
        condition = "input.button == 'Input Data'",
        shiny::uiOutput("datafiles")))
  })
  
  output$version <- shiny::renderText({
    paste("Ewing package version ", utils::packageVersion("ewing"))
  })
}

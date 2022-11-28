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
          shiny::radioButtons("nsim",
                             "Number of Simulations",
                             c(1,10,20,50,100,200),
                             1, inline = TRUE),
          
          shiny::actionButton("go", "Start Simulation"),
          
          shiny::conditionalPanel(
            condition = "input.go != '0'",
            shiny::tagList(
              shiny::HTML("<hr  style='height:1px;border:none;color:#333;background-color:#333;' />"),
              shiny::fluidRow(
                shiny::conditionalPanel(
                  condition = "input.nsim != '1'",
                  shiny::checkboxInput("conf", "Confidence band", FALSE)),
                shiny::conditionalPanel(
                  condition = "input.nsim == '1'",
                  shiny::tagList(
                    shiny::checkboxInput("norm",
                                         "Normalize Plot",
                                         TRUE),
                    shiny::checkboxInput("total",
                                         "Include Total",
                                         TRUE))),
                shiny::h4("Save Files"),
                shiny::column(
                  6,
                  shiny::textInput("outfile", "Species Table", "mysim")),
                shiny::column(
                  3,
                  shiny::selectInput("species", "", c("host", "parasite"), "host")),
                shiny::column(
                  3,
                  shiny::downloadButton("downloadRun", "Table"))),
              shiny::fluidRow(
                shiny::column(
                  9,
                  shiny::textInput("plotfile", "Plot File", "myplot")),
                shiny::column(
                  3,
                  shiny::downloadButton("downloadPlot", "Plots"))))),
          
          shiny::HTML("<hr  style='height:1px;border:none;color:#333;background-color:#333;' />"),
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
#'                   downloadHandler fileInput
#'                   incProgress withProgress
#' @importFrom utils write.csv
#' @importFrom stringr str_remove
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
          siminit <- init.simulation(count = as.numeric(c(input$host, input$parasite)),
                                     datafile = datafile()) # initialize simulation
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
              out[[i]] <- ewing_discrete1(count = as.numeric(c(input$host, input$parasite)), nstep = input$steps)
            }
            make_ewing_discrete(out)
          })
        }
      }),
      input$host, input$parasite, input$steps, input$nsim, input$go, input$datafile),
    input$go)
      
  distplot <- shiny::reactive({
    if(inherits(simres(), "ewing")) {
      ggplot_ewing(simres(), total = input$total, normalize = input$norm)
    } else {
      NULL
    }
  })
  output$distPlot <- shiny::renderPlot({
    distplot()
  })
  hostplot <- shiny::reactive({
    if(inherits(simres(), "ewing")) {
      ggplot_current(simres(), "host") + 
        ggplot2::ggtitle(paste("host", "on substrate"))
    } else {
      NULL
    }
  })
  output$hostPlot <- shiny::renderPlot({
    hostplot()
  })
  parasiteplot <- shiny::reactive({
    if(inherits(simres(), "ewing")) {
      ggplot_current(simres(), "parasite") + 
        ggplot2::ggtitle(paste("parasite", "on substrate"))
    } else {
      NULL
    }
  })
  output$parasitePlot <- shiny::renderPlot({
    parasiteplot()
  })
  envdata <- shiny::reactive({
    shiny::req(simres())
    if(inherits(simres(), "ewing_discrete")) {
      ewing_envelopes(simres())
    } else {
      NULL
    }
  })
  envelopePlot <- shiny::reactive({
    shiny::req(envdata())
    nsim <- as.integer(shiny::req(input$nsim))
    conf <- (nsim >= 10) & input$conf 
    if(inherits(simres(), "ewing_discrete")) {
      ggplot_ewing_envelopes(
        envdata(),
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

  data <- reactive({
    nsim <- as.integer(shiny::req(input$nsim))
    species <- shiny::req(input$species)
    if(nsim == 1) {
      readCount(simres())[[species]]
    } else {
      shiny::req(envdata())
      print(envdata(), species = species)
    }
  })
  params <- shiny::reactive({
    nsim <- shiny::req(input$nsim)
    paste(shiny::req(input$host), shiny::req(input$parasite),
          shiny::req(input$steps), nsim, sep = "_")
  })
  output$downloadRun <- shiny::downloadHandler(
    filename = function() {
      paste0(paste(shiny::req(input$outfile), shiny::req(input$species), params(), sep = "_"), ".csv") },
    content = function(file) {
      utils::write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(paste(shiny::req(input$plotfile), params(), sep = "_"), ".pdf") },
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
  
  datanames <- shiny::reactive({
    if(datafile() == "") {
      c("organism.features", "future.host", "future.parasite",
        "substrate.host", "substrate.parasite", "substrate.substrate",
        "temperature.base", "temperature.par")
    } else {
      readxl::excel_sheets(datafile())
    }
  })
  output$inputfiles <- shiny::renderUI({
    shiny::tagList(
      shiny::selectInput("dataname", "", datanames(), "organism.features"),
      shiny::renderDataTable({
        out <- getOrgData(simres(),
                   left = stringr::str_remove(shiny::req(input$dataname), "\\..*"),
                   right = stringr::str_remove(shiny::req(input$dataname), ".*\\."),
                   messages = FALSE, datafile = datafile())
        # Kludge to reinstate rownames as a column
        if(!identical(rownames(out), as.character(seq_len(nrow(out))))) {
          out <- data.frame(rownames = rownames(out), out)
        }
        out
      }, escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10)))
  })
  
  datafile <- shiny::reactive({
    if(shiny::isTruthy(input$datafile)) {
      input$datafile$datapath
    } else {
      ""
    }
  })
  output$outs <- shiny::renderUI({
    shiny::tagList(
      shiny::radioButtons("button", "", c("Plots", "Input Data"), "Plots", inline = TRUE),
      shiny::conditionalPanel(
        condition = "input.button == 'Plots'",
        shiny::uiOutput("plots")),
      shiny::conditionalPanel(
        condition = "input.button == 'Input Data'",
        shiny::tagList(
          shiny::fileInput("datafile", "Optional XLSX Input Data File",
                           multiple = FALSE,
                           accept = c(".xls", ".xlsx")),
          shiny::uiOutput("inputfiles"))))
  })
  
  output$version <- shiny::renderText({
    paste("Ewing package version ", utils::packageVersion("ewing"))
  })
}

#' @export
#' @importFrom shiny column fluidPage fluidRow mainPanel sidebarLayout sidebarPanel tagList titlePanel 
#'                   checkboxInput selectInput sliderInput plotOutput textInput uiOutput
#'                   HTML h2
#'                   downloadButton
#'                   actionButton bindCache bindEvent
#' @importFrom ggplot2 autoplot ggplot ggtitle
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
          shiny::uiOutput("sppsize"),
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
          
          shiny::fileInput("datafile", "Optional XLSX Input Data File",
                           multiple = FALSE,
                           accept = c(".xls", ".xlsx")),
          
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
      ggplot2::autoplot(ewing_ageclass(simres(), total = input$total, normalize = input$norm))
    } else {
      NULL
    }
  })
  output$distPlot <- shiny::renderPlot({
    distplot()
  })
  # *** This is not right. Need to get each species name here and in `ewing_substrate`
  # species <- ewing:::getOrgFeature(simres)
  #   gives list but includes substrates.
  # can figure out what substrate goes to species with 
  # ewing:::getOrgFeature(simres, species[i], "substrate")
  # if it is NA (or "NA"), then that is a substrate.
  # So cycle through species generating plots.
  # put as much in `ewing_substrate` as possible.
  species <- shiny::reactive({
    get.organisms(datafile())$species
  })
  substrates <- shiny::reactive({
    get.organisms(datafile())$substrates
  })
  output$sppsize <- shiny::renderUI({
    shiny::req(species())
    lapply(species(), function(x) {
      shiny::sliderInput(inputId = x,
                         label = paste0("Number of ", x, "s:"),
                         min = 0,
                         max = 500,
                         value = 100,
                         step = 20)
    })
  })
  sppplot <- shiny::reactive({
    shiny::req(species(), simres())
    if(inherits(simres(), "ewing")) {
      if(!is.null(simres())) {
        p <- lapply(species(), function(x) {
          p <- ggplot2::autoplot(ewing_substrate(simres(), x))
          if(inherits(p, "ggplot"))
            p <- p + ggplot2::ggtitle(paste(x, "on", substrates()[1]))
          p
        })
        if(any(unlist(map(p, is.null))))
          p <- NULL
        p
      }
    } else {
      ggplot2::ggplot()
    }
  })
  output$sppPlot <- shiny::renderPlot({
    if(!is.null(sppplot())) {
      spp <- length(species())
      p <- sppplot()[[1]]
      if(spp > 1) for(i in seq(2, spp)) {
        p <- p + sppplot()[[i]]
      }
      p + patchwork::plot_layout(nrow = spp)
    } else {
      ggplot2::ggplot()
    }
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
      shiny::req(species())
      shiny::tagList(
        shiny::plotOutput(outputId = "distPlot", height = "4in"),
        shiny::plotOutput(outputId = "sppPlot", height = paste0(2 * length(species()), "in")))
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
        for(i in species()) {
          print(sppplot()[[i]])
        }
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
        shiny::uiOutput("inputfiles")))
  })
  
  output$version <- shiny::renderText({
    paste("Ewing package version ", utils::packageVersion("ewing"))
  })
}

#' @export
#' @importFrom utils write.csv
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom shiny actionButton bindCache bindEvent checkboxInput column
#'             downloadButton downloadHandler fileInput fluidPage fluidRow HTML
#'             h2 incProgress isTruthy mainPanel moduleServer NS plotOutput
#'             reactive renderPlot renderUI req selectInput sidebarLayout
#'             sidebarPanel sliderInput tagList textInput titlePanel withProgress
#'             uiOutput
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' @importFrom DT renderDataTable
#' 
ewingServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
                                    out[[i]] <- ewing_discrete1(
                                      count = as.numeric(c(input$host, input$parasite)),
                                      nstep = input$steps)
                                  }
                                  make_ewing_discrete(out)
                                })
          }
        }),
        input$host, input$parasite, input$steps, input$nsim, input$go,
        input$datafile),
      input$go)
    
    distplot <- shiny::reactive({
      if(inherits(simres(), "ewing")) {
        ggplot2::autoplot(ewing_ageclass(simres(), total = input$total,
                                         normalize = input$norm))
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
        shiny::sliderInput(ns(x),
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
          if(any(unlist(purrr::map(p, is.null))))
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
        ggplot_ewing_envelopes(envdata(), conf)
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
          shiny::plotOutput(ns("distPlot"), height = "4in"),
          shiny::plotOutput(ns("sppPlot"), height = paste0(2 * length(species()), "in")))
      } else {
        shiny::plotOutput(ns("envPlot"))
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
      getOrgNames(datafile())
    })
    output$inputfiles <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(ns("dataname"), "", datanames(), "organism.features"),
        DT::renderDataTable({
          getOrgDataSimple(simres(),shiny::req(input$dataname), datafile())
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
      switch(input$button,
             Plots        =  shiny::uiOutput(ns("plots")),
             "Input Data" =  shiny::uiOutput(ns("inputfiles")))
    })
    
    output$plottype <- shiny::renderUI({
      if(input$nsim == 1) {
        shiny::tagList(
          shiny::checkboxInput(ns("norm"), "Normalize Plot", TRUE),
          shiny::checkboxInput(ns("total"), "Include Total", TRUE))
      } else {
        shiny::checkboxInput(ns("conf"), "Confidence band", FALSE)
      }
    })
    output$version <- shiny::renderText({
      paste("Ewing package version ", utils::packageVersion("ewing"))
    })
  })
}
#' Ewing Input
#' @export
#' @rdname ewingServer
ewingInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Simulation Settings"),
    shiny::uiOutput(ns("sppsize")),
    shiny::sliderInput(ns("steps"),
                       label = "Simulation steps:",
                       min = 1000,
                       max = 10000,
                       value = 1000,
                       step = 500),
    shiny::radioButtons(ns("nsim"),
                        "Number of Simulations",
                        c(1,10,20,50,100,200),
                        1, inline = TRUE),
    
    shiny::fileInput(ns("datafile"), "Optional XLSX Input Data File",
                     multiple = FALSE,
                     accept = c(".xls", ".xlsx")),
    
    shiny::actionButton(ns("go"), "Start Simulation"),
    
    shiny::HTML("<hr style='height:1px;border:none;color:#333;background-color:#333;' />"),
    shiny::uiOutput(ns("plottype")),
    shiny::h4("Save Files"),
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(ns("outfile"), "Species Table", "mysim")),
      shiny::column(3, shiny::selectInput(ns("species"), "", c("host", "parasite"), "host")),
      shiny::column(3, shiny::downloadButton(ns("downloadRun"), "Table"))),
    shiny::fluidRow(
      shiny::column(9, shiny::textInput(ns("plotfile"), "Plot File", "myplot")),
      shiny::column(3, shiny::downloadButton(ns("downloadPlot"), "Plots"))),
    
    shiny::HTML("<hr  style='height:1px;border:none;color:#333;background-color:#333;' />"),
    shiny::HTML("See <a href='https://github.com/byandell/ewing'>ewing package on github</a>"),
    shiny::uiOutput(ns("version"))
  )
}
#' Ewing Output
#' @export
#' @rdname ewingServer
ewingOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("button"), "", c("Plots", "Input Data"),
                        "Plots", inline = TRUE),
    shiny::uiOutput(ns("outs"))
  )
}
#' Ewing App
#' @export
#' @rdname ewingServer
ewingApp <- function(title = "Population Ethology") {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        ewingInput("ewing")
      ),
      shiny::mainPanel(
        ewingOutput("ewing")
    )))
  
  server <- function(input, output, server) {
    ewingServer("ewing")
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
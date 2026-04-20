#' Download File App
#' 
#' Shiny application components for exporting evaluation metrics, data tables, and dynamic plot
#' graphics out of the simulation runtime.
#' 
#' @param title Application title
#' @param id module ID string
#' @param sim_data reactive list holding the primary `simApp` state values (`simres`, `nsim`)
#' @param distplot reactive evaluating to the main age class visualization
#' @param sppplot reactive evaluating to the host/parasite substrate plot grids 
#' @param envplot reactive evaluating to the generalized statistical envelopes
#' @export
#' @importFrom utils write.csv
#' @importFrom shiny column downloadButton downloadHandler fluidRow NS req selectInput textInput
#' @importFrom grDevices pdf dev.off
downloadApp <- function(title = "Download Options") {
  # Stub for testing downloadApp independently
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      downloadInput("download")
    ),
    "Test Server Hook"
  )
  server <- function(input, output, server) { }
  shiny::shinyApp(ui = ui, server = server)
}

#' @export
#' @rdname downloadApp
downloadServer <- function(id, sim_data, distplot = shiny::reactive(NULL), sppplot = shiny::reactive(NULL), envplot = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Internal parameters string extraction
    params <- shiny::reactive({
      nsim <- shiny::req(sim_data$nsim)
      inps <- shiny::req(sim_data$input)
      paste(inps$steps, nsim(), sep = "_") # Modified bounds logic since we dynamically extract
    })
    
    # CSV Data Generator extraction mapping exactly to original ewingApp
    data <- shiny::reactive({
      nsim <- as.integer(shiny::req(sim_data$nsim()))
      sp <- shiny::req(input$species)
      res <- shiny::req(sim_data$simres())
      if(nsim == 1) {
        readCount(res)[[sp]]
      } else {
        # For discrete states, evaluate envdata
        env_d <- shiny::req(envplot$envdata())
        print(env_d, species = sp)
      }
    })
    
    output$downloadRun <- shiny::downloadHandler(
      filename = function() {
        paste0(paste(shiny::req(input$outfile), shiny::req(input$species), params(), sep = "_"), ".csv") 
      },
      content = function(file) {
        utils::write.csv(data(), file, row.names = FALSE)
      }
    )
    
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        paste0(paste(shiny::req(input$plotfile), params(), sep = "_"), ".pdf") 
      },
      content = function(file) {
        grDevices::pdf(file, width = 9)
        on.exit(grDevices::dev.off(), add = TRUE)
        nsim <- as.integer(shiny::req(sim_data$nsim()))
        
        if(nsim == 1) {
          # Handle discrete plot captures natively
          print(shiny::req(distplot()))
          sp_lists <- shiny::req(sppplot())
          if (!is.null(sp_lists)) {
            for(p in sp_lists) {
              if(!is.null(p)) print(p)
            }
          }
        } else {
          print(shiny::req(envplot$envelopePlot()))
        }
      }
    )
  })
}

#' @export
#' @rdname downloadApp
downloadInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Save Files"),
    shiny::fluidRow(
      shiny::column(6, shiny::textInput(ns("outfile"), "Species Table", "mysim")),
      shiny::column(3, shiny::selectInput(ns("species"), "", c("host", "parasite"), "host")),
      shiny::column(3, shiny::downloadButton(ns("downloadRun"), "Table"))),
    shiny::fluidRow(
      shiny::column(9, shiny::textInput(ns("plotfile"), "Plot File", "myplot")),
      shiny::column(3, shiny::downloadButton(ns("downloadPlot"), "Plots")))
  )
}

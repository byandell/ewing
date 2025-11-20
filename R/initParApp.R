#' Ewing Initialization App
#' 
#' @export
#' @importFrom utils write.csv
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom shiny actionButton bindCache bindEvent checkboxInput column
#'             downloadButton downloadHandler fileInput fluidPage fluidRow HTML
#'             h4 incProgress isTruthy mainPanel moduleServer NS plotOutput
#'             reactive renderPlot renderUI req selectInput sidebarLayout
#'             sidebarPanel sliderInput tagList textInput titlePanel
#'             withProgress uiOutput
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' @importFrom bslib page_sidebar sidebar
#' @importFrom DT renderDataTable
#' @importFrom cowplot plot_grid
initParApp <- function(title = "Population Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      initParInput("init_par")),
    initParUI("init_par"),
    initParOutput("init_par")
  )
  server <- function(input, output, server) {
    init_par <- initParServer("init_par")
  }
  shiny::shinyApp(ui = ui, server = server)
}
#' @export
#' @rdname initParApp
initParServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    species <- shiny::reactive({
      get.organisms()$species
    })
    output$sppsize <- shiny::renderUI({
      shiny::req(species()) # "host", "parasite"
      lapply(species(), function(x) {
        shiny::sliderInput(ns(x),
                           label = paste0("Number of ", x, "s:"),
                           min = 0,
                           max = 500,
                           value = 100,
                           step = 20)
      })
    })

    datanames <- shiny::reactive({
      getOrgNames()
    })
    output$inputfiles <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(ns("dataname"), "", datanames(), "organism.features"),
        DT::renderDataTable({
          getOrgDataSimple(simres(),shiny::req(input$dataname))
        }, escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    # Show parameters
    output$show_par <- shiny::renderUI({
      nlist <- names(input)
      # Remove any internal inputs, which have numbers.
      glist <- grep("[0-9]", names(input))
      if(length(glist))
        nlist <- nlist[-glist]
      # Construct output string.
      out <- paste0("inputs: ", paste(nlist, collapse = ", "))
      for(i in nlist) {
        out <- paste(out, "<br>",
                     paste(i, input[[i]], sep = " = "))
      }
      shiny::HTML(out)
    })
    
    # Return.
    input
  })
}
#' @export
#' @rdname initParApp
initParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sppsize"))
}
#' @export
#' @rdname initParApp
initParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_par"))
}
#' @export
#' @rdname initParApp
initParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("inputfiles"))
}

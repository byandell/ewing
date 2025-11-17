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
ewingInitApp <- function(title = "Population Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      ewingInitInput("ewing")),
    shiny::uiOutput("siminit"),
    ewingInitOutput("ewing")
  )
  server <- function(input, output, server) {
    siminit <- ewingInitServer("ewing")
    output$siminit <- shiny::renderUI({
      simstuff <- shiny::req(siminit())
      nlist <- names(simstuff)
      out <- paste(nlist, collapse = ", ")
      for(i in nlist) {
        out <- paste(out, "<br>",
                     paste(names(simstuff[[i]]), collapse = ", "))
      }
      shiny::HTML(out)
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
#' @export
#' @rdname ewingInitApp
ewingInitServer <- function(id) {
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
    siminit <- shiny::reactive({
      init.simulation(count = as.numeric(c(shiny::req(input$host),
                                           shiny::req(input$parasite))))
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

    # Return.
    siminit
  })
}
#' Ewing Input
#' @export
#' @rdname ewingInitApp
ewingInitInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sppsize"))
}
#' Ewing Output
#' @export
#' @rdname ewingInitApp
ewingInitOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("inputfiles"))
}

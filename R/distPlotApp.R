#' Distribution Plot App
#' 
#' Shiny UI components and module server logic for visualizing population age-class 
#' distribution distributions and demographic structures.
#' 
#' @param title Application title
#' @param id module ID string
#' @param simres reactive simulation state holding the active `ewing` data object
#' @export
#' @importFrom utils write.csv
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom shiny actionButton bindCache bindEvent checkboxInput column
#' @importFrom shiny downloadButton downloadHandler fileInput fluidPage fluidRow
#' @importFrom shiny HTML h4 incProgress isTruthy mainPanel moduleServer NS
#' @importFrom shiny plotOutput reactive renderPlot renderUI req selectInput
#' @importFrom shiny sidebarLayout sidebarPanel sliderInput tagList textInput
#' @importFrom shiny titlePanel withProgress uiOutput
#' @importFrom ggplot2 autoplot ggplot ggtitle
#' @importFrom DT renderDataTable
#' @importFrom cowplot plot_grid
distPlotApp <- function(title = "Population Ethology") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      initParInput("init_par"),
      distPlotInput("dist_plot"),
      futureInput("future")),
    distPlotOutput("dist_plot")
  )
  server <- function(input, output, server) {
    init_par <- initParServer("init_par")
    siminit <- initServer("init", init_par)
    simres <- futureServer("future", siminit)
    distPlotServer("dist_plot", simres)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
#' @export
#' @rdname distPlotApp
distPlotServer <- function(id, simres, x_var = NULL, total = NULL, norm = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dist_plot <- shiny::reactive({
      sim <- if (is.reactive(simres)) simres() else simres
      shiny::req(sim)
      
      tot_val <- if (is.reactive(total)) total() else if (!is.null(total)) total else if (!is.null(input$total)) input$total else TRUE
      norm_val <- if (is.reactive(norm)) norm() else if (!is.null(norm)) norm else if (!is.null(input$norm)) input$norm else TRUE
      xv_val <- if (is.reactive(x_var)) x_var() else if (!is.null(x_var)) x_var else if (!is.null(input$x_var)) input$x_var else "step"
      
      if (is.null(tot_val)) tot_val <- TRUE
      if (is.null(norm_val)) norm_val <- TRUE
      if (is.null(xv_val) || !xv_val %in% c("step", "time")) xv_val <- "step"
      
      object <- tryCatch({
        ewing_ageclass(sim, total = tot_val, normalize = norm_val)
      }, error = function(e) NULL)
      
      if (is.null(object)) return(plot_null("no simulation"))
      ggplot2::autoplot(object, x_var = xv_val)
    })
    
    output$dist_plot <- shiny::renderPlot({
      dist_plot()
    })
    
    # Return.
    dist_plot
  })
}
#' @export
#' @rdname distPlotApp
distPlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxInput(ns("norm"), "Normalize Plot", TRUE),
    shiny::checkboxInput(ns("total"), "Include Total", TRUE))
}
#' @export
#' @rdname distPlotApp
distPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("dist_plot"), height = "400px")
}

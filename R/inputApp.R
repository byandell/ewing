#' Input Data App
#'
#' Shiny module for inspecting simulation input parameter data tables, organism features,
#' stage transition futures, and substrate interaction matrices.
#'
#' @param title Application title
#' @param id module ID string
#' @param simres Reactive simulation state (`ewing` object)
#' @param datafile Reactive optional datafile path
#' @export
#' @importFrom shiny moduleServer NS renderTable req selectInput tableOutput tagList uiOutput
#' @importFrom bslib page_sidebar sidebar card
inputApp <- function(title = "Input Data Explorer") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      inputAppInput("input_app")
    ),
    bslib::card(
      inputAppOutput("input_app")
    )
  )
  server <- function(input, output, session) {
    inputAppServer("input_app")
  }
  shiny::shinyApp(ui = ui, server = server)
}

#' Input Data Controls Module
#' @export
#' @rdname inputApp
inputAppInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("dataname"), "Select Dataset Table:",
      choices = c(
        "organism.features", "future.host", "future.parasite",
        "substrate.host", "substrate.parasite", "substrate.substrate",
        "host.parasite", "temperature.base", "temperature.par"
      ),
      selected = "organism.features"
    )
  )
}

#' Input Data Table Output Module
#' @export
#' @rdname inputApp
inputAppOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tableOutput(ns("org_table"))
}

#' Input Data Server Module
#' @param id Module ID string
#' @param simres Reactive simulation state (`ewing` object)
#' @param datafile Reactive optional datafile path
#' @export
#' @rdname inputApp
inputAppServer <- function(id, simres = shiny::reactiveVal(NULL), datafile = shiny::reactiveVal("")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$org_table <- shiny::renderTable(
      {
        name <- input$dataname %||% "organism.features"
        sim <- if (is.reactive(simres)) simres() else simres
        if (is.null(sim)) sim <- init.simulation()
        sim_single <- if (inherits(sim, "ewing_discrete")) sim[[1]] else sim

        switch(name,
          "organism.features" = {
            ft_h <- getOrgFeature(sim_single, "host", "features")
            if (is.null(ft_h) || length(ft_h) == 0) {
              data.frame(species = c("host", "parasite"), count = c(200, 100), stringsAsFactors = FALSE)
            } else {
              as.data.frame(ft_h)
            }
          },
          "future.host" = getOrgFuture(sim_single, "host"),
          "future.parasite" = getOrgFuture(sim_single, "parasite"),
          "substrate.host" = getOrgInteract(sim_single, "substrate", "host"),
          "substrate.parasite" = getOrgInteract(sim_single, "substrate", "parasite"),
          "substrate.substrate" = getOrgInteract(sim_single, "substrate", "substrate"),
          "host.parasite" = getOrgInteract(sim_single, "host", "parasite"),
          "temperature.base" = getOrgData(sim_single, "temperature", "base"),
          "temperature.par" = getOrgData(sim_single, "temperature", "par"),
          data.frame(Info = paste("Dataset", name, "selected"))
        )
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE
    )
  })
}

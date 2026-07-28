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
        dfile <- if (is.reactive(datafile)) datafile() else datafile

        if (is.null(sim)) sim <- tryCatch(init.simulation(datafile = dfile), error = function(e) NULL)
        sim_single <- if (inherits(sim, "ewing_discrete")) sim[[1]] else sim

        res <- NULL

        # 1. Check if dataset is stored in sim$datasets (e.g. injected in webR demo)
        if (!is.null(sim_single$datasets) && !is.null(sim_single$datasets[[name]])) {
          res <- sim_single$datasets[[name]]
        }

        # 2. Extract dynamically via getOrgDataSimple or getOrg* package routines
        if (is.null(res)) {
          res <- tryCatch({
            getOrgDataSimple(sim_single, name, datafile = dfile)
          }, error = function(e) NULL)
        }

        # 3. Dynamic fallback to extracting from sim_single$org state structures
        if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
          res <- tryCatch({
            left <- stringr::str_remove(name, "\\..*")
            right <- stringr::str_remove(name, ".*\\.")

            if (left == "organism" && right == "features") {
              if (!is.null(sim_single$org$Feature)) as.data.frame(sim_single$org$Feature) else NULL
            } else if (left == "future") {
              if (!is.null(sim_single$org$Future[[right]])) sim_single$org$Future[[right]] else getOrgFuture(sim_single, right)
            } else if (!is.null(sim_single$org$Interact[[left]][[right]])) {
              sim_single$org$Interact[[left]][[right]]
            } else if (!is.null(sim_single$org[[left]][[right]])) {
              sim_single$org[[left]][[right]]
            } else {
              NULL
            }
          }, error = function(e) NULL)
        }

        if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
          res <- data.frame(Info = paste("Dataset", name, "is not available in current simulation instance."))
        }

        res
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE
    )
  })
}

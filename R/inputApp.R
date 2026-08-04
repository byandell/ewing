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
#' @importFrom shiny moduleServer NS renderTable req selectInput tableOutput tagList uiOutput observe updateSelectInput
#' @importFrom bslib page_sidebar sidebar card
#' @importFrom tools file_path_sans_ext
#' @importFrom utils read.table read.csv
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

#' Helper to dynamically discover available dataset tables from an input folder or simulation instance
#' @param datafile Character path to folder or Excel file containing input tables
#' @param sim Simulation instance (`ewing` or `isle_royale_sim`)
#' @export
#' @rdname inputApp
discover_dataset_tables <- function(datafile = "", sim = NULL) {
  found <- character(0)
  
  # 1. Inspect datafile directory
  d_path <- if (is.character(datafile) && datafile != "") datafile else if (!is.null(sim) && !is.null(sim$datafile)) sim$datafile else ""
  
  if (d_path != "" && file.exists(d_path)) {
    if (dir.exists(d_path)) {
      files <- list.files(d_path, pattern = "\\.(txt|csv|rds)$", full.names = FALSE)
      if (length(files) > 0) {
        found <- tools::file_path_sans_ext(files)
      }
    } else if (grepl("\\.xlsx$", d_path, ignore.case = TRUE)) {
      sheets <- tryCatch(readxl::excel_sheets(d_path), error = function(e) character(0))
      if (length(sheets) > 0) found <- sheets
    }
  }
  
  # 2. Inspect sim$datasets if present
  if (!is.null(sim) && !is.null(sim$datasets)) {
    found <- unique(c(found, names(sim$datasets)))
  }
  if (!is.null(sim) && !is.null(sim$community) && !is.null(sim$community$datasets)) {
    found <- unique(c(found, names(sim$community$datasets)))
  }
  if (exists("isle_royale_datasets") && is.list(isle_royale_datasets)) {
    found <- unique(c(found, names(isle_royale_datasets)))
  }
  
  # 3. Default fallback choices if nothing found
  if (length(found) == 0) {
    found <- c(
      "organism.features", "future.moose", "future.wolf",
      "substrate.moose", "substrate.wolf", "substrate.substrate",
      "moose.wolf", "future.host", "future.parasite"
    )
  }
  
  unique(found)
}

#' Input Data Controls Module
#' @param id Module ID string
#' @param choices Optional vector of initial table choices
#' @export
#' @rdname inputApp
inputAppInput <- function(id, choices = NULL) {
  ns <- shiny::NS(id)
  default_choices <- if (!is.null(choices)) choices else c(
    "organism.features", "future.moose", "future.wolf",
    "substrate.moose", "substrate.wolf", "substrate.substrate",
    "moose.wolf", "future.host", "future.parasite"
  )
  shiny::tagList(
    shiny::selectInput(ns("dataname"), "Select Dataset Table:",
      choices = default_choices,
      selected = default_choices[1]
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
#' @param simres Reactive simulation state (`ewing` or `isle_royale_sim` object)
#' @param datafile Reactive optional datafile path or string
#' @export
#' @rdname inputApp
inputAppServer <- function(id, simres = shiny::reactiveVal(NULL), datafile = shiny::reactiveVal("")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamically update select choices based on folder / simulation contents
    shiny::observe({
      sim <- if (is.reactive(simres)) simres() else simres
      dfile <- if (is.reactive(datafile)) datafile() else datafile
      
      discovered <- discover_dataset_tables(dfile, sim)
      if (length(discovered) > 0) {
        current_sel <- input$dataname
        sel <- if (!is.null(current_sel) && current_sel %in% discovered) current_sel else discovered[1]
        shiny::updateSelectInput(session, "dataname", choices = discovered, selected = sel)
      }
    })

    output$org_table <- shiny::renderTable(
      {
        name <- input$dataname %||% "organism.features"
        sim <- if (is.reactive(simres)) simres() else simres
        dfile <- if (is.reactive(datafile)) datafile() else datafile

        res <- NULL

        # 0. Check direct file in datafile directory if points to folder
        if (is.character(dfile) && dfile != "" && dir.exists(dfile)) {
          txt_path <- file.path(dfile, paste0(name, ".txt"))
          if (file.exists(txt_path)) res <- tryCatch(utils::read.table(txt_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE), error = function(e) NULL)
          if (is.null(res)) {
            csv_path <- file.path(dfile, paste0(name, ".csv"))
            if (file.exists(csv_path)) res <- tryCatch(utils::read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
          }
        }

        # 1. Check if dataset is stored in sim$datasets or sim$community$datasets or global isle_royale_datasets
        if (is.null(res) && !is.null(sim)) {
          if (!is.null(sim$datasets) && !is.null(sim$datasets[[name]])) {
            res <- sim$datasets[[name]]
          } else if (!is.null(sim$community) && !is.null(sim$community$datasets) && !is.null(sim$community$datasets[[name]])) {
            res <- sim$community$datasets[[name]]
          }
        }
        if (is.null(res) && exists("isle_royale_datasets") && is.list(isle_royale_datasets) && !is.null(isle_royale_datasets[[name]])) {
          res <- isle_royale_datasets[[name]]
        }

        # 2. Extract dynamically via getOrgDataSimple or getOrg* package routines
        if (is.null(res) && !is.null(sim) && inherits(sim, "ewing")) {
          sim_single <- if (inherits(sim, "ewing_discrete")) sim[[1]] else sim
          res <- tryCatch({
            getOrgDataSimple(sim_single, name, datafile = dfile)
          }, error = function(e) NULL)
        }

        # 3. Dynamic fallback to extracting from sim_single$org state structures
        if ((is.null(res) || !is.data.frame(res) || nrow(res) == 0) && !is.null(sim) && inherits(sim, "ewing")) {
          sim_single <- if (inherits(sim, "ewing_discrete")) sim[[1]] else sim
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

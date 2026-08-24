#' Input Data App
#'
#' Shiny module for inspecting simulation input parameter data tables, organism features,
#' stage transition futures, and substrate interaction matrices across ecosystems.
#'
#' @param title Application title
#' @param id Module ID string
#' @param ecosystem Target ecosystem name (e.g. `"default"`, `"isle_royale"`) or directory path. Defaults to `"default"`.
#' @param simres Reactive simulation state (`ewing` object)
#' @param datafile Reactive optional datafile path
#' @export
#' @importFrom shiny moduleServer NS renderTable req selectInput tableOutput tagList uiOutput observe updateSelectInput reactive
#' @importFrom bslib page_sidebar sidebar card
#' @importFrom tools file_path_sans_ext
#' @importFrom utils read.table read.csv
inputApp <- function(title = "Input Data Explorer", ecosystem = "default") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      inputSystemInput("sys_select", selected = ecosystem),
      shiny::hr(),
      inputAppInput("input_app", ecosystem = ecosystem)
    ),
    bslib::card(
      inputAppOutput("input_app")
    )
  )
  server <- function(input, output, session) {
    eco <- inputSystemServer("sys_select")
    inputAppServer("input_app", ecosystem = eco)
  }
  shiny::shinyApp(ui = ui, server = server)
}

#' Input Data Controls Module
#' @param id Module ID string
#' @param choices Optional vector of initial table choices
#' @param ecosystem Target ecosystem name or directory path. Defaults to `"default"`.
#' @export
#' @rdname inputApp
inputAppInput <- function(id, choices = NULL, ecosystem = "default") {
  ns <- shiny::NS(id)
  default_ch <- if (!is.null(choices)) choices else default_choices(ecosystem)
  shiny::tagList(
    shiny::selectInput(ns("dataname"), "Select Dataset Table:",
      choices = default_ch,
      selected = default_ch[1]
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
#' @param datafile Reactive optional datafile path or string
#' @param ecosystem Reactive or character target ecosystem name or directory path. Defaults to `"default"`.
#' @export
#' @rdname inputApp
inputAppServer <- function(id, simres = shiny::reactiveVal(NULL), datafile = shiny::reactiveVal(""), ecosystem = "default") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamically update select choices based on folder / simulation / ecosystem contents
    shiny::observe({
      sim <- if (is.reactive(simres)) simres() else simres
      dfile <- if (is.reactive(datafile)) datafile() else datafile
      eco <- if (is.reactive(ecosystem)) ecosystem() else ecosystem
      if (!is.null(sim) && !is.null(sim$ecosystem) && is.character(sim$ecosystem) && nzchar(sim$ecosystem)) {
        eco <- sim$ecosystem
      }
      
      discovered <- discover_dataset_tables(dfile, sim, ecosystem = eco)
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
        eco <- if (is.reactive(ecosystem)) ecosystem() else ecosystem
        if (!is.null(sim) && !is.null(sim$ecosystem) && is.character(sim$ecosystem) && nzchar(sim$ecosystem)) {
          eco <- sim$ecosystem
        }

        # Resolve ecosystem directory if dfile is not given
        target_eco_dir <- ""
        if (is.character(eco) && length(eco) == 1 && nzchar(eco)) {
          if (dir.exists(eco)) {
            target_eco_dir <- eco
          } else {
            pkg_dir <- system.file(file.path("extdata", eco), package = "ewing")
            if (pkg_dir == "" || !dir.exists(pkg_dir)) {
              pkg_dir <- file.path("inst", "extdata", eco)
            }
            if (dir.exists(pkg_dir)) target_eco_dir <- pkg_dir
          }
        }

        effective_dfile <- if (is.character(dfile) && nzchar(dfile)) dfile else target_eco_dir

        res <- NULL

        # 0. Check direct file in effective datafile directory
        if (is.character(effective_dfile) && effective_dfile != "" && dir.exists(effective_dfile)) {
          txt_path <- file.path(effective_dfile, paste0(name, ".txt"))
          if (file.exists(txt_path)) res <- tryCatch(utils::read.table(txt_path, header = TRUE, fill = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
          if (is.null(res)) {
            csv_path <- file.path(effective_dfile, paste0(name, ".csv"))
            if (file.exists(csv_path)) res <- tryCatch(utils::read.csv(csv_path, fill = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
          }
          if (is.null(res)) {
            tsv_path <- file.path(effective_dfile, paste0(name, ".tsv"))
            if (file.exists(tsv_path)) res <- tryCatch(utils::read.table(tsv_path, header = TRUE, fill = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
          }
        }

        # 1. Check if dataset is stored in sim$datasets or sim$community$datasets
        if (is.null(res) && !is.null(sim)) {
          if (!is.null(sim$datasets) && !is.null(sim$datasets[[name]])) {
            res <- sim$datasets[[name]]
          } else if (!is.null(sim$community) && !is.null(sim$community$datasets) && !is.null(sim$community$datasets[[name]])) {
            res <- sim$community$datasets[[name]]
          }
        }

        # 2. Extract dynamically via getOrgDataSimple
        if (is.null(res)) {
          sim_single <- if (!is.null(sim) && inherits(sim, "ewing_discrete")) sim[[1]] else sim
          res <- tryCatch({
            getOrgDataSimple(sim_single, name, datafile = effective_dfile)
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

        # 4. Fallback to reading from default package directory
        if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
          pkg_dir <- system.file("extdata/default", package = "ewing")
          if (pkg_dir == "" || !dir.exists(pkg_dir)) pkg_dir <- file.path("inst", "extdata", "default")
          if (dir.exists(pkg_dir)) {
            txt_path <- file.path(pkg_dir, paste0(name, ".txt"))
            if (file.exists(txt_path)) res <- tryCatch(utils::read.table(txt_path, header = TRUE, fill = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
          }
        }

        # Ensure rownames are displayed as a column if present
        if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
          if (!identical(rownames(res), as.character(seq_len(nrow(res)))) && !("rownames" %in% names(res))) {
            res <- data.frame(rownames = rownames(res), res, check.names = FALSE, stringsAsFactors = FALSE)
          }
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

#' Input System Selection Module
#'
#' Shiny UI module to select from available ecosystem systems.
#'
#' @param id Module ID string
#' @param choices Optional vector of system choices. Defaults to `available_ecosystems()`.
#' @param selected Initial selected system. Defaults to `"default"`.
#' @export
#' @rdname inputApp
inputSystemInput <- function(id, choices = NULL, selected = "default") {
  ns <- shiny::NS(id)
  sys_choices <- if (!is.null(choices)) choices else available_ecosystems()
  if (!selected %in% sys_choices && length(sys_choices) > 0) {
    selected <- sys_choices[1]
  }
  shiny::tagList(
    shiny::selectInput(ns("ecosystem"), "Select System / Ecosystem:",
      choices = sys_choices,
      selected = selected
    )
  )
}

#' Input System Server Module
#'
#' Server module returning a reactive for the selected system.
#'
#' @param id Module ID string
#' @return Reactive expression returning the selected system name or directory.
#' @export
#' @rdname inputApp
inputSystemServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      input$ecosystem %||% "default"
    })
  })
}

#' Input System App
#'
#' Interactive Shiny application to explore datasets across different systems/ecosystems.
#'
#' @param title Application title
#' @param selected Initial system selection. Defaults to `"default"`.
#' @export
#' @rdname inputApp
inputSystem <- function(title = "System Data Explorer", selected = "default") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      inputSystemInput("sys_select", selected = selected),
      shiny::hr(),
      inputAppInput("sys_data")
    ),
    bslib::card(
      inputAppOutput("sys_data")
    )
  )
  server <- function(input, output, session) {
    eco <- inputSystemServer("sys_select")
    inputAppServer("sys_data", ecosystem = eco)
  }
  shiny::shinyApp(ui = ui, server = server)
}

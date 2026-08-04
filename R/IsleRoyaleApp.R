#' Isle Royale Wolf-Moose Simulation App
#'
#' Interactive platform for exploring Isle Royale wolf-moose spatial predator-prey
#' dynamics over real-world island geography, habitat suitability features,
#' life stage age classes, and historical 1980-2019 census benchmarks.
#'
#' @param title Application title string
#' @export
#' @importFrom utils read.csv read.table
#' @importFrom shiny NS tagList h4 p selectInput sliderInput checkboxInput actionButton br HTML reactiveVal reactive observeEvent renderPlot renderTable req moduleServer updateSliderInput renderUI
#' @importFrom bslib page_sidebar sidebar
#' @importFrom sf st_transform st_bbox st_as_sf
#' @rdname IsleRoyaleApp
IsleRoyaleApp <- function(title = "Isle Royale Wolf-Moose Simulation Platform") {
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      width = 340,
      IsleRoyaleInput("isleroyale")
    ),
    IsleRoyaleOutput("isleroyale")
  )

  server <- function(input, output, session) {
    IsleRoyaleServer("isleroyale")
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Isle Royale Input Controls Module
#' @param id Module ID
#' @export
#' @rdname IsleRoyaleApp
IsleRoyaleInput <- function(id) {
  ns <- shiny::NS(id)

  # Fetch available years from wolf_moose.csv if available
  csv_path <- system.file("doc/isle_royale/wolf_moose.csv", package = "ewing")
  if (csv_path == "" || !file.exists(csv_path)) csv_path <- "inst/doc/isle_royale/wolf_moose.csv"

  years <- 1980:2019
  if (file.exists(csv_path)) {
    df <- tryCatch(utils::read.csv(csv_path), error = function(e) NULL)
    if (!is.null(df) && "Year" %in% names(df)) years <- df$Year
  }

  shiny::tagList(
    shiny::tags$style(shiny::HTML(sprintf("
      #%s .form-group { margin-bottom: 4px; }
      #%s .irs { margin-bottom: 0px; }
      #%s .irs-with-grid { height: 34px; }
      #%s .checkbox { margin-top: 1px; margin-bottom: 2px; }
      #%s .radio-inline, #%s .checkbox-inline { margin-right: 10px; }
      #%s label { font-size: 0.82rem; margin-bottom: 1px; }
      #%s div.shiny-options-group { margin-top: -2px; }
    ", id, id, id, id, id, id, id, id))),

    # 1. Top Section: Step size & Action Buttons (Run / Reset on same line)
    step_size_slider(ns("step_size"), "Steps per click:", selected = 200),
    shiny::div(
      style = "display: flex; gap: 8px; margin: 4px 0 6px 0;",
      shiny::actionButton(ns("step_sim"), "Run", class = "btn-primary", style = "flex: 1; font-weight: 600; padding: 3px 6px; font-size: 0.85rem;"),
      shiny::actionButton(ns("reset_sim"), "Reset", class = "btn-warning", style = "flex: 1; font-weight: 600; padding: 3px 6px; font-size: 0.85rem;")
    ),

    # 2. Population & Hexagon Extent Sliders
    shiny::sliderInput(ns("n_moose"), "Initial Moose:", min = 50, max = 3000, value = 664, step = 50),
    shiny::sliderInput(ns("n_wolves"), "Initial Wolves:", min = 0, max = 60, value = 50, step = 2),
    shiny::sliderInput(ns("hex_diameter"), "Hex Diameter:", min = 0.005, max = 0.03, value = 0.01, step = 0.001),

    # 3. Conditional Map Overlay Options (Substrate Plot & Census Benchmarks)
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'Substrate Plot' || input['%s'] == 'Census Benchmarks'", ns("tabset"), ns("tabset")),
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 4px 0 2px 0;"),
      shiny::div(
        style = "display: flex; gap: 12px; align-items: center; margin-bottom: 2px;",
        shiny::checkboxInput(ns("show_habitat"), "Moose Habitat", value = TRUE),
        shiny::checkboxInput(ns("show_landmarks"), "Landmarks", value = TRUE)
      )
    ),

    # 4. Conditional Substrate Plot Axis Units
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'Substrate Plot'", ns("tabset")),
      axisUnitInput(ns("substrate_axis"), time_label = "Days")
    ),

    # 5. Conditional Age Classes Options (Age Classes tab)
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'Age Classes'", ns("tabset")),
      shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 4px 0 2px 0;"),
      ageClassControlInput(ns("age_ctrls"), time_label = "Days")
    ),

    shiny::div(style = "border-top: 1px solid rgba(0,0,0,0.1); margin: 4px 0 2px 0;"),

    # 6. Bottom Section: Baseline Year & Status
    shiny::selectInput(ns("start_year"), "Baseline Year:", choices = years, selected = 1980),
    shiny::uiOutput(ns("status"))
  )
}

#' Isle Royale Output Display Module
#' @param id Module ID
#' @export
#' @rdname IsleRoyaleApp
IsleRoyaleOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabsetPanel(
      id = ns("tabset"),
      type = "tabs",
      shiny::tabPanel(
        "Substrate Plot",
        shiny::plotOutput(ns("substrate_plot"), height = "650px")
      ),
      shiny::tabPanel(
        "Age Classes",
        distPlotOutput(ns("dist_plot"))
      ),
      shiny::tabPanel(
        "Census Benchmarks",
        shiny::plotOutput(ns("autoplot"), height = "650px")
      ),
      shiny::tabPanel(
        "Live Demographics",
        shiny::br(),
        shiny::h4("Live Organism Population Summary (Updates Live on Simulation Step)"),
        shiny::tableOutput(ns("summary_table"))
      ),
      shiny::tabPanel(
        "Input Data",
        shiny::br(),
        inputAppInput(ns("input_data")),
        inputAppOutput(ns("input_data"))
      )
    )
  )
}

#' Isle Royale Server Logic Module
#' @param id Module ID
#' @export
#' @rdname IsleRoyaleApp
IsleRoyaleServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    status_msg <- shiny::reactiveVal("")
    sim_state <- shiny::reactiveVal(NULL)
    sub_x_var <- axisUnitServer("substrate_axis")
    age_ctrls <- ageClassControlServer("age_ctrls")

    # Auto-update initial population slider defaults when baseline year changes
    shiny::observeEvent(input$start_year, {
      yr <- as.numeric(input$start_year)
      csv_path <- system.file("doc/isle_royale/wolf_moose.csv", package = "ewing")
      if (csv_path == "" || !file.exists(csv_path)) csv_path <- "inst/doc/isle_royale/wolf_moose.csv"

      if (file.exists(csv_path)) {
        df <- tryCatch(utils::read.csv(csv_path), error = function(e) NULL)
        if (!is.null(df) && "Year" %in% names(df) && yr %in% df$Year) {
          row_match <- df[df$Year == yr, ]
          shiny::updateSliderInput(session, "n_moose", value = as.numeric(row_match$Moose[1]))
          shiny::updateSliderInput(session, "n_wolves", value = as.numeric(row_match$Wolves[1]))
        }
      }
    })

    # Initialize / Reset Simulation with Default Initial Steps (Matching sysetholApp behavior)
    shiny::observeEvent(list(input$reset_sim, input$start_year),
      {
        yr <- as.numeric(input$start_year)
        nm <- input$n_moose
        nw <- input$n_wolves
        hd <- input$hex_diameter

        shiny::req(yr, nm, nw, hd)

        sim <- init_isle_royale_sim(
          year = yr,
          n_moose = nm,
          n_wolves = nw,
          hex_diameter = hd
        )

        # Run initial default simulation steps (matching sysetholApp.R)
        steps <- parse_step_size(input$step_size)
        if (is.null(steps) || steps <= 0) steps <- 200
        sim <- run_isle_royale_sim(sim, nstep = steps)

        sim_state(sim)
        status_msg(paste0("<div style='color:green;'><b>Simulation Initialized:</b> Year ", yr, " with ", nm, " Moose and ", nw, " Wolves (Executed ", steps, " initial steps).</div>"))
      },
      ignoreNULL = FALSE
    )

    # Step Simulation Execution
    shiny::observeEvent(input$step_sim, {
      sim <- sim_state()
      shiny::req(sim)
      steps <- parse_step_size(input$step_size)
      if (is.null(steps) || steps <= 0) steps <- 200

      updated_sim <- run_isle_royale_sim(sim, nstep = steps)
      sim_state(updated_sim)
      status_msg(paste0("<div style='color:blue;'><b>Executed ", steps, " steps:</b> Total Steps = ", updated_sim$nstep, "</div>"))
    })

    # Render Offline Isle Royale Substrate Plot (0 API calls!)
    output$substrate_plot <- shiny::renderPlot({
      sim <- sim_state()
      shiny::req(sim)
      ewing_substrate(sim, x_var = sub_x_var())
    })

    # Compose Dist Plot Module for Age Classes
    distPlotServer("dist_plot", simres = sim_state, x_var = age_ctrls$x_var, total = age_ctrls$total, norm = age_ctrls$norm)

    # Render ggplot autoplot (Dual-panel benchmark plot)
    output$autoplot <- shiny::renderPlot({
      sim <- sim_state()
      shiny::req(sim)
      ggplot_isle_royale_sim(sim)
    })

    # Render Summary Table
    output$summary_table <- shiny::renderTable({
      sim <- sim_state()
      shiny::req(sim)

      moose_counts <- table(sim$moose_pop$ageclass)
      wolf_counts <- table(sim$wolf_pop$ageclass)

      df_moose <- data.frame(
        Species = "Moose",
        AgeClass = names(moose_counts),
        Count = as.numeric(moose_counts),
        stringsAsFactors = FALSE
      )

      df_wolf <- data.frame(
        Species = "Wolf",
        AgeClass = names(wolf_counts),
        Count = as.numeric(wolf_counts),
        stringsAsFactors = FALSE
      )

      rbind(df_moose, df_wolf)
    })

    # Compose Input Data App Module for Input Table Inspection
    pkg_dir <- system.file("extdata/isle_royale", package = "ewing")
    if (pkg_dir == "" || !dir.exists(pkg_dir)) pkg_dir <- "inst/extdata/isle_royale"
    inputAppServer("input_data", simres = sim_state, datafile = shiny::reactiveVal(pkg_dir))

    # Output status message
    output$status <- shiny::renderUI({
      shiny::HTML(status_msg())
    })
  })
}

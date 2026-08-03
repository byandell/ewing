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
    shiny::h4("Isle Royale Controls", style = "font-size: 1.0rem; font-weight: 600;"),
    shiny::selectInput(ns("start_year"), "Historical Baseline Year:", choices = years, selected = 1980),
    shiny::sliderInput(ns("n_moose"), "Initial Moose Count:", min = 50, max = 3000, value = 664, step = 50),
    shiny::sliderInput(ns("n_wolves"), "Initial Wolf Count:", min = 0, max = 60, value = 50, step = 2),
    shiny::sliderInput(ns("hex_diameter"), "Hexagon Extent Diameter (Degrees):", min = 0.005, max = 0.03, value = 0.01, step = 0.001),
    shiny::checkboxInput(ns("show_habitat"), "Overlay Moose Habitat Features", value = TRUE),
    shiny::checkboxInput(ns("show_landmarks"), "Show Moose Sighting Landmarks", value = TRUE),
    shiny::HTML("<hr style='margin: 10px 0;'/>"),
    shiny::actionButton(ns("reset_sim"), "Reset Simulation", class = "btn-warning", style = "margin-bottom: 6px; width: 100%;"),
    shiny::sliderInput(ns("step_size"), "Steps per click:", min = 50, max = 1000, value = 200, step = 50),
    shiny::actionButton(ns("step_sim"), "Run Simulation Steps", class = "btn-primary", style = "width: 100%;"),
    shiny::br(), shiny::br(),
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
      type = "tabs",
      shiny::tabPanel(
        "Substrate Plot",
        shiny::plotOutput(ns("substrate_plot"), height = "650px")
      ),
      shiny::tabPanel(
        "Age Distributions",
        shiny::plotOutput(ns("dist_plot"), height = "500px")
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
        shiny::selectInput(ns("input_table_name"), "Select Input Data Table:",
                           choices = c(
                             "organism.features",
                             "future.moose",
                             "future.wolf",
                             "moose.wolf",
                             "substrate.moose",
                             "substrate.wolf",
                             "wolf_moose (census)"
                           ),
                           selected = "organism.features"),
        shiny::tableOutput(ns("input_data_table"))
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
    
    # Initialize / Reset Simulation
    shiny::observeEvent(list(input$reset_sim, input$start_year), {
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
      sim_state(sim)
      status_msg(paste0("<div style='color:green;'><b>Simulation Initialized:</b> Year ", yr, " with ", nm, " Moose and ", nw, " Wolves.</div>"))
    }, ignoreNULL = FALSE)
    
    # Step Simulation Execution
    shiny::observeEvent(input$step_sim, {
      sim <- sim_state()
      shiny::req(sim)
      steps <- input$step_size
      if (is.null(steps) || steps <= 0) steps <- 100
      
      updated_sim <- run_isle_royale_sim(sim, nstep = steps)
      sim_state(updated_sim)
      status_msg(paste0("<div style='color:blue;'><b>Executed ", steps, " steps:</b> Total Steps = ", updated_sim$nstep, "</div>"))
    })
    
    # Render Offline Isle Royale Substrate Plot (0 API calls!)
    output$substrate_plot <- shiny::renderPlot({
      sim <- sim_state()
      shiny::req(sim)
      ewing_substrate(sim)
    })
    
    # Render Age-Class Distribution Simulation Dynamics Plot ("Dist Plot")
    output$dist_plot <- shiny::renderPlot({
      sim <- sim_state()
      shiny::req(sim)
      age_obj <- ewing_ageclass(sim, normalize = FALSE)
      if (!is.null(age_obj)) {
        ggplot2::autoplot(age_obj)
      } else {
        ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle("No simulation steps recorded yet")
      }
    })
    
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
      wolf_counts  <- table(sim$wolf_pop$ageclass)
      
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
    
    # Render Input Data Table
    output$input_data_table <- shiny::renderTable({
      name <- input$input_table_name
      if (is.null(name) || name == "") name <- "organism.features"
      
      pkg_dir <- system.file("extdata/isle_royale", package = "ewing")
      if (pkg_dir == "" || !dir.exists(pkg_dir)) pkg_dir <- "inst/extdata/isle_royale"
      
      if (name == "wolf_moose (census)") {
        csv_path <- system.file("doc/isle_royale/wolf_moose.csv", package = "ewing")
        if (csv_path == "" || !file.exists(csv_path)) csv_path <- "inst/doc/isle_royale/wolf_moose.csv"
        if (file.exists(csv_path)) return(utils::read.csv(csv_path))
      } else {
        txt_path <- file.path(pkg_dir, paste0(name, ".txt"))
        if (file.exists(txt_path)) return(utils::read.table(txt_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE))
      }
      
      data.frame(Info = paste("Table", name, "not found."))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Output status message
    output$status <- shiny::renderUI({
      shiny::HTML(status_msg())
    })
  })
}

#' Watershed App
#' 
#' Real-time USGS topological projection interfaces capable of drawing geometric features statically
#' onto active coordinate matrices dynamically. 
#' 
#' @param title Application title
#' @param id module ID string
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel moduleServer NS reactive renderPlot req tagList textInput uiOutput plotOutput actionButton h4 bindEvent shinyApp
#' @importFrom ggplot2 autoplot
watershedApp <- function(title = "Ewing Watershed Projection") {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        watershedInput("watershed")
      ),
      shiny::mainPanel(
        watershedOutput("watershed")
      )
    )
  )
  
  server <- function(input, output, session) {
    watershedServer("watershed")
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

#' @export
#' @rdname watershedApp
watershedServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    status_msg <- shiny::reactiveVal("")
    
    # Load dynamic dictionary
    csv_path <- system.file("extdata/watershed/huc_features.csv", package = "ewing")
    if (csv_path == "") csv_path <- "inst/extdata/watershed/huc_features.csv"
    
    feature_dict <- shiny::reactive({
      if (file.exists(csv_path)) {
        utils::read.csv(csv_path, colClasses = "character", stringsAsFactors = FALSE)
      } else {
        data.frame(huc12_id = character(), feature_name = character())
      }
    })
    
    # Render the dynamic feature selector
    output$feature_selector <- shiny::renderUI({
      dict <- feature_dict()
      huc <- input$huc12_id
      
      # Extract any known geographic landmarks mapping to the entered HUC12
      valid_features <- if (!is.null(huc)) dict$feature_name[dict$huc12_id == huc] else character(0)
      
      if (length(valid_features) > 0) {
        # Combine a dropdown box populated with known GIS entities, while preserving custom text entry
        shiny::selectizeInput(ns("feature_name"), "Geographic Feature Name:", 
                              choices = c("", valid_features), 
                              selected = valid_features[1], 
                              options = list(create = TRUE))
      } else {
        # Fallback to standard text string input
        shiny::textInput(ns("feature_name"), "Geographic Feature Name (Optional):", value = "")
      }
    })
    
    # Throttle rapid keystrokes to prevent USGS API spam
    throttled_huc_id <- shiny::reactive(input$huc12_id) |> shiny::debounce(1500)
    
    # Base geography cache reactive fetching ONLY when HUC ID legally stabilizes
    base_huc <- shiny::reactive({
      huc_req <- throttled_huc_id()
      shiny::req(huc_req)
      res <- NULL
      shiny::withProgress(message = 'Fetching USGS Base Boundary...', value = 0.3, {
        res <- nhdplusTools::get_huc(id = huc_req, type = "huc12")
      })
      return(res)
    })
    
    # Reactive to fetch the geography, bounded to the update button to prevent API polling spam
    huc_info <- shiny::reactive({
      shiny::req(input$huc12_id)
      status_msg("") # reset on fresh run
      
      feat <- input$feature_name
      
      # UI Race Condition Fix: If the frontend feature_selector hasn't fully mounted on initial startup, 
      # preload the intended default from the dictionary natively so it maps correctly!
      if (is.null(feat)) {
        dict <- feature_dict()
        valid_features <- if (!is.null(input$huc12_id)) dict$feature_name[dict$huc12_id == input$huc12_id] else character(0)
        if (length(valid_features) > 0) {
          feat <- valid_features[1]
        }
      }
      
      # Treat empty strings or 0-length lists as NULL for the feature_name lookup
      if (length(feat) == 0 || trimws(feat[1]) == "") {
        feat <- NULL
      }
      
      shiny::withProgress(message = 'Applying Topologies...', value = 0.5, {
        res <- NULL
        tryCatch({
          withCallingHandlers({
            # The base_huc() natively returns instantly from cache UNLESS the actual text string changed!
            res <- get_watershed(input$huc12_id, feature_name = feat, huc_layer = base_huc())
          }, warning = function(w) {
            # Catch warnings and display in orange
            status_msg(paste0(status_msg(), "<br><span style='color:orange;'><b>Warning:</b> ", w$message, "</span>"))
            invokeRestart("muffleWarning")
          })
        }, error = function(e) {
          # Catch hard errors and display in red
          status_msg(paste0(status_msg(), "<br><span style='color:red;'><b>Error:</b> ", e$message, "</span>"))
        })
        res
      })
    }) |> shiny::bindEvent(input$update, ignoreNULL = FALSE)
    
    # Reactive to construct the overlay metrics
    hex_obj <- shiny::reactive({
      huc <- huc_info()
      shiny::req(huc)
      
      # Inject the live slider bounds dynamically
      val <- input$hex_diameter
      if (is.null(val)) val <- 0.01
      
      add_watershed_hex_overlay(huc, hex_diameter = val)
    })
    
    # Render the S3 hex topology
    output$plot <- shiny::renderPlot({
      shiny::req(hex_obj())
      ggplot2::autoplot(hex_obj())
    })
    
    # Output status messages to UI
    output$status <- shiny::renderUI({
      shiny::HTML(status_msg())
    })
  })
}

#' Watershed Input
#' @export
#' @rdname watershedApp
watershedInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Geographic Settings"),
    shiny::p(shiny::HTML('Visit <a href="https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/explore?location=48.022258%2C-88.833410%2C9" target="_blank">HUC12 Map</a> to find ID and optional feature name.')),
    shiny::textInput(ns("huc12_id"), "HUC12 ID:", value = "041800000101"),
    shiny::uiOutput(ns("feature_selector")),
    shiny::sliderInput(ns("hex_diameter"), "Hexagon Extent Diameter (Degrees):", 
                       min = 0.001, max = 0.05, value = 0.01, step = 0.001),
    shiny::actionButton(ns("update"), "Generate Topology"),
    shiny::br(),
    shiny::uiOutput(ns("status"))
  )
}

#' Watershed Output
#' @export
#' @rdname watershedApp
watershedOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("plot"), height = "600px")
  )
}

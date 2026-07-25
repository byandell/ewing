#' Interactive Hexagonal Watershed Mapping UI (Input)
#'
#' UI controls for the hexagonal watershed mapping module. Allows geographic feature
#' search, HUC12 identification, feature boundary clipping, and hexagon scaling.
#'
#' @param id Module ID
#' @export
#' @importFrom shiny NS tagList h4 p textInput uiOutput sliderInput actionButton br HTML selectizeInput
#' @rdname hexmapApp
hexmapInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Interactive Hexagonal Watershed Controls"),
    shiny::p(shiny::HTML('Search or click the Leaflet map to identify a USGS Watershed (HUC12), or enter details manually below.')),
    shiny::textInput(ns("huc12_id"), "HUC12 ID:", value = "041800000101"),
    shiny::uiOutput(ns("feature_selector")),
    shiny::sliderInput(ns("hex_diameter"), "Hexagon Extent Diameter (Degrees):", 
                       min = 0.001, max = 0.05, value = 0.01, step = 0.001),
    shiny::actionButton(ns("update"), "Generate Hex Topology", class = "btn-primary"),
    shiny::br(), shiny::br(),
    shiny::uiOutput(ns("status"))
  )
}

#' Interactive Hexagonal Watershed Mapping UI (Output)
#'
#' Main visual output panel presenting interactive Leaflet map renderings (via `leafletInput` module composition)
#' and static `ggplot2` autoplots.
#'
#' @param id Module ID
#' @export
#' @importFrom shiny NS tagList tabsetPanel tabPanel plotOutput
#' @rdname hexmapApp
hexmapOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Interactive Leaflet Grid",
        leafletInput(ns("map"))
      ),
      shiny::tabPanel(
        "Static Autoplot (ggplot2)",
        shiny::plotOutput(ns("autoplot"), height = "600px")
      )
    )
  )
}

#' Interactive Hexagonal Watershed Mapping Server Logic
#'
#' Server logic utilizing Shiny module composition by calling `leafletServer("map")` directly
#' to handle interactive map discovery and HUC identification.
#'
#' @param id Module ID
#' @export
#' @importFrom shiny moduleServer reactiveVal reactive renderUI observeEvent updateTextInput req withProgress bindEvent renderPlot HTML
#' @importFrom leaflet leafletProxy clearShapes fitBounds
#' @importFrom ggplot2 autoplot
#' @importFrom sf st_bbox st_transform
#' @importFrom utils read.csv
#' @importFrom nhdplusTools get_huc
#' @rdname hexmapApp
hexmapServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    status_msg <- shiny::reactiveVal("")
    
    # Module Composition: Delegate map discovery to leafletServer module
    leaflet_mod <- leafletServer("map")
    
    # Load dynamic landmark dictionary
    csv_path <- system.file("extdata/watershed/huc_features.csv", package = "ewing")
    if (csv_path == "") csv_path <- "inst/extdata/watershed/huc_features.csv"
    
    feature_dict <- shiny::reactive({
      if (file.exists(csv_path)) {
        utils::read.csv(csv_path, colClasses = "character", stringsAsFactors = FALSE)
      } else {
        data.frame(huc12_id = character(), feature_name = character())
      }
    })
    
    # Render dynamic feature selector
    output$feature_selector <- shiny::renderUI({
      dict <- feature_dict()
      huc <- input$huc12_id
      
      valid_features <- if (!is.null(huc)) dict$feature_name[dict$huc12_id == huc] else character(0)
      
      if (length(valid_features) > 0) {
        shiny::selectizeInput(ns("feature_name"), "Geographic Feature Name (Optional):", 
                              choices = c("", valid_features), 
                              selected = valid_features[1], 
                              options = list(create = TRUE))
      } else {
        shiny::textInput(ns("feature_name"), "Geographic Feature Name (Optional):", 
                         value = ifelse(huc == "041800000101", "Isle Royale", ""))
      }
    })
    
    # Sync HUC ID when user clicks map in the composed leaflet module
    shiny::observeEvent(leaflet_mod$huc(), {
      huc_sf <- leaflet_mod$huc()
      if (!is.null(huc_sf) && nrow(huc_sf) > 0) {
        huc_id <- huc_sf$huc12[1]
        huc_name <- huc_sf$name[1]
        shiny::updateTextInput(session, "huc12_id", value = huc_id)
        status_msg(paste0("<div style='color:green;'><b>Identified HUC12 from Map:</b> ", huc_id, " (", huc_name, ")</div>"))
      }
    })
    
    # Debounce HUC ID changes to avoid excessive network calls
    throttled_huc_id <- shiny::reactive(input$huc12_id) |> shiny::debounce(1500)
    
    # Base geography cache reactive fetching ONLY when HUC ID legally stabilizes
    base_huc <- shiny::reactive({
      huc_req <- throttled_huc_id()
      shiny::req(huc_req)
      res <- NULL
      shiny::withProgress(message = 'Fetching USGS Base Boundary...', value = 0.3, {
        res <- tryCatch({
          nhdplusTools::get_huc(id = huc_req, type = "huc12")
        }, error = function(e) NULL)
      })
      return(res)
    })
    
    # Reactive pipeline to process watershed boundaries and feature restrictions
    huc_info <- shiny::reactive({
      shiny::req(input$huc12_id)
      status_msg("")
      
      feat <- input$feature_name
      
      # Handle initial UI mount race condition
      if (is.null(feat)) {
        dict <- feature_dict()
        valid_features <- if (!is.null(input$huc12_id)) dict$feature_name[dict$huc12_id == input$huc12_id] else character(0)
        if (length(valid_features) > 0) {
          feat <- valid_features[1]
        } else if (input$huc12_id == "041800000101") {
          feat <- "Isle Royale"
        }
      }
      
      if (length(feat) == 0 || trimws(feat[1]) == "") {
        feat <- NULL
      }
      
      shiny::withProgress(message = 'Applying Topologies & Feature Restrictions...', value = 0.5, {
        res <- NULL
        tryCatch({
          withCallingHandlers({
            res <- get_watershed(input$huc12_id, feature_name = feat, huc_layer = base_huc())
          }, warning = function(w) {
            status_msg(paste0(status_msg(), "<br/><span style='color:orange;'><b>Warning:</b> ", w$message, "</span>"))
            invokeRestart("muffleWarning")
          })
        }, error = function(e) {
          status_msg(paste0(status_msg(), "<br/><span style='color:red;'><b>Error:</b> ", e$message, "</span>"))
        })
        res
      })
    }) |> shiny::bindEvent(input$update, ignoreNULL = FALSE)
    
    # Reactive to construct the hexagonal overlay mesh
    hex_obj <- shiny::reactive({
      huc <- huc_info()
      shiny::req(huc)
      
      val <- input$hex_diameter
      if (is.null(val)) val <- 0.01
      
      add_watershed_hex_overlay(huc, hex_diameter = val)
    })
    
    # Update Leaflet Map Shapes on composed module map handle
    shiny::observeEvent(hex_obj(), {
      obj <- hex_obj()
      shiny::req(obj)
      
      # Target the sub-module's leafletProxy handle ('map-mapper')
      map_proxy_id <- ns("map-mapper")
      proxy <- leaflet::leafletProxy(map_proxy_id) |>
        leaflet::clearShapes() |>
        add_leaflet_hex_overlay(obj)
      
      # Zoom map to fit generated watershed bounding box
      bbox <- sf::st_bbox(sf::st_transform(obj$layer, 4326))
      proxy |> leaflet::fitBounds(
        lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])
      )
      
      n_hex <- length(obj$hex_overlay)
      feat_str <- if (!is.null(obj$feature_name) && obj$feature_name != "") paste0(" (Restricted to '", obj$feature_name, "')") else ""
      status_msg(paste0("<div style='color:green;'><b>Generated Hex Grid:</b> ", n_hex, " hex cells created for HUC ", obj$huc_id, feat_str, "</div>"))
    })
    
    # Render ggplot autoplot
    output$autoplot <- shiny::renderPlot({
      shiny::req(hex_obj())
      ggplot2::autoplot(hex_obj())
    })
    
    # Output status messages
    output$status <- shiny::renderUI({
      shiny::HTML(status_msg())
    })
  })
}

#' Run the Hexagonal Watershed Projection App
#'
#' Launches an interactive Shiny application combining Leaflet spatial feature identification
#' (via `leafletApp` module composition), USGS HUC12 subwatershed boundary lookup, feature area restriction,
#' and hexagonal substrate grid overlays.
#'
#' @param title Application title string
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel shinyApp
#' @rdname hexmapApp
hexmapApp <- function(title = "Ewing Hexagonal Watershed Projection") {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        hexmapInput("hexmap")
      ),
      shiny::mainPanel(
        hexmapOutput("hexmap")
      )
    )
  )
  
  server <- function(input, output, session) {
    hexmapServer("hexmap")
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

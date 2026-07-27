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
    shiny::p(shiny::HTML('Search, click, or outline a rubberband region on the Leaflet map, or manage selected HUCs below.')),
    shiny::uiOutput(ns("huc_selector")),
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
#' @importFrom shiny moduleServer reactiveVal reactive renderUI observeEvent updateSelectizeInput req withProgress bindEvent renderPlot HTML selectizeInput
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
    
    # Render dynamic HUC selector UI (multi-select with remove_button plugin)
    output$huc_selector <- shiny::renderUI({
      shiny::selectizeInput(
        ns("huc12_id"),
        "Selected HUC Watershed(s):",
        choices = c("041800000101" = "041800000101"),
        selected = c("041800000101"),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), create = TRUE)
      )
    })
    
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
      huc_vec <- input$huc12_id
      
      valid_features <- if (!is.null(huc_vec)) dict$feature_name[dict$huc12_id %in% huc_vec] else character(0)
      
      if (length(valid_features) > 0) {
        shiny::selectizeInput(ns("feature_name"), "Geographic Feature Name (Optional):", 
                              choices = c("", valid_features), 
                              selected = valid_features[1], 
                              options = list(create = TRUE))
      } else {
        shiny::textInput(ns("feature_name"), "Geographic Feature Name (Optional):", 
                         value = ifelse("041800000101" %in% huc_vec, "Isle Royale", ""))
      }
    })
    
    # Sync HUC selector choices & selected values when map search or polygon click occurs
    shiny::observeEvent(list(leaflet_mod$all_hucs(), leaflet_mod$included_ids()), {
      all_sf <- leaflet_mod$all_hucs()
      inc_ids <- unname(as.character(leaflet_mod$included_ids()))
      current_sel <- unname(as.character(input$huc12_id))
      if (is.null(current_sel)) current_sel <- character(0)
      
      # Only trigger updateSelectizeInput if map selection differs from current sidebar selection
      if (!is.null(all_sf) && nrow(all_sf) > 0 && !setequal(inc_ids, current_sel)) {
        huc_col <- if ("huc12" %in% names(all_sf)) "huc12" else if ("huc10" %in% names(all_sf)) "huc10" else if ("huc8" %in% names(all_sf)) "huc8" else names(all_sf)[1]
        ids <- unname(as.character(all_sf[[huc_col]]))
        names_vec <- if ("name" %in% names(all_sf)) all_sf$name else rep("", length(ids))
        
        choices_vec <- stats::setNames(ids, paste0(ids, ifelse(names_vec != "", paste0(" (", names_vec, ")"), "")))
        
        shiny::updateSelectizeInput(
          session,
          "huc12_id",
          choices = choices_vec,
          selected = inc_ids,
          server = FALSE
        )
      }
    }, ignoreNULL = TRUE)
    
    # Push sidebar selection changes back to leaflet module map shape rendering
    shiny::observeEvent(input$huc12_id, {
      selected_vec <- unname(as.character(input$huc12_id))
      if (is.null(selected_vec)) selected_vec <- character(0)
      
      current_inc <- unname(as.character(leaflet_mod$included_ids()))
      if (!setequal(selected_vec, current_inc)) {
        if (!is.null(leaflet_mod$set_included_ids)) {
          leaflet_mod$set_included_ids(selected_vec)
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # Debounce HUC selector inputs to avoid unnecessary processing
    throttled_huc_id <- shiny::reactive(input$huc12_id) |> shiny::debounce(1000)
    
    # Base geography cache reactive: Filters from in-memory sf data if available (0 API calls), otherwise fetches
    base_huc <- shiny::reactive({
      huc_vec <- throttled_huc_id()
      shiny::req(huc_vec)
      huc_vec <- huc_vec[huc_vec != ""]
      shiny::req(length(huc_vec) > 0)
      
      all_sf <- leaflet_mod$all_hucs()
      
      # Zero-API-Call Cache: If candidate HUC geometries exist in memory, filter locally!
      if (!is.null(all_sf) && nrow(all_sf) > 0) {
        huc_col <- if ("huc12" %in% names(all_sf)) "huc12" else if ("huc10" %in% names(all_sf)) "huc10" else if ("huc8" %in% names(all_sf)) "huc8" else names(all_sf)[1]
        mem_ids <- as.character(all_sf[[huc_col]])
        if (all(huc_vec %in% mem_ids)) {
          return(all_sf[mem_ids %in% huc_vec, ])
        }
      }
      
      # Fallback to network fetch only for newly typed/un-cached HUC IDs
      res <- NULL
      shiny::withProgress(message = 'Fetching USGS Base Boundary...', value = 0.3, {
        res <- tryCatch({
          nhdplusTools::get_huc(id = huc_vec, type = "huc12")
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

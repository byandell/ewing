leafletInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Interactive Map Explorer"),
    shiny::p("Search for a landmark, click a point on the map, or use the draw toolbar (top left) to outline a rubberband region."),
    leaflet::leafletOutput(ns("mapper"), height = "500px"),
    shiny::br(),
    shiny::uiOutput(ns("region_controls")),
    shiny::uiOutput(ns("huc_status"))
  )
}

#' Interactive Leaflet Mapping UI (Output)
#' @param id Module ID
#' @export
#' @rdname leafletApp
leafletOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Extra output if needed, e.g. mapping details
  )
}

#' Interactive Leaflet Mapping Server Logic
#'
#' Server logic for interactive Leaflet discovery. Returns a list of reactives
#' (`huc`, `status`, `click`, `drawn_polygon`) enabling Shiny module composition.
#'
#' @param id Module ID
#' @return A list of reactive objects: `huc` (reactiveVal holding discovered `sf` HUC polygon(s)),
#'   `status` (reactiveVal holding HTML status message), `click` (reactive holding map click details),
#'   and `drawn_polygon` (reactiveVal holding user drawn rubberband polygon sf).
#' @export
#' @importFrom leaflet renderLeaflet leafletProxy addPolygons clearShapes
#' @importFrom sf st_sfc st_polygon st_sf
#' @rdname leafletApp
leafletServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store dynamic reactive outputs
    status_msg <- shiny::reactiveVal("")
    huc_boundary <- shiny::reactiveVal(NULL)
    all_hucs_sf <- shiny::reactiveVal(NULL)
    included_huc_ids <- shiny::reactiveVal(character(0))
    drawn_polygon_sf <- shiny::reactiveVal(NULL)
    is_drawing <- shiny::reactiveVal(FALSE)
    
    output$huc_status <- shiny::renderUI({
      shiny::HTML(status_msg())
    })
    
    output$region_controls <- shiny::renderUI({
      poly <- drawn_polygon_sf()
      if (!is.null(poly)) {
        shiny::div(
          style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap; margin-bottom: 15px;",
          shiny::actionButton(ns("search_region"), "Search Watersheds in Region", class = "btn-success", icon = shiny::icon("search-location")),
          shiny::actionButton(ns("clear_region"), "Clear Region", class = "btn-secondary"),
          shiny::div(
            style = "display: flex; align-items: center; margin-top: 5px;",
            shiny::checkboxInput(ns("hide_rubberband"), "Hide Drawn Region", value = FALSE)
          )
        )
      } else {
        NULL
      }
    })
    
    # Observer for Hide Drawn Region checkbox
    shiny::observeEvent(input$hide_rubberband, {
      proxy <- leaflet::leafletProxy("mapper", session = session)
      if (isTRUE(input$hide_rubberband)) {
        proxy |> leaflet::clearGroup("Drawn Region") |> leaflet::hideGroup("Drawn Region")
      } else {
        poly <- drawn_polygon_sf()
        if (!is.null(poly)) {
          proxy |>
            leaflet::showGroup("Drawn Region") |>
            leaflet::addPolygons(
              data = sf::st_transform(poly, 4326),
              color = "#8E44AD",
              weight = 3,
              fillColor = "#9B59B6",
              fillOpacity = 0.15,
              group = "Drawn Region"
            )
        }
      }
    }, ignoreInit = FALSE)
    
    # Helper function to render HUC polygon shapes with styled included/excluded layers
    render_huc_shapes <- function(hucs, selected_ids) {
      proxy <- leaflet::leafletProxy("mapper", session = session)
      
      if (is.null(hucs) || nrow(hucs) == 0) {
        proxy |> leaflet::clearGroup("huc_polygons")
        return()
      }
      
      huc_col <- if ("huc12" %in% names(hucs)) "huc12" else if ("huc10" %in% names(hucs)) "huc10" else if ("huc8" %in% names(hucs)) "huc8" else names(hucs)[1]
      huc_type <- toupper(huc_col)
      
      hucs_4326 <- sf::st_transform(hucs, 4326)
      ids <- unname(as.character(hucs_4326[[huc_col]]))
      selected_ids <- unname(as.character(selected_ids))
      names_vec <- if ("name" %in% names(hucs_4326)) hucs_4326$name else rep("", length(ids))
      
      # Explicitly remove shapes by layerId and clear group to ensure Leaflet JS re-renders updated polygon styles
      proxy |> leaflet::removeShape(layerId = ids)
      proxy |> leaflet::clearGroup("huc_polygons")
      
      inc_mask <- ids %in% selected_ids
      
      # De-selected (excluded) watersheds: bold, high-contrast dashed crimson outline
      if (any(!inc_mask)) {
        excl_sf <- hucs_4326[!inc_mask, ]
        excl_ids <- ids[!inc_mask]
        excl_names <- names_vec[!inc_mask]
        proxy |> leaflet::addPolygons(
          data = excl_sf,
          layerId = excl_ids,
          group = "huc_polygons",
          color = "#C0392B",
          weight = 2.5,
          dashArray = "6,6",
          fillColor = "#E74C3C",
          fillOpacity = 0.12,
          popup = paste0("<b>", huc_type, ":</b> ", excl_ids, "<br/><b>Name:</b> ", excl_names, "<br/><i>(Excluded - click shape on map to include)</i>")
        )
      }
      
      # Selected (included) watersheds: solid vibrant purple outline
      if (any(inc_mask)) {
        inc_sf <- hucs_4326[inc_mask, ]
        inc_ids <- ids[inc_mask]
        inc_names <- names_vec[inc_mask]
        proxy |> leaflet::addPolygons(
          data = inc_sf,
          layerId = inc_ids,
          group = "huc_polygons",
          color = "#8E44AD",
          weight = 2.5,
          fillColor = "#9B59B6",
          fillOpacity = 0.30,
          popup = paste0("<b>", huc_type, ":</b> ", inc_ids, "<br/><b>Name:</b> ", inc_names, "<br/><i>(Included - click shape on map to exclude)</i>")
        )
      }
    }
    
    # Helper function to update included IDs, re-render shapes, and update huc_boundary
    update_included_ids <- function(new_inc) {
      all_hucs <- all_hucs_sf()
      if (is.null(all_hucs) || nrow(all_hucs) == 0) return()
      
      huc_col <- if ("huc12" %in% names(all_hucs)) "huc12" else if ("huc10" %in% names(all_hucs)) "huc10" else if ("huc8" %in% names(all_hucs)) "huc8" else names(all_hucs)[1]
      valid_ids <- unname(as.character(all_hucs[[huc_col]]))
      
      new_inc <- intersect(unname(as.character(new_inc)), valid_ids)
      included_huc_ids(new_inc)
      render_huc_shapes(all_hucs, new_inc)
      
      filtered_sf <- all_hucs[valid_ids %in% new_inc, ]
      huc_boundary(if (nrow(filtered_sf) > 0) filtered_sf else NULL)
      
      n_inc <- length(new_inc)
      n_total <- length(valid_ids)
      status_msg(paste0("<div style='color:purple;'><b>Updated Selection:</b> ", n_inc, " of ", n_total, " watersheds included.</div>"))
    }
    
    # Observer for Shape Clicks (Toggle HUC inclusion/exclusion in-memory)
    shiny::observeEvent(input$mapper_shape_click, {
      click_shape <- input$mapper_shape_click
      if (is.null(click_shape) || is.null(click_shape$id)) return()
      
      clicked_id <- unname(as.character(click_shape$id))
      all_hucs <- all_hucs_sf()
      if (is.null(all_hucs) || nrow(all_hucs) == 0) return()
      
      huc_col <- if ("huc12" %in% names(all_hucs)) "huc12" else if ("huc10" %in% names(all_hucs)) "huc10" else if ("huc8" %in% names(all_hucs)) "huc8" else names(all_hucs)[1]
      valid_ids <- unname(as.character(all_hucs[[huc_col]]))
      if (!clicked_id %in% valid_ids) return()
      
      current_inc <- unname(as.character(included_huc_ids()))
      new_inc <- if (clicked_id %in% current_inc) {
        setdiff(current_inc, clicked_id)
      } else {
        union(current_inc, clicked_id)
      }
      
      update_included_ids(new_inc)
    })
    
    # Render the initial basemap (Option A: Search and Draw toolbar included)
    output$mapper <- leaflet::renderLeaflet({
      build_base_map()
    })
    
    # Track drawing state to ignore vertex clicks while user is drawing rubberband polygon
    shiny::observeEvent(input$mapper_draw_start, {
      is_drawing(TRUE)
    })
    
    shiny::observeEvent(input$mapper_draw_stop, {
      is_drawing(FALSE)
    })
    
    # Parse GeoJSON drawn feature into sf polygon
    parse_drawn_feature <- function(feature) {
      if (is.null(feature) || is.null(feature$geometry) || is.null(feature$geometry$coordinates)) return(NULL)
      coords_raw <- feature$geometry$coordinates[[1]]
      if (is.null(coords_raw) || length(coords_raw) < 3) return(NULL)
      
      mat <- do.call(rbind, lapply(coords_raw, function(pt) c(as.numeric(pt[[1]]), as.numeric(pt[[2]]))))
      poly <- sf::st_sfc(sf::st_polygon(list(mat)), crs = 4326)
      sf::st_sf(geometry = poly)
    }
    
    # Observer for Drawn Features (Rubberband polygon)
    shiny::observeEvent(input$mapper_draw_new_feature, {
      is_drawing(FALSE)
      feature <- input$mapper_draw_new_feature
      poly_sf <- parse_drawn_feature(feature)
      if (!is.null(poly_sf)) {
        drawn_polygon_sf(poly_sf)
        status_msg("<div style='color:purple;'><b>Rubberband Region Outlined:</b> Play with boundary on map if desired, then click <b>'Search Watersheds in Region'</b>.</div>")
      }
    })
    
    shiny::observeEvent(input$mapper_draw_edited_features, {
      is_drawing(FALSE)
      features <- input$mapper_draw_edited_features$features
      if (!is.null(features) && length(features) > 0) {
        poly_sf <- parse_drawn_feature(features[[1]])
        if (!is.null(poly_sf)) {
          drawn_polygon_sf(poly_sf)
          status_msg("<div style='color:purple;'><b>Boundary Updated:</b> Click <b>'Search Watersheds in Region'</b> to discover watersheds.</div>")
        }
      }
    })
    
    shiny::observeEvent(input$mapper_draw_deleted_features, {
      is_drawing(FALSE)
      drawn_polygon_sf(NULL)
      huc_boundary(NULL)
      all_hucs_sf(NULL)
      included_huc_ids(character(0))
      leaflet::leafletProxy("mapper", session = session) |> leaflet::clearShapes()
      status_msg("<div style='color:gray;'>Drawn region cleared.</div>")
    })
    
    shiny::observeEvent(input$clear_region, {
      is_drawing(FALSE)
      drawn_polygon_sf(NULL)
      huc_boundary(NULL)
      all_hucs_sf(NULL)
      included_huc_ids(character(0))
      leaflet::leafletProxy("mapper", session = session) |> leaflet::clearShapes()
      status_msg("<div style='color:gray;'>Drawn region cleared.</div>")
    })
    
    # Trigger watershed discovery for drawn rubberband polygon region
    shiny::observeEvent(input$search_region, {
      poly <- drawn_polygon_sf()
      if (is.null(poly)) return()
      
      status_msg("<div style='color:blue;'><b>Processing:</b> Querying USGS for watersheds in region...</div>")
      shiny::withProgress(message = 'Searching Regional Watersheds...', value = 0.5, {
        hucs <- get_hucs_from_polygon(poly)
        
        if (!is.null(hucs) && nrow(hucs) > 0) {
          huc_col <- if ("huc12" %in% names(hucs)) "huc12" else if ("huc10" %in% names(hucs)) "huc10" else if ("huc8" %in% names(hucs)) "huc8" else names(hucs)[1]
          huc_type <- toupper(huc_col)
          huc_ids <- as.character(hucs[[huc_col]])
          huc_names <- if ("name" %in% names(hucs)) hucs$name else rep("", length(huc_ids))
          n_hucs <- length(huc_ids)
          
          # Cache all fetched HUC geometries and initialize all as included
          all_hucs_sf(hucs)
          included_huc_ids(huc_ids)
          render_huc_shapes(hucs, huc_ids)
          
          huc_str <- paste(paste0(huc_ids, " (", huc_names, ")"), collapse = ", ")
          status_msg(paste0("<div style='color:green;'><b>Identified ", n_hucs, " ", huc_type, " Watersheds in Region:</b><br/>", huc_str, "<br/><i>Click any polygon on map to toggle inclusion/exclusion.</i></div>"))
          
          huc_boundary(hucs)
        } else {
          all_hucs_sf(NULL)
          included_huc_ids(character(0))
          status_msg("<div style='color:orange;'><b>Warning:</b> No USGS Watershed topology found in drawn region. Try adjusting boundary.</div>")
        }
      })
    })
    
    # Observer for Single User Clicks (Point Reverse Geocoding)
    shiny::observeEvent(input$mapper_click, {
      click <- input$mapper_click
      if (is.null(click)) return()
      
      # Ignore click if user is currently drawing or already has a drawn polygon region
      if (isTRUE(is_drawing()) || !is.null(drawn_polygon_sf())) return()
      
      status_msg(paste0("<div style='color:blue;'><b>Processing:</b> Connecting to USGS at Coordinate [", 
                        round(click$lng, 4), ", ", round(click$lat, 4), "]...</div>"))
      
      # Reverse-geocode the click point to its structural USGS bounds
      shiny::withProgress(message = 'Discovering HUC Boundary...', value = 0.5, {
        huc <- get_huc_from_point(lng = click$lng, lat = click$lat)
        
        if (!is.null(huc) && nrow(huc) > 0) {
          huc_col <- if ("huc12" %in% names(huc)) "huc12" else if ("huc10" %in% names(huc)) "huc10" else if ("huc8" %in% names(huc)) "huc8" else names(huc)[1]
          huc_id <- as.character(huc[[huc_col]][1])
          huc_name <- if ("name" %in% names(huc)) huc$name[1] else ""
          status_msg(paste0("<div style='color:green;'><b>Found HUC:</b> ", huc_id, " (", huc_name, ")</div>"))
          
          all_hucs_sf(huc)
          included_huc_ids(huc_id)
          render_huc_shapes(huc, huc_id)
            
          huc_boundary(huc)
        } else {
          leaflet::leafletProxy("mapper", session = session) |> leaflet::clearGroup("huc_polygons")
          all_hucs_sf(NULL)
          included_huc_ids(character(0))
          huc_boundary(NULL)
          status_msg("<div style='color:orange;'><b>Warning:</b> No USGS Watershed topology found at this location. Ensure click is within US territory.</div>")
        }
      })
    })
    
    # Return reactives and setters for parent Shiny modules (module composition)
    return(list(
      huc = huc_boundary,
      all_hucs = all_hucs_sf,
      included_ids = included_huc_ids,
      set_included_ids = update_included_ids,
      status = status_msg,
      click = shiny::reactive(input$mapper_click),
      drawn_polygon = drawn_polygon_sf
    ))
  })
}

#' Run the Leaflet Mapping Integration App Tracker
#'
#' @export
#' @rdname leafletApp
leafletApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Ewing Spatial Interaction Discovery"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        leafletInput("mapper")
      ),
      shiny::mainPanel(
        leafletOutput("mapper")
      )
    )
  )
  server <- function(input, output, session) {
    leafletServer("mapper")
  }
  shiny::shinyApp(ui, server)
}

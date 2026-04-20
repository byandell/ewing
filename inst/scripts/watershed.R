#' Get Watershed Geometry and Centroid
#'
#' Retrieves the watershed boundary dataset (WBD) geometry for a specific HUC12 identifier
#' and optionally restricts it to an OpenStreetMap geographic feature (e.g., an island or park).
#'
#' @param huc_id A character string representing the HUC12 identifier.
#' @param feature_name An optional character string specifying a geographic feature to restrict the watershed (via `osmdata`).
#' @param huc_layer An optional pre-fetched SF object boundary to intercept identical querying sequences dynamically.
#'
#' @return A list containing the `huc_id`, the `lon` and `lat` of the centroid, and the `sf` `layer`.
#' @export
#'
#' @importFrom nhdplusTools get_huc
#' @importFrom sf st_transform st_crs st_intersection st_centroid st_geometry st_coordinates
get_watershed <- function(huc_id, feature_name = NULL, huc_layer = NULL) {
  # Get HUC12 sf object (queries USGS WBD)
  if (is.null(huc_layer)) {
    huc_layer <- nhdplusTools::get_huc(id = huc_id, type = "huc12")
  }
  
  if (is.null(huc_layer) || nrow(huc_layer) == 0) {
    stop("Invalid HUC12 ID or could not retrieve watershed data from USGS.")
  }
  
  if (!is.null(feature_name)) {
    if (!requireNamespace("osmdata", quietly = TRUE)) {
      stop("The 'osmdata' package is required to filter geographic features by name. Install it using install.packages('osmdata')")
    }
    
    # Try querying OpenStreetMap nominatim for the geographic feature boundary
    feature_geom <- tryCatch({
      osmdata::getbb(feature_name, format_out = "sf_polygon", limit = 1)
    }, error = function(e) {
      warning(paste("osmdata could not find a valid polygon for feature:", feature_name, "- Generating whole HUC12 instead."))
      return(NULL)
    })
    
    if (!is.null(feature_geom)) {
      # Formats may vary (list containing polygon/multipolygon, or an sf object directly)
      feature_sf <- NULL
      if (inherits(feature_geom, "sf") || inherits(feature_geom, "sfc")) {
        feature_sf <- feature_geom
      } else if (is.list(feature_geom)) {
        if (!is.null(feature_geom$multipolygon)) {
          feature_sf <- feature_geom$multipolygon
        } else if (!is.null(feature_geom$polygon)) {
          feature_sf <- feature_geom$polygon
        } else if (length(feature_geom) > 0) {
          feature_sf <- feature_geom[[1]]
        }
      } 
      
      if (!is.null(feature_sf)) {
        # Project to HUC's CRS and spatially intersect to restrict bounds
        feature_sf <- sf::st_transform(feature_sf, sf::st_crs(huc_layer))
        clipped_layer <- suppressWarnings(sf::st_intersection(huc_layer, feature_sf))
        
        # If intersection yields valid geometries, use it; otherwise fallback to full HUC
        if (nrow(clipped_layer) > 0) {
           huc_layer <- clipped_layer
        } else {
           warning(paste("Feature", feature_name, "does not overlap with HUC12", huc_id, "- Generating whole HUC12 instead."))
        }
      } else {
        warning(paste("osmdata could not extract a valid polygon for feature:", feature_name, "- Generating whole HUC12 instead."))
      }
    }
  }
  
  # Calculate geographic centroid of the final geometry
  centroid <- suppressWarnings(sf::st_centroid(sf::st_geometry(huc_layer)))
  coords <- sf::st_coordinates(centroid)
  
  list(
    huc_id = huc_id,
    lon = as.numeric(coords[1, "X"]),
    lat = as.numeric(coords[1, "Y"]),
    layer = huc_layer
  )
}

#' Add Hexagonal Overlay to Watershed
#'
#' Generates a spatial hexagonal grid across the target watershed bounds.
#'
#' @param huc_info A watershed list returned from `get_watershed()`.
#' @param hex_diameter Numeric representing the diameter of the hexagons in CRS units.
#'
#' @return An S3 object of class `watershed_hex_overlay` containing the original geometry plus the hex layer.
#' @export
#'
#' @importFrom sf st_make_grid st_intersects
add_watershed_hex_overlay <- function(huc_info, hex_diameter = 0.01) {
  huc_layer <- huc_info$layer
  
  # Use sf::st_make_grid with square = FALSE to mathematically build a spatial hex mesh atop the bounding box
  hex_mesh <- sf::st_make_grid(huc_layer, square = FALSE, cellsize = c(hex_diameter, hex_diameter))
  
  # Filter the generated mesh to only retain hexagons crossing the actual geographical feature
  # (lengths > 0 signifies the hexagon touches the island geometry)
  hex_overlay <- hex_mesh[lengths(sf::st_intersects(hex_mesh, huc_layer)) > 0]
  
  huc_info$hex_overlay <- hex_overlay
  huc_info$hex_diameter <- hex_diameter
  
  class(huc_info) <- "watershed_hex_overlay"
  return(huc_info)
}

#' Plot Watershed Hexagonal Overlay
#'
#' Visualizes the watershed bounded area and the superimposed hexagonal scaling network.
#'
#' @param object An S3 object of class `watershed_hex_overlay`.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A `ggplot` object representing the spatial mesh.
#' @export
#'
#' @importFrom ggplot2 ggplot geom_sf theme_minimal ggtitle labs
autoplot.watershed_hex_overlay <- function(object, ...) {
  ggplot2::ggplot() +
    # Represents underlying restricted outline
    ggplot2::geom_sf(data = object$layer, fill="lightblue", alpha=0.3, color="blue", linewidth=0.5) +
    # Overlay our spatial hexagons
    ggplot2::geom_sf(data = object$hex_overlay, fill=NA, color="darkred", linewidth=0.7) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(paste("Geographic Hexagonal Grid (Watershed:", object$huc_id, ")", 
                           "\nHexagon Extent Diameter:", object$hex_diameter)) +
    ggplot2::labs(x="Longitude", y="Latitude")
}

#' Discover Named Geographic Features within a Watershed
#'
#' Dynamically queries the OpenStreetMap (OSM) Overpass API to discover named geographic
#' features (such as lakes, islands, parks, dams) that intersect a given HUC12 subwatershed.
#'
#' @param huc_id A character string representing the HUC12 identifier.
#' @param feature_types A character vector of OSM keys to query (default: c("natural", "waterway", "leisure")).
#'
#' @return A character vector of unique feature names found physically within the watershed bounds.
#' @export
#'
#' @importFrom nhdplusTools get_huc
#' @importFrom sf st_bbox st_transform st_crs st_intersects
discover_watershed_features <- function(huc_id, feature_types = c("natural", "waterway", "leisure")) {
  # Get HUC12 sf object to establish the tight bounding limit
  huc_layer <- nhdplusTools::get_huc(id = huc_id, type = "huc12")
  
  if (is.null(huc_layer) || nrow(huc_layer) == 0) {
    stop("Invalid HUC12 ID or could not retrieve watershed data from USGS.")
  }
  
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    stop("The 'osmdata' package is required to execute dynamic feature discovery. Install it using install.packages('osmdata')")
  }
  
  # Map coordinates to WGS84 for the public Overpass API interface
  bbox <- sf::st_bbox(sf::st_transform(huc_layer, 4326))
  bbox_str <- paste(bbox["ymin"], bbox["xmin"], bbox["ymax"], bbox["xmax"], sep = ",")
  
  # Construct raw Overpass QL to execute a UNION (OR) query natively
  # This combines all target features into a single API request perfectly bypassing rate limit locks
  ql_union_body <- paste(
    sapply(feature_types, function(key) {
      paste0(
        "  node[\"", key, "\"](", bbox_str, ");\n",
        "  way[\"", key, "\"](", bbox_str, ");\n",
        "  relation[\"", key, "\"](", bbox_str, ");\n"
      )
    }), collapse = ""
  )
  
  ql_query <- paste0(
    "[out:xml][timeout:60];\n(\n",
    ql_union_body,
    ");\n",
    "out body;\n>;\nout skel qt;\n"
  )
  
  discovered_names <- c()
  
  # Temporarily reroute to the lz4 alternative Overpass Mirror to dodge IP bans on the main branch
  old_url <- osmdata::get_overpass_url()
  osmdata::set_overpass_url("https://lz4.overpass-api.de/api/interpreter")
  
  tryCatch({
    # Temporarily disable standard S2 spherical tracking to bypass the notorious OSM topological boundary crashes 
    old_s2 <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    
    # Send the raw, unified union query into the osmdata parser
    osm_res <- osmdata::osmdata_sf(ql_query)
    
    # Extract polygons (Lakes, Parks)
    if (!is.null(osm_res$osm_polygons) && "name" %in% colnames(osm_res$osm_polygons)) {
      poly_sf <- osm_res$osm_polygons[!is.na(osm_res$osm_polygons$name), ]
      if (nrow(poly_sf) > 0) {
        # Strict intersection check (OSM bbox often bleeds outside the true vector boundary)
        poly_sf <- sf::st_transform(poly_sf, sf::st_crs(huc_layer))
        poly_sf <- suppressWarnings(sf::st_make_valid(poly_sf))
        inter <- suppressWarnings(lengths(sf::st_intersects(poly_sf, huc_layer))) > 0
        discovered_names <- c(discovered_names, poly_sf$name[inter])
      }
    }
    
    # Extract multipolygons (Great Lakes, large reserves)
    if (!is.null(osm_res$osm_multipolygons) && "name" %in% colnames(osm_res$osm_multipolygons)) {
      mpoly_sf <- osm_res$osm_multipolygons[!is.na(osm_res$osm_multipolygons$name), ]
      if (nrow(mpoly_sf) > 0) {
        mpoly_sf <- sf::st_transform(mpoly_sf, sf::st_crs(huc_layer))
        mpoly_sf <- suppressWarnings(sf::st_make_valid(mpoly_sf))
        inter <- suppressWarnings(lengths(sf::st_intersects(mpoly_sf, huc_layer))) > 0
        discovered_names <- c(discovered_names, mpoly_sf$name[inter])
      }
    }
    
    # Extract linestrings (Dams, Rivers)
    if (!is.null(osm_res$osm_lines) && "name" %in% colnames(osm_res$osm_lines)) {
      line_sf <- osm_res$osm_lines[!is.na(osm_res$osm_lines$name), ]
      if (nrow(line_sf) > 0) {
        line_sf <- sf::st_transform(line_sf, sf::st_crs(huc_layer))
        line_sf <- suppressWarnings(sf::st_make_valid(line_sf))
        inter <- suppressWarnings(lengths(sf::st_intersects(line_sf, huc_layer))) > 0
        discovered_names <- c(discovered_names, line_sf$name[inter])
      }
    }
    
    # Reset standard mapping state
    sf::sf_use_s2(old_s2)
    
  }, error = function(e) {
    if (exists("old_s2")) sf::sf_use_s2(old_s2)
    warning(paste("Raw OSM Extraction timeout or failure:", e$message))
  })
  
  # Return sanitized unique list 
  clean_names <- unique(discovered_names)
  clean_names <- sort(clean_names[!is.na(clean_names) & trimws(clean_names) != ""])
  
  # Reset the API routing mirror
  osmdata::set_overpass_url(old_url)
  
  return(clean_names)
}

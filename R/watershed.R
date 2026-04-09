#' Get Watershed Geometry and Centroid
#'
#' Retrieves the watershed boundary dataset (WBD) geometry for a specific HUC12 identifier
#' and optionally restricts it to an OpenStreetMap geographic feature (e.g., an island or park).
#'
#' @param huc_id A character string representing the HUC12 identifier.
#' @param feature_name An optional character string specifying a geographic feature to restrict the watershed (via `osmdata`).
#'
#' @return A list containing the `huc_id`, the `lon` and `lat` of the centroid, and the `sf` `layer`.
#' @export
#'
#' @importFrom nhdplusTools get_huc
#' @importFrom sf st_transform st_crs st_intersection st_centroid st_geometry st_coordinates
get_watershed <- function(huc_id, feature_name = NULL) {
  # Get HUC12 sf object (queries USGS WBD)
  huc_layer <- nhdplusTools::get_huc(id = huc_id, type = "huc12")
  
  if (!is.null(feature_name)) {
    if (!requireNamespace("osmdata", quietly = TRUE)) {
      stop("The 'osmdata' package is required to filter geographic features by name. Install it using install.packages('osmdata')")
    }
    # Query OpenStreetMap nominatim for the geographic feature boundary
    feature_geom <- osmdata::getbb(feature_name, format_out = "sf_polygon", limit = 1)
    
    # Formats may vary (list containing polygon/multipolygon, or an sf object directly)
    if (inherits(feature_geom, "sf") || inherits(feature_geom, "sfc")) {
      feature_sf <- feature_geom
    } else if (is.list(feature_geom)) {
      if (!is.null(feature_geom$multipolygon)) {
        feature_sf <- feature_geom$multipolygon
      } else if (!is.null(feature_geom$polygon)) {
        feature_sf <- feature_geom$polygon
      } else {
        feature_sf <- feature_geom[[1]]
      }
    } else {
      stop(paste("osmdata could not find a polygon for feature:", feature_name))
    }
    
    # Project to HUC's CRS and spatially intersect to restrict bounds
    feature_sf <- sf::st_transform(feature_sf, sf::st_crs(huc_layer))
    huc_layer <- suppressWarnings(sf::st_intersection(huc_layer, feature_sf))
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

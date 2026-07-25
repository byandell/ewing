#' Interactive Leaflet Geographic Utilities
#'
#' Helper utilities for building interactive Leaflet base maps with search capabilities,
#' reverse-geocoding points to USGS HUC12 subwatershed boundaries, and rendering spatial
#' hexagonal grid overlays.
#'
#' @return `build_base_map`: A `leaflet` HTML widget object.
#' @export
#' @name leaflet
#' @rdname leaflet
#'
#' @importFrom leaflet leaflet addTiles setView
build_base_map <- function() {
  if (!requireNamespace("leaflet", quietly = TRUE) || !requireNamespace("leaflet.extras", quietly = TRUE)) {
    stop("Packages 'leaflet' and 'leaflet.extras' are required for building the interactive mapper.")
  }
  
  # Initialize the map centered near central North America
  map <- leaflet::leaflet() |>
    leaflet::addTiles(group = "OpenStreetMap")
  
  # Embed the OpenStreetMap search bar widget
  map <- leaflet.extras::addSearchOSM(
    map, 
    options = leaflet.extras::searchOptions(
      zoom = 12,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )
  
  # Default view point (Center of US)
  map <- leaflet::setView(map, lng = -98.5795, lat = 39.8283, zoom = 4)
  
  return(map)
}

#' @param lng Numeric longitude coordinate
#' @param lat Numeric latitude coordinate
#'
#' @return `get_huc_from_point`: An `sf` polygon representation of the covering HUC12.
#' @export
#' @rdname leaflet
#'
#' @importFrom sf st_sfc st_point
#' @importFrom nhdplusTools get_huc
get_huc_from_point <- function(lng, lat) {
  if (is.null(lng) || is.null(lat)) {
    return(NULL)
  }
  
  # Convert physical math to rigorous Coordinate Reference System geometry
  pt <- sf::st_sfc(sf::st_point(c(lng, lat)), crs = 4326)
  
  res <- NULL
  tryCatch({
    # Automatically reverse-geocode the coordinate into the encompassing USGS HUC shape
    res <- nhdplusTools::get_huc(AOI = pt, type = "huc12")
  }, error = function(e) {
    warning("Failed to locate USGS overlapping geometry at point coordinates: ", e$message)
  })
  
  return(res)
}

#' @param map A `leaflet` map object or `leafletProxy` handle.
#' @param hex_obj A `watershed_hex_overlay` S3 object (or a list containing `layer` and `hex_overlay` sf objects).
#' @param hex_color Stroke color for hexagonal grid cells (default: "#C0392B").
#' @param bound_color Stroke color for watershed boundary (default: "#2980B9").
#'
#' @return `add_leaflet_hex_overlay`: Updated `leaflet` map object.
#' @export
#' @rdname leaflet
#'
#' @importFrom leaflet addPolygons clearShapes
#' @importFrom sf st_transform
add_leaflet_hex_overlay <- function(map, hex_obj, hex_color = "#C0392B", bound_color = "#2980B9") {
  if (is.null(hex_obj)) return(map)
  
  # Ensure geometries are transformed to WGS84 (EPSG 4326) for leaflet
  bound_sf <- sf::st_transform(hex_obj$layer, 4326)
  hex_sf <- sf::st_transform(hex_obj$hex_overlay, 4326)
  
  # Add watershed boundary polygon
  map <- map |>
    leaflet::addPolygons(
      data = bound_sf,
      color = bound_color,
      weight = 2,
      fillColor = "#3498DB",
      fillOpacity = 0.15,
      group = "Watershed Boundary",
      popup = paste0("<b>HUC12:</b> ", hex_obj$huc_id, 
                     if (!is.null(hex_obj$feature_name) && hex_obj$feature_name != "") 
                       paste0("<br/><b>Feature:</b> ", hex_obj$feature_name) else "")
    )
  
  # Add hex grid overlay
  if (!is.null(hex_sf) && length(hex_sf) > 0) {
    map <- map |>
      leaflet::addPolygons(
        data = hex_sf,
        color = hex_color,
        weight = 1,
        fillColor = hex_color,
        fillOpacity = 0.05,
        group = "Hex Overlay"
      )
  }
  
  return(map)
}

#' Build Interactive Base Map
#'
#' Constructs a blank interactive Leaflet mapping canvas optimized for North America
#' geographic search. It embeds the `leaflet.extras::addSearchOSM()` search bar,
#' allowing text-based flying capabilities (Option A interaction paradigm).
#'
#' @return A `leaflet` HTML widget object
#' @export
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

#' Get HUC12 From Map Click Coordinate
#'
#' Automatically retrieves the underlying USGS HUC12 subwatershed boundary polygon
#' given an arbitrary Longitude and Latitude pair (usually derived from a user click).
#' This powers the Option B interaction paradigm.
#'
#' @param lng Numeric longitude coordinate
#' @param lat Numeric latitude coordinate
#'
#' @return An `sf` polygon representation of the covering HUC12.
#' @export
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

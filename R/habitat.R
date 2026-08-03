#' Moose Habitat & Substrate Overlay Utilities
#'
#' Functions for extracting geographic habitat features that attract moose
#' (inland lakes, beaver ponds, cool shaded forests, and bogs/wetlands),
#' geocoding notable sighting landmarks (Washington Creek, Ojibway Lake, Feldtmann Lake, Hidden Lake),
#' computing habitat suitability weights on hexagonal substrate grids, and visualizing overlays.
#'
#' @param watershed_obj A list object returned by `get_watershed()`.
#' @param categories Character vector of habitat feature categories to extract.
#' @param use_cache Logical; if TRUE, uses pre-fetched local feature data when available.
#'
#' @return `get_habitat_features`: An `sf` data frame of habitat features clipped to the watershed.
#' @export
#' @name habitat
#' @rdname habitat
#'
#' @importFrom sf st_transform st_crs st_intersection st_union st_geometry st_make_valid st_bbox st_as_sf st_intersects st_polygon st_sfc st_sf
get_habitat_features <- function(watershed_obj, 
                                 categories = c("lakes", "waterways", "forests", "bogs"), 
                                 use_cache = TRUE) {
  huc_layer <- watershed_obj$layer
  if (is.null(huc_layer)) {
    stop("Invalid watershed_obj: layer component is required.")
  }
  
  # Check for pre-created cached data for Isle Royale
  cache_dir <- system.file("extdata/isle_royale", package = "ewing")
  if (cache_dir == "") cache_dir <- "inst/extdata/isle_royale"
  cache_file <- file.path(cache_dir, "isle_royale_features.rds")
  
  if (use_cache && file.exists(cache_file)) {
    cached_sf <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached_sf) && inherits(cached_sf, "sf")) {
      cached_sf <- sf::st_transform(cached_sf, sf::st_crs(huc_layer))
      clipped <- suppressWarnings(sf::st_intersection(cached_sf, huc_layer))
      if (nrow(clipped) > 0) return(clipped)
    }
  }
  
  # Query OpenStreetMap if osmdata package is installed
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    warning("The 'osmdata' package is not installed. Returning fallback habitat geometries.")
    return(get_fallback_habitat_features(huc_layer))
  }
  
  bbox <- sf::st_bbox(sf::st_transform(huc_layer, 4326))
  bbox_str <- paste(bbox["ymin"], bbox["xmin"], bbox["ymax"], bbox["xmax"], sep = ",")
  
  old_url <- osmdata::get_overpass_url()
  osmdata::set_overpass_url("https://lz4.overpass-api.de/api/interpreter")
  
  features_list <- list()
  
  old_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  on.exit({
    sf::sf_use_s2(old_s2)
    osmdata::set_overpass_url(old_url)
  }, add = TRUE)
  
  # Extract Inland Lakes & Waterbodies
  if ("lakes" %in% categories) {
    ql_lakes <- paste0(
      "[out:xml][timeout:30];\n(\n",
      "  natural[\"water\"](", bbox_str, ");\n",
      "  waterway[\"riverbank\"](", bbox_str, ");\n",
      ");\nout body;\n>;\nout skel qt;\n"
    )
    res_lakes <- tryCatch(osmdata::osmdata_sf(ql_lakes), error = function(e) NULL)
    if (!is.null(res_lakes) && !is.null(res_lakes$osm_polygons) && nrow(res_lakes$osm_polygons) > 0) {
      poly <- res_lakes$osm_polygons
      poly$habitat_type <- "Lake/Pond"
      features_list$lakes <- poly[, c("habitat_type", "geometry")]
    }
  }
  
  # Extract Waterways & Beaver Ponds
  if ("waterways" %in% categories) {
    ql_water <- paste0(
      "[out:xml][timeout:30];\n(\n",
      "  waterway[\"stream\"](", bbox_str, ");\n",
      "  waterway[\"river\"](", bbox_str, ");\n",
      "  waterway[\"drain\"](", bbox_str, ");\n",
      ");\nout body;\n>;\nout skel qt;\n"
    )
    res_water <- tryCatch(osmdata::osmdata_sf(ql_water), error = function(e) NULL)
    if (!is.null(res_water) && !is.null(res_water$osm_lines) && nrow(res_water$osm_lines) > 0) {
      lines <- res_water$osm_lines
      lines$habitat_type <- "Waterway"
      features_list$waterways <- lines[, c("habitat_type", "geometry")]
    }
  }
  
  # Extract Cool Shaded Forests
  if ("forests" %in% categories) {
    ql_forest <- paste0(
      "[out:xml][timeout:30];\n(\n",
      "  landuse[\"forest\"](", bbox_str, ");\n",
      "  natural[\"wood\"](", bbox_str, ");\n",
      ");\nout body;\n>;\nout skel qt;\n"
    )
    res_forest <- tryCatch(osmdata::osmdata_sf(ql_forest), error = function(e) NULL)
    if (!is.null(res_forest) && !is.null(res_forest$osm_polygons) && nrow(res_forest$osm_polygons) > 0) {
      poly <- res_forest$osm_polygons
      poly$habitat_type <- "Forest"
      features_list$forests <- poly[, c("habitat_type", "geometry")]
    }
  }
  
  # Extract Bogs & Wetlands
  if ("bogs" %in% categories) {
    ql_bogs <- paste0(
      "[out:xml][timeout:30];\n(\n",
      "  natural[\"wetland\"](", bbox_str, ");\n",
      "  wetland[\"bog\"](", bbox_str, ");\n",
      "  wetland[\"marsh\"](", bbox_str, ");\n",
      ");\nout body;\n>;\nout skel qt;\n"
    )
    res_bogs <- tryCatch(osmdata::osmdata_sf(ql_bogs), error = function(e) NULL)
    if (!is.null(res_bogs) && !is.null(res_bogs$osm_polygons) && nrow(res_bogs$osm_polygons) > 0) {
      poly <- res_bogs$osm_polygons
      poly$habitat_type <- "Bog/Wetland"
      features_list$bogs <- poly[, c("habitat_type", "geometry")]
    }
  }
  
  if (length(features_list) == 0) {
    return(get_fallback_habitat_features(huc_layer))
  }
  
  combined <- do.call(rbind, features_list)
  combined <- sf::st_transform(combined, sf::st_crs(huc_layer))
  combined <- suppressWarnings(sf::st_make_valid(combined))
  clipped <- suppressWarnings(sf::st_intersection(combined, huc_layer))
  
  if (nrow(clipped) == 0) {
    return(get_fallback_habitat_features(huc_layer))
  }
  
  return(clipped)
}

#' Internal Fallback Builder for Isle Royale Moose Habitat Features
#' @noRd
get_fallback_habitat_features <- function(huc_layer) {
  crs_target <- sf::st_crs(huc_layer)
  
  # Landmark reference coordinates (WGS84)
  # Washington Creek Windigo (-89.146, 47.923)
  # Ojibway Lake (-88.618, 48.113)
  # Feldtmann Lake (-88.961, 47.876)
  # Hidden Lake (-88.490, 48.151)
  
  mk_poly <- function(lon, lat, dx = 0.015, dy = 0.01) {
    pts <- matrix(c(
      lon - dx, lat - dy,
      lon + dx, lat - dy,
      lon + dx, lat + dy,
      lon - dx, lat + dy,
      lon - dx, lat - dy
    ), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(pts))
  }
  
  p_ojibway <- mk_poly(-88.618, 48.113, 0.012, 0.008)
  p_feldtmann <- mk_poly(-88.961, 47.876, 0.018, 0.010)
  p_hidden <- mk_poly(-88.490, 48.151, 0.010, 0.006)
  p_wash_forest <- mk_poly(-89.146, 47.923, 0.025, 0.015)
  p_bog <- mk_poly(-88.750, 48.020, 0.020, 0.012)
  
  geom_sfc <- sf::st_sfc(p_ojibway, p_feldtmann, p_hidden, p_wash_forest, p_bog, crs = 4326)
  df <- data.frame(
    habitat_type = c("Lake/Pond", "Lake/Pond", "Lake/Pond", "Forest", "Bog/Wetland"),
    stringsAsFactors = FALSE
  )
  
  fallback_sf <- sf::st_sf(df, geometry = geom_sfc)
  fallback_sf <- sf::st_transform(fallback_sf, crs_target)
  clipped <- suppressWarnings(sf::st_intersection(fallback_sf, huc_layer))
  return(clipped)
}

#' Geocode Notable Moose Sighting Landmarks
#'
#' Retrieves key moose sighting landmarks on Isle Royale (or customizable spatial targets):
#' Washington Creek in Windigo, Ojibway Lake, Feldtmann Lake, and Hidden Lake in Tobin Harbor.
#'
#' @param watershed_obj Watershed object from `get_watershed()`.
#' @param use_cache Logical; if TRUE, uses pre-fetched local landmark definitions when available.
#'
#' @return `get_moose_landmarks`: An `sf` object containing landmark point geometries and attributes.
#' @export
#' @rdname habitat
get_moose_landmarks <- function(watershed_obj, use_cache = TRUE) {
  huc_layer <- watershed_obj$layer
  
  cache_dir <- system.file("extdata/isle_royale", package = "ewing")
  if (cache_dir == "") cache_dir <- "inst/extdata/isle_royale"
  cache_file <- file.path(cache_dir, "isle_royale_landmarks.rds")
  
  if (use_cache && file.exists(cache_file)) {
    cached_pts <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached_pts) && inherits(cached_pts, "sf")) {
      return(sf::st_transform(cached_pts, sf::st_crs(huc_layer)))
    }
  }
  
  df <- data.frame(
    name = c(
      "Washington Creek (Windigo)",
      "Ojibway Lake",
      "Feldtmann Lake",
      "Hidden Lake (Tobin Harbor)"
    ),
    location = c("Windigo", "Ojibway", "Feldtmann", "Tobin Harbor"),
    description = c(
      "Feeding area along stream & forest cover",
      "Aquatic vegetation feeding lake",
      "Major southwest inland lake habitat",
      "Aquatic plant feeding area near Tobin Harbor"
    ),
    lon = c(-89.146, -88.618, -88.961, -88.490),
    lat = c(47.923, 48.113, 47.876, 48.151),
    stringsAsFactors = FALSE
  )
  
  pts_sf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  pts_sf <- sf::st_transform(pts_sf, sf::st_crs(huc_layer))
  return(pts_sf)
}

#' Construct Moose Habitat & Substrate Overlay Object
#'
#' Intersects habitat features (lakes, waterways, forests, bogs) and sighting landmarks
#' with a hexagonal substrate grid overlay, calculating habitat suitability weights per hex cell.
#'
#' @param hex_obj An S3 object of class `watershed_hex_overlay`.
#' @param habitat_sf Optional pre-extracted `sf` data frame of habitat features.
#' @param landmarks_sf Optional pre-geocoded `sf` points of sighting landmarks.
#'
#' @return `add_habitat_hex_overlay`: An S3 object of class `habitat_hex_overlay`.
#' @export
#' @rdname habitat
add_habitat_hex_overlay <- function(hex_obj, habitat_sf = NULL, landmarks_sf = NULL) {
  if (is.null(habitat_sf)) {
    habitat_sf <- get_habitat_features(hex_obj)
  }
  if (is.null(landmarks_sf)) {
    landmarks_sf <- get_moose_landmarks(hex_obj)
  }
  
  hex_mesh <- hex_obj$hex_overlay
  
  # Calculate habitat preference score per hexagon
  # Base weight = 1.0; bonus for lake (+2.0), waterway (+1.5), forest (+1.0), bog (+1.8)
  scores <- numeric(length(hex_mesh))
  types_list <- character(length(hex_mesh))
  
  if (!is.null(habitat_sf) && nrow(habitat_sf) > 0) {
    inter <- sf::st_intersects(hex_mesh, habitat_sf)
    for (i in seq_along(inter)) {
      indices <- inter[[i]]
      if (length(indices) > 0) {
        sub_types <- habitat_sf$habitat_type[indices]
        score <- 1.0
        if ("Lake/Pond" %in% sub_types) score <- score + 2.0
        if ("Waterway" %in% sub_types) score <- score + 1.5
        if ("Bog/Wetland" %in% sub_types) score <- score + 1.8
        if ("Forest" %in% sub_types) score <- score + 1.0
        scores[i] <- score
        types_list[i] <- paste(unique(sub_types), collapse = ", ")
      } else {
        scores[i] <- 1.0
        types_list[i] <- "Upland/Open"
      }
    }
  } else {
    scores[] <- 1.0
    types_list[] <- "General"
  }
  
  hex_sf <- sf::st_sf(
    hex_id = seq_along(hex_mesh),
    habitat_score = scores,
    habitat_type = types_list,
    geometry = hex_mesh
  )
  
  res <- hex_obj
  res$habitat_sf <- habitat_sf
  res$landmarks_sf <- landmarks_sf
  res$hex_habitat_sf <- hex_sf
  class(res) <- c("habitat_hex_overlay", class(hex_obj))
  
  return(res)
}

#' Autoplot Method for Moose Habitat Hexagonal Overlay
#'
#' @param object An S3 object of class `habitat_hex_overlay`.
#' @param show_landmarks Logical; whether to draw moose sighting POIs.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A `ggplot` visualization of the Isle Royale Moose Habitat Overlay.
#' @export
#' @rdname habitat
#'
#' @importFrom ggplot2 ggplot geom_sf theme_minimal ggtitle labs scale_fill_viridis_c aes geom_sf_text
autoplot.habitat_hex_overlay <- function(object, show_landmarks = TRUE, ...) {
  p <- ggplot2::ggplot() +
    # Underlying island boundary
    ggplot2::geom_sf(data = object$layer, fill = "#eef4f8", color = "#2c3e50", linewidth = 0.8)
  
  # Render habitat layers if available
  if (!is.null(object$habitat_sf) && nrow(object$habitat_sf) > 0) {
    p <- p + ggplot2::geom_sf(
      data = object$habitat_sf, 
      ggplot2::aes(fill = .data$habitat_type), 
      alpha = 0.5, color = NA
    )
  }
  
  # Hexagonal substrate overlay colored by habitat suitability score
  if (!is.null(object$hex_habitat_sf)) {
    p <- p + ggplot2::geom_sf(
      data = object$hex_habitat_sf,
      ggplot2::aes(color = .data$habitat_score),
      fill = NA, linewidth = 0.6
    ) +
    ggplot2::scale_color_viridis_c(option = "viridis", name = "Habitat Weight")
  }
  
  # Render Moose Sighting Landmarks
  if (show_landmarks && !is.null(object$landmarks_sf) && nrow(object$landmarks_sf) > 0) {
    p <- p + 
      ggplot2::geom_sf(data = object$landmarks_sf, color = "#d35400", size = 3, shape = 18) +
      ggplot2::geom_sf_text(
        data = object$landmarks_sf, 
        ggplot2::aes(label = .data$name), 
        color = "#900c3f", size = 3, fontface = "bold", vjust = -0.7
      )
  }
  
  title_txt <- "Isle Royale Moose Habitat & Substrate Overlay Model"
  if (!is.null(object$feature_name) && object$feature_name != "") {
    title_txt <- paste0(title_txt, " (", object$feature_name, ")")
  }
  
  p +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title_txt) +
    ggplot2::labs(
      x = "Longitude", y = "Latitude",
      caption = "Habitats: Inland Lakes, Beaver Ponds/Waterways, Shaded Forests & Bogs"
    )
}

#' Add Leaflet Habitat Layers
#'
#' @param map A `leaflet` map object.
#' @param object An S3 object of class `habitat_hex_overlay`.
#'
#' @return An updated `leaflet` map.
#' @export
#' @rdname habitat
#'
#' @importFrom leaflet addPolygons addCircleMarkers addPopups
add_leaflet_habitat_overlay <- function(map, object) {
  if (is.null(map) || is.null(object)) return(map)
  
  # Render habitat suitability polygons/hexagons
  if (!is.null(object$hex_habitat_sf)) {
    hex_wgs <- sf::st_transform(object$hex_habitat_sf, 4326)
    map <- map |>
      leaflet::addPolygons(
        data = hex_wgs,
        color = "#e74c3c",
        weight = 1,
        fillOpacity = 0.15,
        popup = paste0("<b>Hex ID:</b> ", hex_wgs$hex_id, 
                       "<br/><b>Habitat Score:</b> ", hex_wgs$habitat_score,
                       "<br/><b>Habitat Types:</b> ", hex_wgs$habitat_type),
        group = "Habitat Substrate Mesh"
      )
  }
  
  # Render Moose Sighting Area Markers
  if (!is.null(object$landmarks_sf)) {
    lm_wgs <- sf::st_transform(object$landmarks_sf, 4326)
    map <- map |>
      leaflet::addCircleMarkers(
        data = lm_wgs,
        color = "#d35400",
        radius = 7,
        fillOpacity = 0.9,
        popup = paste0("<b>Moose Sighting Area:</b> ", lm_wgs$name,
                       "<br/><b>Description:</b> ", lm_wgs$description),
        group = "Moose Sighting Areas"
      )
  }
  
  return(map)
}

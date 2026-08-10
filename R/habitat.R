if (!exists("safe_st_intersects", mode = "function")) {
  safe_st_intersects <- function(x, y) {
    tryCatch(
      sf::st_intersects(x, y),
      error = function(e) {
        old_s2 <- sf::sf_use_s2(FALSE)
        on.exit(sf::sf_use_s2(old_s2), add = TRUE)
        x_val <- tryCatch(sf::st_make_valid(x), error = function(e2) x)
        y_val <- tryCatch(sf::st_make_valid(y), error = function(e2) y)
        sf::st_intersects(x_val, y_val)
      }
    )
  }
}

#' Moose Habitat & Substrate Overlay Utilities
#'
#' Functions for extracting geographic habitat features that attract moose
#' (inland lakes, beaver ponds, cool shaded forests, and bogs/wetlands),
#' geocoding notable sighting landmarks (Washington Creek, Ojibway Lake, Feldtmann Lake, Hidden Lake),
#' computing habitat suitability weights on hexagonal substrate grids, and visualizing overlays.
#'
#' @param watershed_obj A spatial object or list containing a `layer` geometry (optional).
#' @param categories Character vector of habitat feature categories to extract.
#' @param use_cache Logical; if TRUE, uses pre-fetched local feature data when available.
#' @param site Target simulation landscape/site folder name (default: `"isle_royale"`).
#'
#' @return `get_habitat_features`: An `sf` data frame of habitat features clipped to the target geometry.
#' @name habitat
#' @rdname habitat
#' @export
#'
#' @importFrom sf st_transform st_crs st_intersection st_union st_geometry st_make_valid st_bbox st_as_sf st_intersects st_polygon st_sfc st_sf st_make_grid
get_habitat_features <- function(watershed_obj = NULL, 
                                 categories = c("lakes", "waterways", "forests", "bogs"), 
                                 use_cache = TRUE,
                                 site = "isle_royale") {
  huc_layer <- if (is.null(watershed_obj)) {
    NULL
  } else if (inherits(watershed_obj, "sf") || inherits(watershed_obj, "sfc")) {
    watershed_obj
  } else if (is.list(watershed_obj) && "layer" %in% names(watershed_obj)) {
    watershed_obj$layer
  } else {
    NULL
  }
  
  if (use_cache) {
    cached_sf <- NULL
    feat_key <- paste0(site, "_features")
    feat_file <- paste0(site, "_features.rds")
    if (exists("isle_royale_datasets") && is.list(isle_royale_datasets) && !is.null(isle_royale_datasets[[feat_key]])) {
      cached_sf <- isle_royale_datasets[[feat_key]]
    } else if (exists(feat_key) && inherits(get(feat_key), "sf")) {
      cached_sf <- get(feat_key)
    } else {
      cache_file <- get_site_cache_file(feat_file, site = site)
      if (file.exists(cache_file)) {
        cached_sf <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      }
    }
    
    if (!is.null(cached_sf) && inherits(cached_sf, "sf")) {
      if (is.null(huc_layer)) return(cached_sf)
      cached_sf <- sf::st_transform(cached_sf, sf::st_crs(huc_layer))
      clipped <- suppressWarnings(sf::st_intersection(cached_sf, huc_layer))
      if (nrow(clipped) > 0) return(clipped)
      return(cached_sf)
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
    if (!is.null(res_lakes$osm_polygons) && nrow(res_lakes$osm_polygons) > 0) {
      poly <- res_lakes$osm_polygons
      poly$habitat_type <- "Lake/Pond"
      features_list$lakes <- poly[, "habitat_type"]
    }
  }
  
  # Extract Waterways & Streams
  if ("waterways" %in% categories) {
    ql_water <- paste0(
      "[out:xml][timeout:30];\n(\n",
      "  waterway[\"stream\"](", bbox_str, ");\n",
      "  waterway[\"river\"](", bbox_str, ");\n",
      "  waterway[\"drain\"](", bbox_str, ");\n",
      ");\nout body;\n>;\nout skel qt;\n"
    )
    res_water <- tryCatch(osmdata::osmdata_sf(ql_water), error = function(e) NULL)
    if (!is.null(res_water$osm_lines) && nrow(res_water$osm_lines) > 0) {
      line <- res_water$osm_lines
      line$habitat_type <- "Waterway"
      features_list$waterways <- line[, "habitat_type"]
    }
  }
  
  # Combine extracted geometries
  if (length(features_list) == 0) {
    return(get_fallback_habitat_features(huc_layer))
  }
  
  combined <- do.call(rbind, features_list)
  if (is.null(combined) || nrow(combined) == 0) {
    return(get_fallback_habitat_features(huc_layer))
  }
  
  combined <- sf::st_transform(combined, sf::st_crs(huc_layer))
  clipped <- suppressWarnings(sf::st_intersection(combined, huc_layer))
  if (nrow(clipped) > 0) return(clipped)
  return(combined)
}

#' Fallback Habitat Geometries
#'
#' @param huc_layer An `sf` polygon representation of the region.
#' @return Fallback `sf` data frame of habitat features.
#' @export
#' @rdname habitat
get_fallback_habitat_features <- function(huc_layer = NULL) {
  if (is.null(huc_layer)) {
    pts <- matrix(c(-89.17, 47.87, -88.48, 47.87, -88.48, 48.16, -89.17, 48.16, -89.17, 47.87), ncol = 2, byrow = TRUE)
    huc_layer <- sf::st_sfc(sf::st_polygon(list(pts)), crs = 4326)
  }
  
  bbox <- sf::st_bbox(huc_layer)
  xmin <- bbox["xmin"]; xmax <- bbox["xmax"]
  ymin <- bbox["ymin"]; ymax <- bbox["ymax"]
  
  l1 <- sf::st_polygon(list(matrix(c(xmin+0.1, ymin+0.1, xmin+0.15, ymin+0.1, xmin+0.15, ymin+0.15, xmin+0.1, ymin+0.15, xmin+0.1, ymin+0.1), ncol=2, byrow=TRUE)))
  l2 <- sf::st_polygon(list(matrix(c(xmax-0.2, ymax-0.1, xmax-0.1, ymax-0.1, xmax-0.1, ymax-0.05, xmax-0.2, ymax-0.05, xmax-0.2, ymax-0.1), ncol=2, byrow=TRUE)))
  
  sfc <- sf::st_sfc(l1, l2, crs = sf::st_crs(huc_layer))
  sf::st_sf(habitat_type = c("Lake/Pond", "Lake/Pond"), geometry = sfc)
}

#' Moose Sighting Landmark Geometries
#'
#' Retrieves notable moose sighting locations (Washington Creek, Ojibway Lake, Feldtmann Lake, Hidden Lake).
#'
#' @param watershed_obj Target spatial watershed or landscape object.
#' @param use_cache Logical; if TRUE, uses local pre-fetched dataset when available.
#' @param site Target simulation landscape/site folder name (default: `"isle_royale"`).
#'
#' @return An `sf` data frame of point landmark geometries.
#' @export
#' @rdname habitat
get_moose_landmarks <- function(watershed_obj = NULL, use_cache = TRUE, site = "isle_royale") {
  huc_layer <- if (is.null(watershed_obj)) {
    NULL
  } else if (inherits(watershed_obj, "sf") || inherits(watershed_obj, "sfc")) {
    watershed_obj
  } else if (is.list(watershed_obj) && "layer" %in% names(watershed_obj)) {
    watershed_obj$layer
  } else {
    NULL
  }
  
  if (use_cache) {
    cached_sf <- NULL
    lm_key <- paste0(site, "_landmarks")
    lm_file <- paste0(site, "_landmarks.rds")
    if (exists("isle_royale_datasets") && is.list(isle_royale_datasets) && !is.null(isle_royale_datasets[[lm_key]])) {
      cached_sf <- isle_royale_datasets[[lm_key]]
    } else if (exists(lm_key) && inherits(get(lm_key), "sf")) {
      cached_sf <- get(lm_key)
    } else {
      cache_file <- get_site_cache_file(lm_file, site = site)
      if (file.exists(cache_file)) {
        cached_sf <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      }
    }
    
    if (!is.null(cached_sf) && inherits(cached_sf, "sf")) {
      if (is.null(huc_layer)) return(cached_sf)
      cached_sf <- sf::st_transform(cached_sf, sf::st_crs(huc_layer))
      return(cached_sf)
    }
  }
  
  # Fallback coordinate table for notable Isle Royale moose sighting locations
  landmarks <- data.frame(
    name = c("Washington Creek", "Ojibway Lake", "Feldtmann Lake", "Hidden Lake"),
    lon = c(-89.145, -88.618, -88.948, -88.647),
    lat = c(47.922, 48.113, 47.887, 48.148),
    description = c("Major feeding stream near Windigo",
                    "Highland lake surrounded by moose browse",
                    "SW inland lake with heavy aquatic vegetation",
                    "Shaded lake near Tobin Harbor"),
    stringsAsFactors = FALSE
  )
  
  pts <- sf::st_as_sf(landmarks, coords = c("lon", "lat"), crs = 4326)
  if (!is.null(huc_layer)) {
    pts <- sf::st_transform(pts, sf::st_crs(huc_layer))
  }
  return(pts)
}

#' Construct Spatial Substrate Hexagonal Overlay
#'
#' Projects a mathematical hexagonal substrate grid overlay across an extracted watershed or landscape boundary.
#'
#' @param huc_info Watershed object or list containing a `layer` geometry.
#' @param hex_diameter Diameter of hexagonal grid cells in degrees (default = `0.01`).
#'
#' @return `add_watershed_hex_overlay`: An S3 object of class `watershed_hex_overlay` containing the original geometry plus the hex layer.
#' @export
#' @rdname habitat
#'
#' @importFrom sf st_make_grid st_intersects
add_watershed_hex_overlay <- function(huc_info, hex_diameter = 0.01) {
  huc_layer <- huc_info$layer
  
  hex_mesh <- sf::st_make_grid(huc_layer, square = FALSE, cellsize = c(hex_diameter, hex_diameter))
  hex_overlay <- hex_mesh[lengths(safe_st_intersects(hex_mesh, huc_layer)) > 0]
  
  huc_info$hex_overlay <- hex_overlay
  huc_info$hex_diameter <- hex_diameter
  
  class(huc_info) <- "watershed_hex_overlay"
  return(huc_info)
}

#' @param object An S3 object of class `watershed_hex_overlay`.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return `autoplot.watershed_hex_overlay`: A `ggplot` object representing the spatial mesh.
#' @export
#' @rdname habitat
#'
#' @importFrom ggplot2 ggplot geom_sf theme_minimal ggtitle labs
autoplot.watershed_hex_overlay <- function(object, ...) {
  huc_str <- if (!is.null(object$huc_id) && length(object$huc_id) > 1) {
    paste0(length(object$huc_id), " Combined Regions")
  } else if (!is.null(object$huc_id)) {
    paste("Region:", object$huc_id)
  } else {
    "Substrate Grid"
  }
  
  title_txt <- paste("Geographic Hexagonal Grid (", huc_str, ")", 
                     "\nHexagon Extent Diameter:", object$hex_diameter)
  if (!is.null(object$feature_name) && object$feature_name != "") {
    title_txt <- paste0(title_txt, " - Restricted to: ", object$feature_name)
  }
  
  p <- ggplot2::ggplot()
  if (!is.null(object$individual_hucs) && nrow(object$individual_hucs) > 1) {
    p <- p + ggplot2::geom_sf(data = object$individual_hucs, fill = NA, color = "purple", linetype = "dashed", linewidth = 0.4)
  }
  
  p +
    ggplot2::geom_sf(data = object$layer, fill = "lightblue", alpha = 0.3, color = "blue", linewidth = 0.7) +
    ggplot2::geom_sf(data = object$hex_overlay, fill = NA, color = "darkred", linewidth = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title_txt) +
    ggplot2::labs(x = "Longitude", y = "Latitude")
}

#' Create Isle Royale Spatial Substrate Overlay
#'
#' Standalone utility to construct the Isle Royale spatial hexagonal substrate grid.
#'
#' @param hex_diameter Hexagon extent diameter in degrees (default: `0.01`).
#' @param features Optional sf object of habitat features.
#' @param layer Optional sf object of landscape boundary layer.
#' @param site Target simulation landscape/site folder name (default: `"isle_royale"`).
#'
#' @return An S3 object of class `watershed_hex_overlay`.
#' @export
#' @rdname habitat
create_isle_royale_hex_overlay <- function(hex_diameter = 0.01, features = NULL, layer = NULL, site = "isle_royale") {
  boundary_layer <- layer
  layer_key <- paste0(site, "_layer")
  layer_file <- paste0(site, "_layer.rds")
  feat_key <- paste0(site, "_features")
  feat_file <- paste0(site, "_features.rds")
  
  if (is.null(boundary_layer)) {
    if (exists("isle_royale_datasets") && is.list(isle_royale_datasets) && !is.null(isle_royale_datasets[[layer_key]])) {
      boundary_layer <- isle_royale_datasets[[layer_key]]
    } else if (exists(layer_key) && inherits(get(layer_key), c("sf", "sfc"))) {
      boundary_layer <- get(layer_key)
    } else {
      cache_file <- get_site_cache_file(layer_file, site = site)
      if (file.exists(cache_file)) {
        boundary_layer <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      }
    }
  }
  
  habitat_sf <- features
  if (is.character(habitat_sf) && file.exists(habitat_sf)) {
    habitat_sf <- tryCatch(readRDS(habitat_sf), error = function(e) NULL)
  }
  if (is.null(habitat_sf)) {
    if (exists("isle_royale_datasets") && is.list(isle_royale_datasets) && !is.null(isle_royale_datasets[[feat_key]])) {
      habitat_sf <- isle_royale_datasets[[feat_key]]
    } else if (exists(feat_key) && inherits(get(feat_key), "sf")) {
      habitat_sf <- get(feat_key)
    } else {
      cache_file <- get_site_cache_file(feat_file, site = site)
      if (file.exists(cache_file)) {
        habitat_sf <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      }
    }
  }
  
  if (is.null(boundary_layer) && !is.null(habitat_sf) && inherits(habitat_sf, "sf")) {
    boundary_layer <- suppressWarnings(sf::st_union(sf::st_geometry(habitat_sf)))
  }
  
  if (is.null(boundary_layer)) {
    pts <- matrix(c(-89.17, 47.87, -88.48, 47.87, -88.48, 48.16, -89.17, 48.16, -89.17, 47.87), ncol = 2, byrow = TRUE)
    boundary_layer <- sf::st_sfc(sf::st_polygon(list(pts)), crs = 4326)
  } else if (inherits(boundary_layer, "sf")) {
    boundary_layer <- sf::st_geometry(boundary_layer)
  }
  
  cent <- suppressWarnings(sf::st_centroid(boundary_layer))
  coords <- sf::st_coordinates(cent)
  
  hex_mesh <- sf::st_make_grid(boundary_layer, square = FALSE, cellsize = c(hex_diameter, hex_diameter))
  hex_overlay <- hex_mesh[lengths(safe_st_intersects(hex_mesh, boundary_layer)) > 0]
  
  res <- list(
    huc_id = "Isle Royale",
    feature_name = "Isle Royale",
    lon = as.numeric(coords[1, "X"]),
    lat = as.numeric(coords[1, "Y"]),
    layer = boundary_layer,
    hex_overlay = hex_overlay,
    hex_diameter = hex_diameter
  )
  class(res) <- "watershed_hex_overlay"
  return(res)
}

#' Add Moose Habitat Features & Compute Hex Substrate Suitability
#'
#' @param hex_obj S3 object returned from `add_watershed_hex_overlay()` or `create_isle_royale_hex_overlay()`.
#' @param habitat_sf Optional sf object of habitat features.
#' @param landmarks_sf Optional sf object of sighting landmarks.
#' @param features Deprecated alias for `habitat_sf`.
#' @param landmarks Deprecated alias for `landmarks_sf`.
#' @param site Target simulation landscape/site folder name (default: `"isle_royale"`).
#'
#' @return `add_habitat_hex_overlay`: An S3 object of class `habitat_hex_overlay`.
#' @export
#' @rdname habitat
add_habitat_hex_overlay <- function(hex_obj, habitat_sf = NULL, landmarks_sf = NULL, 
                                    features = NULL, landmarks = NULL, site = "isle_royale") {
  if (is.null(habitat_sf)) habitat_sf <- features
  if (is.null(landmarks_sf)) landmarks_sf <- landmarks
  
  if (is.character(habitat_sf) && file.exists(habitat_sf)) {
    habitat_sf <- tryCatch(readRDS(habitat_sf), error = function(e) NULL)
  }
  if (is.character(landmarks_sf) && file.exists(landmarks_sf)) {
    landmarks_sf <- tryCatch(readRDS(landmarks_sf), error = function(e) NULL)
  }
  
  if (is.null(habitat_sf)) {
    habitat_sf <- get_habitat_features(hex_obj, site = site)
  }
  if (is.null(landmarks_sf)) {
    landmarks_sf <- get_moose_landmarks(hex_obj, site = site)
  }
  
  hex_mesh <- hex_obj$hex_overlay
  scores <- numeric(length(hex_mesh))
  types_list <- character(length(hex_mesh))
  
  if (!is.null(habitat_sf) && nrow(habitat_sf) > 0) {
    inter <- safe_st_intersects(hex_mesh, habitat_sf)
    for (i in seq_along(inter)) {
      indices <- inter[[i]]
      if (length(indices) > 0) {
        sub_types <- habitat_sf$habitat_type[indices]
        score <- 1
        if ("Lake/Pond" %in% sub_types) score <- score + 2
        if ("Waterway" %in% sub_types) score <- score + 1.5
        if ("Bog/Wetland" %in% sub_types) score <- score + 1.8
        if ("Forest" %in% sub_types) score <- score + 1
        scores[i] <- score
        types_list[i] <- paste(unique(sub_types), collapse = ", ")
      } else {
        scores[i] <- 1
        types_list[i] <- "Upland/Open"
      }
    }
  } else {
    scores[] <- 1
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

#' Visual Autoplot for Habitat Substrate Overlay
#'
#' @param object An S3 object of class `habitat_hex_overlay`.
#' @param show_landmarks Logical; if TRUE, renders moose sighting landmarks on map.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A `ggplot` object.
#' @export
#' @rdname habitat
#'
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_viridis_c theme_minimal ggtitle labs geom_sf_text
autoplot.habitat_hex_overlay <- function(object, show_landmarks = TRUE, ...) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = object$layer, fill = "#eef4f8", color = "#2c3e50", linewidth = 0.8)
  
  if (!is.null(object$habitat_sf) && nrow(object$habitat_sf) > 0) {
    p <- p + ggplot2::geom_sf(data = object$habitat_sf, ggplot2::aes(fill = .data$habitat_type), alpha = 0.5, color = NA)
  }
  
  if (!is.null(object$hex_habitat_sf)) {
    p <- p + ggplot2::geom_sf(data = object$hex_habitat_sf, ggplot2::aes(color = .data$habitat_score), fill = NA, linewidth = 0.6) +
      ggplot2::scale_color_viridis_c(option = "viridis", name = "Habitat Weight")
  }
  
  if (show_landmarks && !is.null(object$landmarks_sf) && nrow(object$landmarks_sf) > 0) {
    p <- p + ggplot2::geom_sf(data = object$landmarks_sf, color = "#d35400", size = 3, shape = 18) +
      ggplot2::geom_sf_text(data = object$landmarks_sf, ggplot2::aes(label = .data$name), color = "#900c3f", size = 3, fontface = "bold", vjust = -0.7)
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

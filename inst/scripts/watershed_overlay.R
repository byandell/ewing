#!/usr/bin/env Rscript
# watershed_overlay.R
# Prototyping geographic transformation of Tridiagonal ewing substrates onto HUC12 overlays.

library(ggplot2)
library(sf)

library(nhdplusTools)

# 1. Geographic Anchor Coordinate Data
huc12_id <- "041800000101" # Isle Royale

# Function to determine huc_lon and huc_lat from HUC12
get_huc_centroid <- function(huc_id) {
  # Get HUC12 sf object (queries USGS WBD)
  huc_layer <- get_huc(id = huc_id, type = "huc12")
  
  # Calculate geographic centroid
  centroid <- suppressWarnings(st_centroid(st_geometry(huc_layer)))
  coords <- st_coordinates(centroid)
  
  list(
    lon = as.numeric(coords[1, "X"]),
    lat = as.numeric(coords[1, "Y"]),
    layer = huc_layer
  )
}

huc_info <- get_huc_centroid(huc12_id)
huc_lon <- huc_info$lon
huc_lat <- huc_info$lat
huc12_layer <- huc_info$layer

# 2. Extract Triangular Abstract Data (reusing our plant script)
if (file.exists("inst/scripts/plant_triangle.R")) {
  source("inst/scripts/plant_triangle.R", local = TRUE)
} else if (file.exists("plant_triangle.R")) {
  source("plant_triangle.R", local = TRUE)
} else {
  stop("Ensure plant_triangle.R is accessible to populate abstract geometries.")
}

# 3. Affine Scaling Pipeline
# Define scaling meters per simulation grid unit (S)
# e.g., 1 tri-coord spatial unit = 0.05 degrees spatially.
geo_scale <- 0.005 

# Center all points around the network mass and apply the multiplier!
centroid_x <- mean(all_points$x)
centroid_y <- mean(all_points$y)

geo_points <- all_points
geo_points$lon <- ((geo_points$x - centroid_x) * geo_scale) + huc_lon
geo_points$lat <- ((geo_points$y - centroid_y) * geo_scale) + huc_lat

# Same translation for standard geometric bounds
geo_poly <- poly_df
geo_poly$lon <- ((geo_poly$x - centroid_x) * geo_scale) + huc_lon
geo_poly$lat <- ((geo_poly$y - centroid_y) * geo_scale) + huc_lat

geo_labels <- labels_df
geo_labels$lon <- ((geo_labels$x - centroid_x) * geo_scale) + huc_lon
geo_labels$lat <- ((geo_labels$y - centroid_y) * geo_scale) + huc_lat

# 4. Generate Prototype Map
p_geo <- ggplot() +
  # Represents underlying HUC12 outline
  geom_sf(data = huc12_layer, fill="lightblue", alpha=0.3, color="blue", linewidth=0.5) +
  geom_polygon(data=geo_poly, aes(x=lon, y=lat, group=substrate), fill=NA, color="black", linewidth=0.7) +
  geom_point(data=geo_points, aes(x=lon, y=lat, color=substrate), size=1.5) +
  geom_text(data=geo_labels, aes(x=lon, y=lat, label=label), color="darkred", fontface="bold", size=4) +
  theme_minimal() +
  ggtitle(paste("Ewing Network Geographic Projection (HUC 12:", huc12_id, ")", 
                "\nCentered at:", round(huc_lon, 3), round(huc_lat, 3))) +
  labs(x="Longitude", y="Latitude")

out_geo <- "huc12_overlay.png"
ggsave(out_geo, p_geo, width=9, height=7, bg="white")
cat("Generated geographic transformation", out_geo, "\n")

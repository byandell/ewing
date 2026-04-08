#!/usr/bin/env Rscript
# watershed_overlay.R
# Prototyping geographic transformation of Tridiagonal ewing substrates onto HUC12 overlays.

library(ggplot2)
library(sf)

# Note: In a production environment with interactive downloads, we might pull:
# library(nhdplusTools)
# huc12_layer <- get_huc12(id = "070700050206") 
# Instead, we construct a mock spatial representation mapping to the Prairie du Sac Dam-Wisconsin River subwatershed

# 1. Geographic Anchor Coordinate Data
# Prairie du Sac Dam-Wisconsin River Centroid Approximation:
huc_lon <- -89.731
huc_lat <- 43.275

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
  # Represents underlying HUC12 shape (mock boundary box via annotation, realistically replaced with geom_sf)
  annotate("rect", xmin=huc_lon - 0.2, xmax=huc_lon + 0.2, 
           ymin=huc_lat - 0.2, ymax=huc_lat + 0.2, fill="lightblue", alpha=0.3) +
  geom_polygon(data=geo_poly, aes(x=lon, y=lat, group=substrate), fill=NA, color="black", linewidth=0.7) +
  geom_point(data=geo_points, aes(x=lon, y=lat, color=substrate), size=1.5) +
  geom_text(data=geo_labels, aes(x=lon, y=lat, label=label), color="darkred", fontface="bold", size=4) +
  theme_minimal() +
  coord_quickmap() +
  ggtitle(paste("Ewing Network Geographic Projection (HUC 12: 070700050206)", 
                "\nCentered at:", huc_lon, huc_lat)) +
  labs(x="Longitude", y="Latitude")

out_geo <- "huc12_overlay.png"
ggsave(out_geo, p_geo, width=9, height=7, bg="white")
cat("Generated geographic transformation", out_geo, "\n")

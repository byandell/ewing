#!/usr/bin/env Rscript
# watershed_overlay.R
# Prototyping geographic transformation of Tridiagonal ewing substrates onto HUC12 overlays.

# 1. Geographic Anchor Coordinate Data
huc12_id <- "041800000101" # Isle Royale

# 2. Example Usage & Output Generation
# Fetch restricted geometry 
huc_info <- ewing::get_watershed(huc12_id, feature_name = "Isle Royale")

# Construct hex map overlay object
hex_obj <- ewing::add_watershed_hex_overlay(huc_info, hex_diameter = 0.01)

# Plot S3 object using autoplot
p_geo <- ggplot2::autoplot(hex_obj)

# 3. Export
out_geo <- "huc12_hex_overlay.png"
ggplot2::ggsave(out_geo, p_geo, width=9, height=7, bg="white")
cat("Generated geographic hexagonal transformation:", out_geo, "\n")

#!/usr/bin/env Rscript
# inst/scripts/isle_royale_habitat.R
# Pre-computes spatial habitat features and moose sighting landmarks for Isle Royale
# and saves them into inst/extdata/isle_royale/ for offline / high-performance loading.

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(ewing)
})

# Load local R functions if developing locally
if (file.exists("R/habitat.R")) {
  source("R/habitat.R")
}

cat("1. Building Base Isle Royale Hexagonal Substrate Overlay...\n")
hex_obj <- create_isle_royale_hex_overlay(hex_diameter = 0.01)

cat("3. Geocoding Moose Sighting Landmarks...\n")
landmarks_sf <- get_moose_landmarks(hex_obj, use_cache = FALSE)

cat("4. Extracting / Constructing Moose Habitat Features (Lakes, Waterways, Forests, Bogs)...\n")
habitat_sf <- get_habitat_features(hex_obj, use_cache = FALSE)

cat("5. Saving Feature Datasets to inst/extdata/isle_royale/ ...\n")
target_dir <- "inst/extdata/isle_royale"
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

saveRDS(landmarks_sf, file.path(target_dir, "isle_royale_landmarks.rds"))
saveRDS(habitat_sf, file.path(target_dir, "isle_royale_features.rds"))
cat("   Saved:", file.path(target_dir, "isle_royale_landmarks.rds"), "\n")
cat("   Saved:", file.path(target_dir, "isle_royale_features.rds"), "\n")

cat("6. Generating Moose Habitat Substrate Overlay Object...\n")
habitat_overlay <- add_habitat_hex_overlay(hex_obj, habitat_sf = habitat_sf, landmarks_sf = landmarks_sf)

cat("7. Plotting & Exporting Visualization...\n")
p <- ggplot2::autoplot(habitat_overlay)
out_png <- "isle_royale_habitat_map.png"
ggplot2::ggsave(out_png, p, width = 10, height = 7, bg = "white")
cat("   Generated Habitat Overlay Plot:", out_png, "\n")
cat("Done!\n")

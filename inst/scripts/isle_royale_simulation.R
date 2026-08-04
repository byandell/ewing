#!/usr/bin/env Rscript
# inst/scripts/isle_royale_simulation.R
# Initializes and executes spatial Isle Royale wolf-moose landscape simulation,
# incorporating habitat suitability preferences (lakes, beaver ponds, forests, bogs),
# moose life stage structure (Calf, Yearling, Adult, Senior), and empirical census benchmarking.

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(ewing)
})

# Load local development R files if present
if (file.exists("R/isle_royale_sim.R")) {
  source("R/habitat.R")
  source("R/isle_royale_sim.R")
}

cat("1. Initializing Isle Royale Spatial Simulation (Start Year 1980)...\n")
sim_obj <- init_isle_royale_sim(year = 1980)

cat("2. Executing Simulation Steps...\n")
sim_obj <- run_isle_royale_sim(sim_obj, nstep = 500, refresh = 100)

cat("3. Generating Visualization (Spatial Landscape + Empirical Census Time Series)...\n")
p_sim <- ggplot_isle_royale_sim(sim_obj)

out_png <- "isle_royale_simulation_plot.png"
ggplot2::ggsave(out_png, p_sim, width = 10, height = 9, bg = "white")
cat("   Exported Simulation Plot:", out_png, "\n")
cat("Done!\n")

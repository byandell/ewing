#!/usr/bin/env Rscript

if(!exists("tri2car")) {
  if (file.exists("R/triangle.R")) source("R/triangle.R")
  else if (file.exists("../../R/triangle.R")) source("../../R/triangle.R")
}

if(!exists("tri2car")) {
  if (file.exists("R/triangle.R")) source("R/triangle.R")
  else if (file.exists("../../R/triangle.R")) source("../../R/triangle.R")
}

if(file.exists("R/substrate_triangle.R")) {
  source("R/substrate_triangle.R")
} else {
  library(ewing)
}

library(ggplot2)

# Example Usage & Output Generation
topology <- substrate_topology()
my_substrate <- create_substrate(topology)

p <- autoplot.substrate(my_substrate)

out_file <- "substrate_triangle_reconstruction.png"
ggsave(out_file, p, width=9, height=7, bg="white")
cat("Generated", out_file, "\n")

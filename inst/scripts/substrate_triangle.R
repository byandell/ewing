#!/usr/bin/env Rscript

# Example Usage & Output Generation
topology <- ewing::substrate_topology()
my_substrate <- ewing::create_substrate(topology)

p <- ggplot2::autoplot(my_substrate)

out_file <- "substrate_triangle_reconstruction.png"
ggplot2::ggsave(out_file, p, width=9, height=7, bg="white")
cat("Generated", out_file, "\n")

#!/usr/bin/env Rscript

if(!exists("tri2car")) {
  if (file.exists("R/triangle.R")) source("R/triangle.R")
  else if (file.exists("../../R/triangle.R")) source("../../R/triangle.R")
}

get_substrate_grid <- function(width, step = 1, orientation = "up") {
  pts <- expand.grid(a = seq(0, width - step, by = step), 
                     b = seq(0, width - step, by = step))
  
  if (orientation == "up") {
    pts <- subset(pts, a + b <= width - step)
    pts$c <- -(pts$a + pts$b)
  } else {
    pts <- subset(pts, a + b <= width - step)
    pts$a <- -pts$a
    pts$b <- -pts$b
    pts$c <- -(pts$a + pts$b)
  }
  return(pts)
}

width <- 10
step <- 1

# W determines the coordinate displacement distance.
# Since we generate 10 dots via seq(0, 9), the mathematical distance is 9.
W <- width - step

# Topology adjacency offsets
topology <- list(
  fr2   = list(offset = c(0, 0, 0), dir = "down"),
  fr1   = list(offset = c(-W, -W, 2*W), dir = "up"),
  fr3   = list(offset = c(-W, 0, W), dir = "up"),
  fr4   = list(offset = c(0, -W, W), dir = "up"),
  tw1   = list(offset = c(-W, 0, W), dir = "down"),
  tw2   = list(offset = c(-2*W, 0, 2*W), dir = "up"),
  lftop = list(offset = c(-2*W, W, W), dir = "down"),
  lfbot = list(offset = c(-3*W, W, 2*W), dir = "up")
)

all_points <- data.frame()
labels_df <- data.frame()
poly_df <- data.frame()

for (sub in names(topology)) {
  cfg <- topology[[sub]]
  grid <- get_substrate_grid(width, step, cfg$dir)
  
  o_a <- cfg$offset[1]
  o_b <- cfg$offset[2]
  o_c <- cfg$offset[3]

  grid_tri <- tricoord(grid$a, grid$b, grid$c)
  grid_tri <- grid_tri + cfg$offset
  
  car_pts <- tri2car(grid_tri)
  car_pts$substrate <- sub
  all_points <- rbind(all_points, car_pts)
  
  # Determine bounds and midpoints for side labels 1,2,3
  if (cfg$dir == "up") {
    v_top <- c(o_a, o_b, o_c)
    v_br  <- c(o_a + W, o_b, o_c - W)
    v_bl  <- c(o_a, o_b + W, o_c - W)
    
    # Edges: 1 (left edge, a=const), 2 (right edge, b=const), 3 (bot edge, c=const)
    # Midpoints of the sides
    m1 <- (v_top + v_bl) / 2
    m2 <- (v_top + v_br) / 2
    m3 <- (v_bl + v_br)  / 2
    
    centroid <- (v_top + v_br + v_bl) / 3
  } else {
    v_bot <- c(o_a, o_b, o_c)
    v_tr  <- c(o_a, o_b - W, o_c + W)
    v_tl  <- c(o_a - W, o_b, o_c + W)
    
    # Edges: 1 (right edge, a=const), 2 (left edge, b=const), 3 (top edge, c=const) 
    m1 <- (v_bot + v_tr) / 2
    m2 <- (v_bot + v_tl) / 2
    m3 <- (v_tl + v_tr)  / 2
    
    centroid <- (v_bot + v_tr + v_tl) / 3
  }
  
  # Interpolate slightly towards the centroid to put text "just inside" the edges
  w_in <- 0.25 # weight towards centroid
  l1 <- m1 * (1 - w_in) + centroid * w_in
  l2 <- m2 * (1 - w_in) + centroid * w_in
  l3 <- m3 * (1 - w_in) + centroid * w_in
  
  mat_l <- cbind(l1, l2, l3)
  car_l <- tri2car(mat_l)
  car_l$label <- c("1", "2", "3")
  car_l$substrate <- sub
  labels_df <- rbind(labels_df, car_l)
  
  # Extract polygon corners for drawing black line
  if (cfg$dir == "up") {
    p_mat <- cbind(v_top, v_br, v_bl)
  } else {
    p_mat <- cbind(v_bot, v_tr, v_tl)
  }
  car_p <- tri2car(p_mat)
  car_p$substrate <- sub
  poly_df <- rbind(poly_df, car_p)
}

library(ggplot2)
p <- ggplot() +
  # Draw the black boundary lines outlining the substrates exactly over outer dots
  geom_polygon(data=poly_df, aes(x=x, y=y, group=substrate), fill=NA, color="black", linewidth=0.7) +
  # Plot grid dots
  geom_point(data=all_points, aes(x=x, y=y, color=substrate), size=1.5) +
  theme_void() +
  coord_fixed() +
  ggtitle("Ewing Tridiagonal Substrate Network Mapping")

# Labels logic
centers <- aggregate(cbind(x,y) ~ substrate, data=all_points, mean)
p <- p + geom_text(data=centers, aes(x=x, y=y, label=substrate), color="black", fontface="bold", size=5)

# Axis side numbers
p <- p + geom_text(data=labels_df, aes(x=x, y=y, label=label), color="darkred", fontface="bold", size=4)

out_file <- "plant_triangle_reconstruction.png"
ggsave(out_file, p, width=9, height=7, bg="white")
cat("Generated", out_file, "\n")

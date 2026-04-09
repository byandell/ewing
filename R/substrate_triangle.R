#' Generate a Substrate Abstract Grid
#'
#' Constructs the analytical coordinate points bounding a standard subset of a tridiagonal geometric matrix.
#'
#' @param width Integer representing the radius spanning across a substrate patch.
#' @param step Integer interval density between geometric dots.
#' @param orientation Character metric "up" or "down" dictating mathematical inversion of coordinate arrays.
#'
#' @return A data.frame holding the computed relative tridiagonal mappings (`a`, `b`, `c`).
#' @export
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

#' Define Tridiagonal Substrate Topology
#'
#' Constructs the foundational topological mapping linking multiple substrate component offsets
#' (e.g., fruit vs. leaf elements) into an interconnected mathematical mesh matrix.
#'
#' @param width Integer representing the radius size limits of spatial components.
#' @param step Numeric grid density spacing. 
#'
#' @return A list encompassing adjacency parameters `offset` and structural `dir` for specific modules.
#' @export
substrate_topology <- function(width = 10, step = 1) {
  W <- width - step
  
  # Topology adjacency offsets
  list(
    fr2   = list(offset = c(0, 0, 0), dir = "down"),
    fr1   = list(offset = c(-W, -W, 2*W), dir = "up"),
    fr3   = list(offset = c(-W, 0, W), dir = "up"),
    fr4   = list(offset = c(0, -W, W), dir = "up"),
    tw1   = list(offset = c(-W, 0, W), dir = "down"),
    tw2   = list(offset = c(-2*W, 0, 2*W), dir = "up"),
    lftop = list(offset = c(-2*W, W, W), dir = "down"),
    lfbot = list(offset = c(-3*W, W, 2*W), dir = "up")
  )
}

#' Create Substrate Geometric Object
#'
#' Iterates against a defined configuration metric to build explicit spatial network definitions
#' tracking Euclidean point plots, dynamic border labels, and spanning geometric boundary polygons.
#'
#' @param topology Abstract parameter list derived from `substrate_topology()`.
#' @param width Physical geometric limits of bounding constraints.
#' @param step Inner plotting density logic parameter.
#'
#' @return An S3 object of format class `substrate`, storing resolved attributes.
#' @export
#'
#' @importFrom stats aggregate
create_substrate <- function(topology, width = 10, step = 1) {
  W <- width - step
  all_points <- data.frame()
  labels_df <- data.frame()
  poly_df <- data.frame()
  
  for (sub in names(topology)) {
    cfg <- topology[[sub]]
    grid <- get_substrate_grid(width, step, cfg$dir)
    
    o_a <- cfg$offset[1]
    o_b <- cfg$offset[2]
    o_c <- cfg$offset[3]

    # Needs tricoord and tri2car which are presumably exported/available from R/triangle.R
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
      
      m1 <- (v_top + v_bl) / 2
      m2 <- (v_top + v_br) / 2
      m3 <- (v_bl + v_br)  / 2
      centroid <- (v_top + v_br + v_bl) / 3
      p_mat <- cbind(v_top, v_br, v_bl)
    } else {
      v_bot <- c(o_a, o_b, o_c)
      v_tr  <- c(o_a, o_b - W, o_c + W)
      v_tl  <- c(o_a - W, o_b, o_c + W)
      
      m1 <- (v_bot + v_tr) / 2
      m2 <- (v_bot + v_tl) / 2
      m3 <- (v_tl + v_tr)  / 2
      centroid <- (v_bot + v_tr + v_tl) / 3
      p_mat <- cbind(v_bot, v_tr, v_tl)
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
    
    car_p <- tri2car(p_mat)
    car_p$substrate <- sub
    poly_df <- rbind(poly_df, car_p)
  }
  
  centers <- stats::aggregate(cbind(x,y) ~ substrate, data=all_points, mean)
  
  obj <- list(
    points = all_points, 
    labels = labels_df, 
    poly = poly_df, 
    centers = centers,
    topology = topology
  )
  class(obj) <- "substrate"
  return(obj)
}

#' Plot Visual Substrate Network Layouts
#'
#' Organizes numerical nodes logically over explicit geometrical borders defined by `create_substrate` attributes.
#'
#' @param object Data structure of type `substrate` mapped previously via abstract tridiagonal coordinates.
#' @param ... Additional graphical tracking arguments passing to generic rendering matrices.
#'
#' @return A `ggplot` visual abstraction of the Ewing network.
#' @export
#'
#' @importFrom ggplot2 ggplot geom_polygon aes geom_point geom_text theme_void coord_fixed ggtitle
autoplot.substrate <- function(object, ...) {
  ggplot2::ggplot() +
    # Draw the black boundary lines outlining the substrates exactly over outer dots
    ggplot2::geom_polygon(data=object$poly, ggplot2::aes(x=x, y=y, group=substrate), fill=NA, color="black", linewidth=0.7) +
    # Plot grid dots
    ggplot2::geom_point(data=object$points, ggplot2::aes(x=x, y=y, color=substrate), size=1.5) +
    # Plot Substrate Labels (Centers)
    ggplot2::geom_text(data=object$centers, ggplot2::aes(x=x, y=y, label=substrate), color="black", fontface="bold", size=5) +
    # Axis side numbers
    ggplot2::geom_text(data=object$labels, ggplot2::aes(x=x, y=y, label=label), color="darkred", fontface="bold", size=4) +
    ggplot2::theme_void() +
    ggplot2::coord_fixed() +
    ggplot2::ggtitle("Ewing Tridiagonal Substrate Network Mapping")
}

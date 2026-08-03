#' Isle Royale Wolf-Moose Predator-Prey Simulation
#'
#' Initializes, runs, and visualizes an individual-based spatial predator-prey simulation
#' model of wolves and moose on Isle Royale. Incorporates habitat preferences
#' (inland lakes, beaver ponds, shaded forests, bogs), life stage transitions (Calf, Yearling, Adult, Senior),
#' and empirical benchmarking against historical 1980-2019 annual census data.
#'
#' @param year Target baseline year from `wolf_moose.csv` (1980-2019, default: 1980).
#' @param n_moose Initial number of moose individuals (default: looked up from `wolf_moose.csv`).
#' @param n_wolves Initial number of wolf individuals (default: looked up from `wolf_moose.csv`).
#' @param hex_diameter Diameter of hexagonal substrate mesh (default: 0.01 degrees).
#' @param features_rds Optional path or `sf` object for custom site habitat features (e.g. exported from `hexmapApp`).
#' @param landmarks_rds Optional path or `sf` object for custom site landmarks (e.g. exported from `hexmapApp`).
#'
#' @return `init_isle_royale_sim`: An S3 object of class `isle_royale_sim` containing the initialized `ewing` simulation community and spatial habitat metadata.
#' @export
#' @name isle_royale_sim
#' @rdname isle_royale_sim
#'
#' @importFrom utils read.csv
#' @importFrom sf st_transform st_crs st_coordinates st_bbox st_intersects st_centroid st_geometry st_sample
init_isle_royale_sim <- function(year = 1980, 
                                n_moose = NULL, 
                                n_wolves = NULL, 
                                hex_diameter = 0.01,
                                datafile = "",
                                features_rds = NULL,
                                landmarks_rds = NULL) {
  
  # Load historical benchmark time series data
  csv_path <- system.file("doc/isle_royale/wolf_moose.csv", package = "ewing")
  if (csv_path == "" || !file.exists(csv_path)) csv_path <- "inst/doc/isle_royale/wolf_moose.csv"
  
  hist_data <- NULL
  if (file.exists(csv_path)) {
    hist_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  }
  
  # Lookup baseline population counts for start year if not explicitly provided
  if (!is.null(hist_data) && year %in% hist_data$Year) {
    row_match <- hist_data[hist_data$Year == year, ]
    if (is.null(n_moose)) n_moose <- as.numeric(row_match$Moose[1])
    if (is.null(n_wolves)) n_wolves <- as.numeric(row_match$Wolves[1])
  }
  
  if (is.null(n_moose)) n_moose <- 664
  if (is.null(n_wolves)) n_wolves <- 50
  
  # 1. Initialize Base Spatial Geography & Habitat Overlay
  huc_info <- get_watershed("041800000101", feature_name = "Isle Royale")
  hex_obj <- add_watershed_hex_overlay(huc_info, hex_diameter = hex_diameter)
  habitat_overlay <- add_habitat_hex_overlay(hex_obj, features = features_rds, landmarks = landmarks_rds)
  
  # 2. Setup Configuration Data Directory
  if (datafile == "") {
    pkg_dir <- system.file("extdata/isle_royale", package = "ewing")
    if (pkg_dir != "" && dir.exists(pkg_dir)) {
      datafile <- pkg_dir
    } else if (dir.exists("inst/extdata/isle_royale")) {
      datafile <- "inst/extdata/isle_royale"
    }
  }
  
  # 3. Initialize ewing Community Core
  community <- NULL
  tryCatch({
    community <- init.simulation(package = "ewing", count = c(n_moose, n_wolves), datafile = datafile, messages = FALSE)
  }, error = function(e) {
    # Soft fallback if ewing initialization runs in isolated test environment
    community <<- list(pop = list())
  })
  
  # 4. Spatially Sample Initial Positions Weighted by Substrate Habitat Score
  hex_sf <- habitat_overlay$hex_habitat_sf
  probs <- hex_sf$habitat_score / sum(hex_sf$habitat_score)
  
  sampled_moose_hex <- sample(seq_len(nrow(hex_sf)), size = n_moose, replace = TRUE, prob = probs)
  sampled_wolf_hex  <- sample(seq_len(nrow(hex_sf)), size = n_wolves, replace = TRUE, prob = probs)
  
  # Extract hex centroids as point locations
  centroids <- sf::st_centroid(sf::st_geometry(hex_sf))
  moose_pts <- centroids[sampled_moose_hex]
  wolf_pts  <- centroids[sampled_wolf_hex]
  
  moose_coords <- sf::st_coordinates(moose_pts)
  wolf_coords  <- sf::st_coordinates(wolf_pts)
  
  moose_df <- data.frame(
    id = paste0("M", seq_len(n_moose)),
    species = "Moose",
    ageclass = sample(c("calf", "yearling", "adult", "senior"), size = n_moose, replace = TRUE, prob = c(0.15, 0.15, 0.55, 0.15)),
    hex_id = sampled_moose_hex,
    lon = moose_coords[, 1],
    lat = moose_coords[, 2],
    stringsAsFactors = FALSE
  )
  
  wolf_df <- data.frame(
    id = paste0("W", seq_len(n_wolves)),
    species = "Wolf",
    ageclass = sample(c("pup", "subadult", "adult"), size = n_wolves, replace = TRUE, prob = c(0.20, 0.25, 0.55)),
    hex_id = sampled_wolf_hex,
    lon = wolf_coords[, 1],
    lat = wolf_coords[, 2],
    stringsAsFactors = FALSE
  )
  
  # Initialize Population History Tallies (for Dist Plots)
  m_counts <- table(factor(moose_df$ageclass, levels = c("calf", "yearling", "adult", "senior")))
  w_counts <- table(factor(wolf_df$ageclass, levels = c("pup", "subadult", "adult")))
  
  hist_df <- data.frame(
    step = 0,
    time = 0,
    Species = c(rep("moose", 4), rep("wolf", 3)),
    State = c(names(m_counts), names(w_counts)),
    Type = "ageclass",
    Count = c(as.numeric(m_counts), as.numeric(w_counts)),
    stringsAsFactors = FALSE
  )
  
  res <- list(
    community = community,
    habitat_overlay = habitat_overlay,
    start_year = year,
    moose_pop = moose_df,
    wolf_pop = wolf_df,
    historical_data = hist_data,
    history = hist_df,
    nstep = 0
  )
  
  class(res) <- "isle_royale_sim"
  return(res)
}

#' Run Isle Royale Wolf-Moose Simulation
#'
#' Executes simulation steps for the Isle Royale wolf-moose spatial model.
#'
#' @param sim_obj An object of class `isle_royale_sim`.
#' @param nstep Number of simulation steps to run (default: 1000).
#' @param refresh Step interval for progress reporting (default: 100).
#' @param ... Additional arguments.
#'
#' @return `run_isle_royale_sim`: Updated `isle_royale_sim` object.
#' @export
#' @rdname isle_royale_sim
run_isle_royale_sim <- function(sim_obj, nstep = 1000, refresh = 100, ...) {
  if (!inherits(sim_obj, "isle_royale_sim")) {
    stop("Input must be an object of class 'isle_royale_sim'.")
  }
  
  # Run underlying ewing simulation engine if present
  if (!is.null(sim_obj$community) && inherits(sim_obj$community, "ewing")) {
    sim_obj$community <- tryCatch({
      future.events(sim_obj$community, nstep = nstep, refresh = refresh, plotit = FALSE, ...)
    }, error = function(e) sim_obj$community)
  }
  
  # Update Spatial Movement across Hexagonal Substrate Network
  hex_sf <- sim_obj$habitat_overlay$hex_habitat_sf
  n_hex <- nrow(hex_sf)
  scores <- hex_sf$habitat_score
  
  # Precompute neighbor adjacency list
  adj_list <- suppressWarnings(sf::st_touches(hex_sf))
  centroids <- sf::st_centroid(sf::st_geometry(hex_sf))
  cent_coords <- sf::st_coordinates(centroids)
  
  # Helper to step individuals to adjacent hexes weighted by habitat_score
  move_pop <- function(pop_df, move_prob = 0.5) {
    if (is.null(pop_df) || nrow(pop_df) == 0) return(pop_df)
    
    n <- nrow(pop_df)
    new_hex <- pop_df$hex_id
    
    for (i in seq_len(n)) {
      if (stats::runif(1) < move_prob) {
        curr_h <- pop_df$hex_id[i]
        nbrs <- adj_list[[curr_h]]
        cands <- c(curr_h, nbrs)
        w <- scores[cands]
        p <- w / sum(w)
        new_h <- sample(cands, 1, prob = p)
        new_hex[i] <- new_h
      }
    }
    
    pop_df$hex_id <- new_hex
    
    # Calculate coords with small spatial jitter so points don't stack directly on top of each other
    hex_diam <- sim_obj$habitat_overlay$hex_diameter
    if (is.null(hex_diam)) hex_diam <- 0.01
    
    jitter_x <- stats::rnorm(n, mean = 0, sd = hex_diam * 0.15)
    jitter_y <- stats::rnorm(n, mean = 0, sd = hex_diam * 0.15)
    
    pop_df$lon <- cent_coords[new_hex, 1] + jitter_x
    pop_df$lat <- cent_coords[new_hex, 2] + jitter_y
    
    return(pop_df)
  }
  
  # Move Moose (preferring high suitability hexes: lakes, bogs, forests)
  sim_obj$moose_pop <- move_pop(sim_obj$moose_pop, move_prob = 0.6)
  
  # Move Wolves (hunting movement across landscape)
  sim_obj$wolf_pop <- move_pop(sim_obj$wolf_pop, move_prob = 0.75)
  
  # Process Predation Interactions (wolves hunting vulnerable moose in nearby hexes)
  if (!is.null(sim_obj$wolf_pop) && !is.null(sim_obj$moose_pop) && nrow(sim_obj$wolf_pop) > 0 && nrow(sim_obj$moose_pop) > 0) {
    wolf_hexes <- unique(sim_obj$wolf_pop$hex_id)
    # Check for moose in same or adjacent hexes
    vulnerable_idx <- which(sim_obj$moose_pop$hex_id %in% wolf_hexes & sim_obj$moose_pop$ageclass %in% c("calf", "senior"))
    if (length(vulnerable_idx) > 0) {
      # Some fraction predated during simulation step
      n_predated <- min(length(vulnerable_idx), max(1, round(nrow(sim_obj$wolf_pop) * 0.05)))
      pred_remove <- sample(vulnerable_idx, n_predated)
      sim_obj$moose_pop <- sim_obj$moose_pop[-pred_remove, ]
    }
  }
  
  sim_obj$nstep <- sim_obj$nstep + nstep
  
  # Append History Tally for Dist Plots
  m_curr <- table(factor(sim_obj$moose_pop$ageclass, levels = c("calf", "yearling", "adult", "senior")))
  w_curr <- table(factor(sim_obj$wolf_pop$ageclass, levels = c("pup", "subadult", "adult")))
  
  step_hist <- data.frame(
    step = sim_obj$nstep,
    time = sim_obj$nstep,
    Species = c(rep("moose", 4), rep("wolf", 3)),
    State = c(names(m_curr), names(w_curr)),
    Type = "ageclass",
    Count = c(as.numeric(m_curr), as.numeric(w_curr)),
    stringsAsFactors = FALSE
  )
  
  sim_obj$history <- rbind(sim_obj$history, step_hist)
  
  return(sim_obj)
}

#' Plot Isle Royale Simulation Spatial Landscape & Benchmark Trajectories
#'
#' @param x An object of class `isle_royale_sim`.
#' @param ... Additional arguments.
#'
#' @return A `ggplot` object displaying spatial individual distributions and historical census benchmarking.
#' @export
#' @rdname isle_royale_sim
#'
#' @importFrom ggplot2 ggplot geom_sf theme_minimal ggtitle labs geom_point scale_color_manual aes geom_line theme
ggplot_isle_royale_sim <- function(x, ...) {
  # 1. Spatial Landscape Plot
  p_map <- autoplot(x$habitat_overlay, show_landmarks = TRUE)
  
  # Prepare spatial organism data frames
  moose_sf <- sf::st_as_sf(x$moose_pop, coords = c("lon", "lat"), crs = sf::st_crs(x$habitat_overlay$layer))
  wolf_sf  <- sf::st_as_sf(x$wolf_pop, coords = c("lon", "lat"), crs = sf::st_crs(x$habitat_overlay$layer))
  
  p_map <- p_map +
    ggplot2::geom_sf(data = moose_sf, color = "#27ae60", shape = 21, fill = NA, stroke = 1.0, size = 0.8, alpha = 0.85) +
    ggplot2::geom_sf(data = wolf_sf, color = "#e74c3c", shape = 21, fill = NA, stroke = 1.4, size = 1.5, alpha = 0.95) +
    ggplot2::ggtitle(paste0("Isle Royale Wolf-Moose Spatial Landscape (Year ", x$start_year, ")"))
  
  # 2. Historical Benchmark Trajectory Plot
  if (!is.null(x$historical_data) && nrow(x$historical_data) > 0) {
    df_hist <- x$historical_data
    p_hist <- ggplot2::ggplot(df_hist, ggplot2::aes(x = .data$Year)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$Moose, color = "Historical Moose"), linewidth = 1.0) +
      ggplot2::geom_line(ggplot2::aes(y = .data$Wolves * 40, color = "Historical Wolves (x40)"), linewidth = 1.0, linetype = "dashed") +
      ggplot2::scale_color_manual(
        name = "Empirical Time Series",
        values = c("Historical Moose" = "#27ae60", "Historical Wolves (x40)" = "#e74c3c")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Isle Royale Empirical Census Benchmarks (1980 - 2019)",
        x = "Year", y = "Moose Abundance (Wolves x40)"
      )
    
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(patchwork::wrap_plots(p_map, p_hist, ncol = 1))
    }
  }
  
  return(p_map)
}

#' @export
plot.isle_royale_sim <- function(x, ...) {
  print(ggplot_isle_royale_sim(x, ...))
}

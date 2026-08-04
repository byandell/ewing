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
  
  # Store datafile path and datasets list on return object
  ds_list <- list()
  if (exists("isle_royale_datasets") && is.list(isle_royale_datasets)) {
    ds_list <- isle_royale_datasets
  } else if (is.character(datafile) && datafile != "" && dir.exists(datafile)) {
    txt_files <- list.files(datafile, pattern = "\\.(txt|csv)$", full.names = TRUE)
    for (f in txt_files) {
      tbl_name <- tools::file_path_sans_ext(basename(f))
      df_f <- tryCatch({
        if (endsWith(f, ".csv")) utils::read.csv(f, stringsAsFactors = FALSE)
        else utils::read.table(f, header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)
      }, error = function(e) NULL)
      if (!is.null(df_f)) ds_list[[tbl_name]] <- df_f
    }
  }
  
  res <- list(
    community = community,
    habitat_overlay = habitat_overlay,
    start_year = year,
    moose_pop = moose_df,
    wolf_pop = wolf_df,
    historical_data = hist_data,
    history = hist_df,
    datafile = datafile,
    datasets = ds_list,
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
run_isle_royale_sim <- function(sim_obj, nstep = 1000, refresh = 10, ...) {
  if (!inherits(sim_obj, "isle_royale_sim")) {
    stop("Input must be an object of class 'isle_royale_sim'.")
  }
  
  hex_sf <- sim_obj$habitat_overlay$hex_habitat_sf
  n_hex <- nrow(hex_sf)
  scores <- hex_sf$habitat_score
  
  # Precompute neighbor adjacency list and centroid coordinates
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
    
    # Calculate coords with small spatial jitter
    hex_diam <- sim_obj$habitat_overlay$hex_diameter
    if (is.null(hex_diam)) hex_diam <- 0.01
    
    jitter_x <- stats::rnorm(n, mean = 0, sd = hex_diam * 0.15)
    jitter_y <- stats::rnorm(n, mean = 0, sd = hex_diam * 0.15)
    
    pop_df$lon <- cent_coords[new_hex, 1] + jitter_x
    pop_df$lat <- cent_coords[new_hex, 2] + jitter_y
    
    return(pop_df)
  }
  
  # Helper for dynamic life stage progression (aging, births, natural deaths)
  update_demographics <- function(moose_df, wolf_df) {
    # 1. Moose Life Stage Transitions & Mortality
    if (!is.null(moose_df) && nrow(moose_df) > 0) {
      n_m <- nrow(moose_df)
      # Aging
      calf_idx <- which(moose_df$ageclass == "calf")
      if (length(calf_idx) > 0) {
        trans <- calf_idx[stats::runif(length(calf_idx)) < 0.03]
        if (length(trans) > 0) moose_df$ageclass[trans] <- "yearling"
      }
      
      yearling_idx <- which(moose_df$ageclass == "yearling")
      if (length(yearling_idx) > 0) {
        trans <- yearling_idx[stats::runif(length(yearling_idx)) < 0.03]
        if (length(trans) > 0) moose_df$ageclass[trans] <- "adult"
      }
      
      adult_idx <- which(moose_df$ageclass == "adult")
      if (length(adult_idx) > 0) {
        trans <- adult_idx[stats::runif(length(adult_idx)) < 0.015]
        if (length(trans) > 0) moose_df$ageclass[trans] <- "senior"
        
        # Reproduction (Calf births from adults)
        n_births <- sum(stats::runif(length(adult_idx)) < 0.012)
        if (n_births > 0) {
          parent_hexes <- sample(moose_df$hex_id[adult_idx], n_births, replace = TRUE)
          max_id <- suppressWarnings(max(as.numeric(gsub("[^0-9]", "", moose_df$id)), na.rm = TRUE))
          if (!is.finite(max_id)) max_id <- nrow(moose_df)
          new_calves <- data.frame(
            id = paste0("M", max_id + seq_len(n_births)),
            species = "Moose",
            ageclass = "calf",
            hex_id = parent_hexes,
            lon = cent_coords[parent_hexes, 1] + stats::rnorm(n_births, 0, 0.001),
            lat = cent_coords[parent_hexes, 2] + stats::rnorm(n_births, 0, 0.001),
            stringsAsFactors = FALSE
          )
          moose_df <- rbind(moose_df, new_calves)
        }
      }
      
      # Natural mortality for seniors
      senior_idx <- which(moose_df$ageclass == "senior")
      if (length(senior_idx) > 0) {
        deaths <- senior_idx[stats::runif(length(senior_idx)) < 0.025]
        if (length(deaths) > 0) moose_df <- moose_df[-deaths, ]
      }
    }
    
    # 2. Wolf Life Stage Transitions & Mortality
    if (!is.null(wolf_df) && nrow(wolf_df) > 0) {
      pup_idx <- which(wolf_df$ageclass == "pup")
      if (length(pup_idx) > 0) {
        trans <- pup_idx[stats::runif(length(pup_idx)) < 0.04]
        if (length(trans) > 0) wolf_df$ageclass[trans] <- "subadult"
      }
      
      sub_idx <- which(wolf_df$ageclass == "subadult")
      if (length(sub_idx) > 0) {
        trans <- sub_idx[stats::runif(length(sub_idx)) < 0.04]
        if (length(trans) > 0) wolf_df$ageclass[trans] <- "adult"
      }
      
      w_adult_idx <- which(wolf_df$ageclass == "adult")
      if (length(w_adult_idx) > 0) {
        # Reproduction (Pup births)
        n_w_births <- sum(stats::runif(length(w_adult_idx)) < 0.015)
        if (n_w_births > 0) {
          parent_hexes <- sample(wolf_df$hex_id[w_adult_idx], n_w_births, replace = TRUE)
          max_w_id <- suppressWarnings(max(as.numeric(gsub("[^0-9]", "", wolf_df$id)), na.rm = TRUE))
          if (!is.finite(max_w_id)) max_w_id <- nrow(wolf_df)
          new_pups <- data.frame(
            id = paste0("W", max_w_id + seq_len(n_w_births)),
            species = "Wolf",
            ageclass = "pup",
            hex_id = parent_hexes,
            lon = cent_coords[parent_hexes, 1] + stats::rnorm(n_w_births, 0, 0.001),
            lat = cent_coords[parent_hexes, 2] + stats::rnorm(n_w_births, 0, 0.001),
            stringsAsFactors = FALSE
          )
          wolf_df <- rbind(wolf_df, new_pups)
        }
        
        # Natural adult wolf mortality
        w_deaths <- w_adult_idx[stats::runif(length(w_adult_idx)) < 0.01]
        if (length(w_deaths) > 0) wolf_df <- wolf_df[-w_deaths, ]
      }
    }
    
    list(moose = moose_df, wolf = wolf_df)
  }
  
  # Micro-step execution loop (recording history continuously as changes happen)
  step_chunk <- max(1, min(10, refresh))
  steps_left <- nstep
  
  while (steps_left > 0) {
    chunk <- min(step_chunk, steps_left)
    steps_left <- steps_left - chunk
    
    # 1. Underlying ewing simulation engine step if present
    if (!is.null(sim_obj$community) && inherits(sim_obj$community, "ewing")) {
      sim_obj$community <- tryCatch({
        future.events(sim_obj$community, nstep = chunk, refresh = chunk, plotit = FALSE, ...)
      }, error = function(e) sim_obj$community)
    }
    
    # 2. Update Spatial Movement
    sim_obj$moose_pop <- move_pop(sim_obj$moose_pop, move_prob = 0.6)
    sim_obj$wolf_pop  <- move_pop(sim_obj$wolf_pop, move_prob = 0.75)
    
    # 3. Process Wolf Predation on Vulnerable Moose
    if (!is.null(sim_obj$wolf_pop) && !is.null(sim_obj$moose_pop) && nrow(sim_obj$wolf_pop) > 0 && nrow(sim_obj$moose_pop) > 0) {
      wolf_hexes <- unique(sim_obj$wolf_pop$hex_id)
      vulnerable_idx <- which(sim_obj$moose_pop$hex_id %in% wolf_hexes & sim_obj$moose_pop$ageclass %in% c("calf", "senior"))
      if (length(vulnerable_idx) > 0) {
        n_predated <- min(length(vulnerable_idx), max(1, round(nrow(sim_obj$wolf_pop) * 0.02)))
        pred_remove <- sample(vulnerable_idx, n_predated)
        sim_obj$moose_pop <- sim_obj$moose_pop[-pred_remove, ]
      }
    }
    
    # 4. Process Life Stage Transitions & Demographics
    demog_res <- update_demographics(sim_obj$moose_pop, sim_obj$wolf_pop)
    sim_obj$moose_pop <- demog_res$moose
    sim_obj$wolf_pop  <- demog_res$wolf
    
    sim_obj$nstep <- sim_obj$nstep + chunk
    
    # 5. Append Micro-step History Tally (for continuous Dist Plot step curves)
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
  }
  
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
    
    if (requireNamespace("cowplot", quietly = TRUE)) {
      return(cowplot::plot_grid(p_map, p_hist, ncol = 1, rel_heights = c(1.2, 1)))
    }
  }
  
  return(p_map)
}

#' @export
plot.isle_royale_sim <- function(x, ...) {
  print(ggplot_isle_royale_sim(x, ...))
}

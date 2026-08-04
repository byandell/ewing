#' Generalized Ecosystem Spatial Predator-Prey Simulation
#'
#' Initializes, runs, and visualizes an individual-based spatial predator-prey simulation
#' model for any target ecosystem or landscape (e.g. `"isle_royale"`, `"yellowstone"`).
#' Incorporates spatial habitat preferences, life stage transitions, and optional
#' empirical census benchmarking data.
#'
#' @param ecosystem Target simulation landscape/site subfolder name under `extdata/` (default: `"isle_royale"`).
#' @param year Target baseline year from census benchmark file if available (default: 1980).
#' @param n_hosts Initial number of host/prey individuals (default: looked up from benchmark CSV or 664).
#' @param n_predators Initial number of predator individuals (default: looked up from benchmark CSV or 50).
#' @param hex_diameter Diameter of hexagonal substrate mesh (default: 0.01 degrees).
#' @param datafile Custom path to data directory or Excel workbook.
#' @param features_rds Optional path or `sf` object for custom site habitat features.
#' @param landmarks_rds Optional path or `sf` object for custom site landmarks.
#'
#' @return `init_ecosystem_sim`: An S3 object of class `c("[ecosystem]_sim", "ecosystem_sim", "ewing")`.
#' @export
#' @name ecosystem_sim
#' @rdname ecosystem_sim
#'
#' @importFrom utils read.csv read.table
#' @importFrom sf st_transform st_crs st_coordinates st_bbox st_intersects st_centroid st_geometry st_sample
init_ecosystem_sim <- function(ecosystem = "isle_royale",
                               year = 1980, 
                               n_hosts = NULL, 
                               n_predators = NULL, 
                               hex_diameter = 0.01,
                               datafile = "",
                               features_rds = NULL,
                               landmarks_rds = NULL) {
  
  # Load historical benchmark time series data if available for this ecosystem
  csv_path <- system.file(file.path("doc", ecosystem, "wolf_moose.csv"), package = "ewing")
  if (csv_path == "" || !file.exists(csv_path)) {
    csv_path <- file.path("inst", "doc", ecosystem, "wolf_moose.csv")
  }
  
  hist_data <- NULL
  if (file.exists(csv_path)) {
    hist_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  }
  
  # Lookup baseline population counts for start year if not explicitly provided
  if (!is.null(hist_data) && year %in% hist_data$Year) {
    row_match <- hist_data[hist_data$Year == year, ]
    if (is.null(n_hosts) && "Moose" %in% names(row_match)) n_hosts <- as.numeric(row_match$Moose[1])
    if (is.null(n_predators) && "Wolves" %in% names(row_match)) n_predators <- as.numeric(row_match$Wolves[1])
  }
  
  if (is.null(n_hosts)) n_hosts <- 664
  if (is.null(n_predators)) n_predators <- 50
  
  # 1. Initialize Base Spatial Geography & Habitat Overlay for Target Ecosystem
  hex_obj <- create_isle_royale_hex_overlay(hex_diameter = hex_diameter, features = features_rds, site = ecosystem)
  habitat_overlay <- add_habitat_hex_overlay(hex_obj, features = features_rds, landmarks = landmarks_rds, site = ecosystem)
  
  # 2. Setup Configuration Data Directory
  if (datafile == "") {
    pkg_dir <- system.file(file.path("extdata", ecosystem), package = "ewing")
    if (pkg_dir != "" && dir.exists(pkg_dir)) {
      datafile <- pkg_dir
    } else if (dir.exists(file.path("inst", "extdata", ecosystem))) {
      datafile <- file.path("inst", "extdata", ecosystem)
    }
  }
  
  # 3. Initialize ewing Community Core
  community <- NULL
  tryCatch({
    community <- init.simulation(package = "ewing", count = c(n_hosts, n_predators), datafile = datafile, messages = FALSE)
  }, error = function(e) {
    community <<- list(pop = list())
  })
  
  # 4. Spatially Sample Initial Positions Weighted by Substrate Habitat Score
  hex_sf <- habitat_overlay$hex_habitat_sf
  probs <- hex_sf$habitat_score / sum(hex_sf$habitat_score)
  
  sampled_moose_hex <- sample(seq_len(nrow(hex_sf)), size = n_hosts, replace = TRUE, prob = probs)
  sampled_wolf_hex  <- sample(seq_len(nrow(hex_sf)), size = n_predators, replace = TRUE, prob = probs)
  
  centroids <- sf::st_centroid(sf::st_geometry(hex_sf))
  moose_pts <- centroids[sampled_moose_hex]
  wolf_pts  <- centroids[sampled_wolf_hex]
  
  moose_coords <- sf::st_coordinates(moose_pts)
  wolf_coords  <- sf::st_coordinates(wolf_pts)
  
  moose_df <- data.frame(
    id = paste0("M", seq_len(n_hosts)),
    species = "Moose",
    ageclass = sample(c("calf", "yearling", "adult", "senior"), size = n_hosts, replace = TRUE, prob = c(0.15, 0.15, 0.55, 0.15)),
    hex_id = sampled_moose_hex,
    lon = moose_coords[, 1],
    lat = moose_coords[, 2],
    stringsAsFactors = FALSE
  )
  
  wolf_df <- data.frame(
    id = paste0("W", seq_len(n_predators)),
    species = "Wolf",
    ageclass = sample(c("pup", "subadult", "adult"), size = n_predators, replace = TRUE, prob = c(0.20, 0.25, 0.55)),
    hex_id = sampled_wolf_hex,
    lon = wolf_coords[, 1],
    lat = wolf_coords[, 2],
    stringsAsFactors = FALSE
  )
  
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
  
  ds_list <- list()
  if (exists("isle_royale_datasets") && is.list(isle_royale_datasets)) {
    ds_list <- isle_royale_datasets
  } else if (is.character(datafile) && datafile != "" && dir.exists(datafile)) {
    txt_files <- list.files(datafile, pattern = "\\.(txt|csv)$", full.names = TRUE)
    for (f in txt_files) {
      tbl_name <- tools::file_path_sans_ext(basename(f))
      df_f <- tryCatch({
        if (endsWith(f, ".csv")) utils::read.csv(f, stringsAsFactors = FALSE)
        else utils::read.table(f, header = TRUE, sep = "\t", fill = TRUE,
                               check.names = FALSE, stringsAsFactors = FALSE)
      }, error = function(e) NULL)
      if (!is.null(df_f)) ds_list[[tbl_name]] <- df_f
    }
  }
  
  res <- list(
    community = community,
    habitat_overlay = habitat_overlay,
    start_year = year,
    ecosystem = ecosystem,
    moose_pop = moose_df,
    wolf_pop = wolf_df,
    historical_data = hist_data,
    history = hist_df,
    datafile = datafile,
    datasets = ds_list,
    nstep = 0
  )
  
  class(res) <- c(paste0(ecosystem, "_sim"), "ecosystem_sim", "ewing")
  return(res)
}

#' Run Ecosystem Spatial Simulation
#'
#' @param sim_obj An object of class `ecosystem_sim` or derived species simulation object.
#' @param nstep Number of simulation steps to run (default: 1000).
#' @param refresh Refresh step interval (default: 10).
#' @param ... Additional arguments.
#'
#' @return Updated simulation object.
#' @export
#' @rdname ecosystem_sim
run_ecosystem_sim <- function(sim_obj, nstep = 1000, refresh = 10, ...) {
  if (!inherits(sim_obj, "ecosystem_sim") && !inherits(sim_obj, "isle_royale_sim")) {
    stop("Input must be an object of class 'ecosystem_sim' or 'isle_royale_sim'.")
  }
  
  hex_sf <- sim_obj$habitat_overlay$hex_habitat_sf
  n_hex <- nrow(hex_sf)
  centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(hex_sf)))
  
  move_pop <- function(pop_df, move_prob = 0.5) {
    if (is.null(pop_df) || nrow(pop_df) == 0) return(pop_df)
    n <- nrow(pop_df)
    movers <- which(stats::runif(n) < move_prob)
    if (length(movers) > 0) {
      new_hex <- sample(seq_len(n_hex), size = length(movers), replace = TRUE, prob = hex_sf$habitat_score / sum(hex_sf$habitat_score))
      pop_df$hex_id[movers] <- new_hex
      pop_df$lon[movers] <- centroids[new_hex, 1]
      pop_df$lat[movers] <- centroids[new_hex, 2]
    }
    return(pop_df)
  }
  
  update_demographics <- function(moose_df, wolf_df) {
    if (!is.null(moose_df) && nrow(moose_df) > 0) {
      n_m <- nrow(moose_df)
      calf_idx <- which(moose_df$ageclass == "calf")
      if (length(calf_idx) > 0) {
        trans <- calf_idx[stats::runif(length(calf_idx)) < 0.002]
        if (length(trans) > 0) moose_df$ageclass[trans] <- "yearling"
      }
      yearling_idx <- which(moose_df$ageclass == "yearling")
      if (length(yearling_idx) > 0) {
        trans <- yearling_idx[stats::runif(length(yearling_idx)) < 0.002]
        if (length(trans) > 0) moose_df$ageclass[trans] <- "adult"
      }
      adult_idx <- which(moose_df$ageclass == "adult")
      if (length(adult_idx) > 0) {
        trans <- adult_idx[stats::runif(length(adult_idx)) < 0.0003]
        if (length(trans) > 0) moose_df$ageclass[trans] <- "senior"
        
        n_births <- sum(stats::runif(length(adult_idx)) < 0.0012)
        if (n_births > 0) {
          parent_hexes <- sample(moose_df$hex_id[adult_idx], n_births, replace = TRUE)
          max_id <- suppressWarnings(max(as.numeric(gsub("[^0-9]", "", moose_df$id)), na.rm = TRUE))
          if (!is.finite(max_id)) max_id <- nrow(moose_df)
          new_ids <- paste0("M", seq(max_id + 1, length.out = n_births))
          new_calves <- data.frame(
            id = new_ids,
            species = "Moose",
            ageclass = "calf",
            hex_id = parent_hexes,
            lon = centroids[parent_hexes, 1],
            lat = centroids[parent_hexes, 2],
            stringsAsFactors = FALSE
          )
          moose_df <- rbind(moose_df, new_calves)
        }
      }
      senior_idx <- which(moose_df$ageclass == "senior")
      if (length(senior_idx) > 0) {
        deaths <- senior_idx[stats::runif(length(senior_idx)) < 0.0008]
        if (length(deaths) > 0) moose_df <- moose_df[-deaths, ]
      }
    }
    
    if (!is.null(wolf_df) && nrow(wolf_df) > 0) {
      pup_idx <- which(wolf_df$ageclass == "pup")
      if (length(pup_idx) > 0) {
        trans <- pup_idx[stats::runif(length(pup_idx)) < 0.002]
        if (length(trans) > 0) wolf_df$ageclass[trans] <- "subadult"
      }
      subadult_idx <- which(wolf_df$ageclass == "subadult")
      if (length(subadult_idx) > 0) {
        trans <- subadult_idx[stats::runif(length(subadult_idx)) < 0.002]
        if (length(trans) > 0) wolf_df$ageclass[trans] <- "adult"
      }
      adult_w_idx <- which(wolf_df$ageclass == "adult")
      if (length(adult_w_idx) > 0) {
        n_w_births <- sum(stats::runif(length(adult_w_idx)) < 0.0008)
        if (n_w_births > 0) {
          p_hexes <- sample(wolf_df$hex_id[adult_w_idx], n_w_births, replace = TRUE)
          max_wid <- suppressWarnings(max(as.numeric(gsub("[^0-9]", "", wolf_df$id)), na.rm = TRUE))
          if (!is.finite(max_wid)) max_wid <- nrow(wolf_df)
          new_wids <- paste0("W", seq(max_wid + 1, length.out = n_w_births))
          new_pups <- data.frame(
            id = new_wids,
            species = "Wolf",
            ageclass = "pup",
            hex_id = p_hexes,
            lon = centroids[p_hexes, 1],
            lat = centroids[p_hexes, 2],
            stringsAsFactors = FALSE
          )
          wolf_df <- rbind(wolf_df, new_pups)
        }
        w_deaths <- adult_w_idx[stats::runif(length(adult_w_idx)) < 0.0005]
        if (length(w_deaths) > 0) wolf_df <- wolf_df[-w_deaths, ]
      }
    }
    
    list(moose = moose_df, wolf = wolf_df)
  }
  
  curr_step <- sim_obj$nstep
  for (s in seq_len(nstep)) {
    curr_step <- curr_step + 1
    sim_obj$moose_pop <- move_pop(sim_obj$moose_pop, move_prob = 0.6)
    sim_obj$wolf_pop  <- move_pop(sim_obj$wolf_pop, move_prob = 0.8)
    
    if (!is.null(sim_obj$wolf_pop) && !is.null(sim_obj$moose_pop) && nrow(sim_obj$wolf_pop) > 0 && nrow(sim_obj$moose_pop) > 0) {
      wolf_hexes <- unique(sim_obj$wolf_pop$hex_id)
      vulnerable_idx <- which(sim_obj$moose_pop$hex_id %in% wolf_hexes & sim_obj$moose_pop$ageclass %in% c("calf", "senior"))
      if (length(vulnerable_idx) > 0) {
        pred_remove <- vulnerable_idx[stats::runif(length(vulnerable_idx)) < 0.0015]
        if (length(pred_remove) > 0) {
          sim_obj$moose_pop <- sim_obj$moose_pop[-pred_remove, ]
        }
      }
    }
    
    demog_res <- update_demographics(sim_obj$moose_pop, sim_obj$wolf_pop)
    sim_obj$moose_pop <- demog_res$moose
    sim_obj$wolf_pop  <- demog_res$wolf
    
    m_curr <- table(factor(sim_obj$moose_pop$ageclass, levels = c("calf", "yearling", "adult", "senior")))
    w_curr <- table(factor(sim_obj$wolf_pop$ageclass, levels = c("pup", "subadult", "adult")))
    
    step_df <- data.frame(
      step = curr_step,
      time = curr_step,
      Species = c(rep("moose", 4), rep("wolf", 3)),
      State = c(names(m_curr), names(w_curr)),
      Type = "ageclass",
      Count = c(as.numeric(m_curr), as.numeric(w_curr)),
      stringsAsFactors = FALSE
    )
    sim_obj$history <- rbind(sim_obj$history, step_df)
  }
  
  sim_obj$nstep <- curr_step
  return(sim_obj)
}

#' Plot Ecosystem Simulation
#'
#' @param x An object of class `ecosystem_sim`.
#' @param ... Additional plot options.
#'
#' @return A `ggplot` visualization.
#' @export
#' @rdname ecosystem_sim
ggplot_ecosystem_sim <- function(x, ...) {
  if (!inherits(x, "ecosystem_sim") && !inherits(x, "isle_royale_sim")) {
    stop("Input must be an object of class 'ecosystem_sim' or 'isle_royale_sim'.")
  }
  
  p_map <- autoplot(x$habitat_overlay, ...)
  
  if (!is.null(x$moose_pop) && nrow(x$moose_pop) > 0) {
    moose_sf <- sf::st_as_sf(x$moose_pop, coords = c("lon", "lat"), crs = sf::st_crs(x$habitat_overlay$layer))
    p_map <- p_map +
      ggplot2::geom_sf(data = moose_sf, color = "#27ae60", shape = 21, fill = NA, stroke = 1.0, size = 0.8, alpha = 0.85) +
      ggplot2::geom_sf(data = moose_sf, color = "#27ae60", alpha = 0.4, size = 0.5)
  }
  
  if (!is.null(x$wolf_pop) && nrow(x$wolf_pop) > 0) {
    wolf_sf <- sf::st_as_sf(x$wolf_pop, coords = c("lon", "lat"), crs = sf::st_crs(x$habitat_overlay$layer))
    p_map <- p_map +
      ggplot2::geom_sf(data = wolf_sf, color = "#e74c3c", shape = 24, fill = "#e74c3c", size = 1.2, alpha = 0.9)
  }
  
  eco_title <- if (!is.null(x$ecosystem)) paste0(toupper(substr(x$ecosystem, 1, 1)), substring(x$ecosystem, 2)) else "Ecosystem"
  p_map <- p_map + ggplot2::ggtitle(paste(eco_title, "Simulation (Step", x$nstep, ")"))
  
  if (!is.null(x$historical_data)) {
    df_hist <- x$historical_data
    p_hist <- ggplot2::ggplot(df_hist, ggplot2::aes(x = .data$Year)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$Moose, color = "Historical Moose"), linewidth = 1.0) +
      ggplot2::geom_line(ggplot2::aes(y = .data$Wolves * 40, color = "Historical Wolves (x40)"), linewidth = 1.0, linetype = "dashed") +
      ggplot2::scale_color_manual(
        name = "Empirical Benchmarks",
        values = c("Historical Moose" = "#27ae60", "Historical Wolves (x40)" = "#e74c3c")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste(eco_title, "Census Trajectories"),
        x = "Year", y = "Moose Abundance (Wolves x40)"
      )
    
    if (requireNamespace("cowplot", quietly = TRUE)) {
      return(cowplot::plot_grid(p_map, p_hist, ncol = 1, rel_heights = c(1.2, 1)))
    }
  }
  
  return(p_map)
}

#' @export
plot.ecosystem_sim <- function(x, ...) {
  print(ggplot_ecosystem_sim(x, ...))
}

#' Resolve Cached Spatial Site Datasets
#'
#' Resolves file paths for pre-computed spatial GIS datasets (`.rds`) for a target simulation site
#' or landscape (e.g. `"isle_royale"`), checking installed package directories (`extdata/[site]`)
#' and local development source trees (`inst/extdata/[site]`).
#'
#' @param filename File name string (e.g. `"isle_royale_layer.rds"`).
#' @param site Target site/landscape directory name under `extdata/` (default: `"isle_royale"`).
#'
#' @return Path to target cached dataset file.
#' @export
#' @name get_site_cache_file
#' @rdname get_site_cache_file
get_site_cache_file <- function(filename, site = "isle_royale") {
  pkg_dir <- system.file(file.path("extdata", site), package = "ewing")
  if (pkg_dir != "") {
    fp <- if (filename == "") pkg_dir else file.path(pkg_dir, filename)
    if (file.exists(fp) || dir.exists(fp)) return(fp)
  }
  dev_fp <- if (filename == "") file.path("inst", "extdata", site) else file.path("inst", "extdata", site, filename)
  if (file.exists(dev_fp) || dir.exists(dev_fp)) return(dev_fp)
  if (pkg_dir != "") file.path(pkg_dir, filename) else dev_fp
}

#' @export
#' @rdname get_site_cache_file
get_isle_royale_cache_file <- function(filename) {
  get_site_cache_file(filename, site = "isle_royale")
}

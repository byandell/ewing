#' Isle Royale Wolf-Moose Predator-Prey Simulation (Wrapper Shell)
#'
#' Backward-compatible wrapper for initializing, running, and visualizing the Isle Royale
#' wolf-moose spatial predator-prey model using the generalized `ecosystem_sim` engine.
#'
#' @param year Target baseline year from `wolf_moose.csv` (1980-2019, default: 1980).
#' @param n_moose Initial number of moose individuals (default: looked up from `wolf_moose.csv`).
#' @param n_wolves Initial number of wolf individuals (default: looked up from `wolf_moose.csv`).
#' @param hex_diameter Diameter of hexagonal substrate mesh (default: 0.01 degrees).
#' @param datafile Optional custom datafile directory path.
#' @param features_rds Optional path or `sf` object for custom site habitat features.
#' @param landmarks_rds Optional path or `sf` object for custom site landmarks.
#'
#' @return `init_isle_royale_sim`: An S3 object of class `c("isle_royale_sim", "ecosystem_sim", "ewing")`.
#' @export
#' @name isle_royale_sim
#' @rdname isle_royale_sim
init_isle_royale_sim <- function(year = 1980, 
                                n_moose = NULL, 
                                n_wolves = NULL, 
                                hex_diameter = 0.01,
                                datafile = "",
                                features_rds = NULL,
                                landmarks_rds = NULL) {
  init_ecosystem_sim(
    ecosystem = "isle_royale",
    year = year,
    n_hosts = n_moose,
    n_predators = n_wolves,
    hex_diameter = hex_diameter,
    datafile = datafile,
    features_rds = features_rds,
    landmarks_rds = landmarks_rds
  )
}

#' Run Isle Royale Wolf-Moose Simulation
#'
#' @param sim_obj An object of class `isle_royale_sim` or `ecosystem_sim`.
#' @param nstep Number of simulation steps to run (default: 1000).
#' @param refresh Refresh step interval (default: 10).
#' @param ... Additional options.
#'
#' @return Updated simulation object.
#' @export
#' @rdname isle_royale_sim
run_isle_royale_sim <- function(sim_obj, nstep = 1000, refresh = 10, ...) {
  run_ecosystem_sim(sim_obj = sim_obj, nstep = nstep, refresh = refresh, ...)
}

#' Plot Isle Royale Simulation
#'
#' @param x An object of class `isle_royale_sim`.
#' @param ... Additional plot options.
#'
#' @return A `ggplot` visualization.
#' @export
#' @rdname isle_royale_sim
ggplot_isle_royale_sim <- function(x, ...) {
  ggplot_ecosystem_sim(x, ...)
}

#' @export
plot.isle_royale_sim <- function(x, ...) {
  print(ggplot_isle_royale_sim(x, ...))
}

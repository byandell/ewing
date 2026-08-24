#' Default Choices for Input Datasets
#'
#' Discovers or returns default dataset table names from a given ecosystem subfolder under `extdata/`
#' or a custom directory path.
#'
#' @param ecosystem Character name of ecosystem directory under `extdata/` (e.g. `"default"`, `"isle_royale"`)
#'   or an explicit folder path. Defaults to `"default"`.
#' @return Character vector of dataset table names with `"organism.features"` as the first element if present.
#' @export
#' @rdname inputApp
default_choices <- function(ecosystem = "default") {
  pkg_dir <- ""
  
  if (is.character(ecosystem) && length(ecosystem) == 1 && nzchar(ecosystem)) {
    if (dir.exists(ecosystem)) {
      pkg_dir <- ecosystem
    } else {
      pkg_dir <- system.file(file.path("extdata", ecosystem), package = "ewing")
      if (pkg_dir == "" || !dir.exists(pkg_dir)) {
        pkg_dir <- file.path("inst", "extdata", ecosystem)
      }
    }
  }
  
  found <- character(0)
  if (dir.exists(pkg_dir)) {
    files <- list.files(pkg_dir, pattern = "\\.(txt|csv|tsv)$", full.names = FALSE)
    if (length(files) > 0) {
      found <- tools::file_path_sans_ext(files)
    }
  }
  
  if (length(found) == 0) {
    if (identical(ecosystem, "isle_royale")) {
      found <- c(
        "organism.features", "future.moose", "future.wolf",
        "moose.wolf", "substrate.moose", "substrate.wolf",
        "substrate.substrate"
      )
    } else {
      found <- c(
        "organism.features", "future.host", "future.parasite",
        "host.parasite", "substrate.host", "substrate.parasite",
        "substrate.substrate", "temperature.base", "temperature.par"
      )
    }
  }
  
  if ("organism.features" %in% found) {
    found <- c("organism.features", setdiff(found, "organism.features"))
  }
  found
}

#' @export
#' @rdname inputApp
default_choice <- function(ecosystem = "default") {
  default_choices(ecosystem = ecosystem)
}

#' Discover Available Ecosystems
#'
#' Scans the package `extdata/` directory and optional extra directories for available ecosystem folders.
#'
#' @param extra_dirs Optional character vector of extra directory paths to scan.
#' @return Character vector of available ecosystem directory names.
#' @export
#' @rdname inputApp
available_ecosystems <- function(extra_dirs = character(0)) {
  pkg_dir <- system.file("extdata", package = "ewing")
  if (pkg_dir == "" || !dir.exists(pkg_dir)) {
    pkg_dir <- file.path("inst", "extdata")
  }
  
  dirs <- character(0)
  if (dir.exists(pkg_dir)) {
    dirs <- list.dirs(pkg_dir, full.names = FALSE, recursive = FALSE)
  }
  
  if (length(extra_dirs) > 0) {
    for (ed in extra_dirs) {
      if (dir.exists(ed)) {
        sub_dirs <- list.dirs(ed, full.names = FALSE, recursive = FALSE)
        dirs <- c(dirs, sub_dirs, ed)
      }
    }
  }
  
  dirs <- unique(dirs[nzchar(dirs)])
  if (length(dirs) == 0) {
    dirs <- c("default", "isle_royale")
  }
  
  # Ensure "default" is first if present
  if ("default" %in% dirs) {
    dirs <- c("default", setdiff(dirs, "default"))
  }
  
  dirs
}

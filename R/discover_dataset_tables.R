#' Helper to dynamically discover available dataset tables from an input folder or simulation instance
#' @param datafile Character path to folder or Excel file containing input tables
#' @param sim Simulation instance (`ewing` object)
#' @param ecosystem Target ecosystem name (e.g. `"default"`, `"isle_royale"`) or directory path. Defaults to `"default"`.
#' @export
#' @rdname inputApp
discover_dataset_tables <- function(datafile = "", sim = NULL, ecosystem = "default") {
  found <- character(0)
  
  eco <- if (!is.null(sim) && !is.null(sim$ecosystem) && is.character(sim$ecosystem) && nzchar(sim$ecosystem)) {
    sim$ecosystem
  } else {
    ecosystem
  }
  
  # 1. Inspect datafile directory
  d_path <- if (is.character(datafile) && datafile != "") {
    datafile
  } else if (!is.null(sim) && !is.null(sim$datafile) && is.character(sim$datafile) && datafile != "") {
    sim$datafile
  } else ""
  
  if (d_path != "" && file.exists(d_path)) {
    if (dir.exists(d_path)) {
      files <- list.files(d_path, pattern = "\\.(txt|csv|tsv)$", full.names = FALSE)
      if (length(files) > 0) {
        found <- tools::file_path_sans_ext(files)
      }
    } else if (grepl("\\.xlsx$", d_path, ignore.case = TRUE)) {
      sheets <- tryCatch(readxl::excel_sheets(d_path), error = function(e) character(0))
      if (length(sheets) > 0) found <- sheets
    }
  }
  
  # 2. Inspect sim$datasets if present
  if (!is.null(sim) && !is.null(sim$datasets)) {
    found <- unique(c(found, names(sim$datasets)))
  }
  if (!is.null(sim) && !is.null(sim$community) && !is.null(sim$community$datasets)) {
    found <- unique(c(found, names(sim$community$datasets)))
  }
  
  # Filter out any .rds spatial layers or non-table objects
  found <- found[!grepl("\\.rds$", found, ignore.case = TRUE)]
  
  # 3. Default fallback choices if nothing found
  if (length(found) == 0) {
    found <- default_choices(eco)
  } else {
    if ("organism.features" %in% found) {
      found <- c("organism.features", setdiff(found, "organism.features"))
    }
  }
  
  unique(found)
}

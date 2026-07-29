# Helper to render standalone Shinylive WebAssembly apps cleanly without webr::install()

render_standalone_app <- function(app_name, height = 800) {
  cat("```{shinylive-r}\n")
  cat("#| standalone: true\n")
  cat(paste0("#| viewerHeight: ", height, "\n"))
  cat("#| components: [viewer]\n\n")
  
  # Standard webR pre-installed libraries
  cat("library(shiny)\n")
  cat("library(bslib)\n")
  if (app_name %in% c("triangleApp", "hexmoveApp", "sysetholApp")) {
    cat("library(ggplot2)\n")
  }
  if (app_name %in% c("fivePlotApp", "fiveShowApp", "tempApp", "sysetholApp")) {
    cat("library(splines)\n")
  }
  cat("library(stats)\n")
  cat("library(graphics)\n\n")
  
  # Auto-include default data tables for apps that require simulation datasets
  if (app_name %in% c("sysetholApp", "hexmoveApp")) {
    data_files <- list.files(file.path("..", "data"), pattern = "\\.txt$", full.names = TRUE)
    for (f in data_files) {
      tbl_name <- sub("\\.txt$", "", basename(f))
      df <- tryCatch(read.table(f, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(df)) {
        cat(paste0("# --- Auto-Included Data Table: ", tbl_name, " ---\n"))
        cat(paste0(tbl_name, " <- "))
        dput(df)
        cat("\n\n")
      }
    }
  }
  
  # Map apps to their required lightweight R source files in ../R/
  app_files <- switch(app_name,
    fivePlotApp = c("spline.R", "five.R", "fivePlotApp.R"),
    fiveShowApp = c("spline.R", "five.R", "fiveShowApp.R"),
    triangleApp = c("triangle.R", "substrate_triangle.R", "triangleApp.R"),
    tempApp     = c("spline.R", "temp.R", "initTemp.R", "temp.design.R", "temp.plot.R", "tempApp.R"),
    hexmoveApp  = c("triangle.R", "substrate_triangle.R", "ewing_substrate.R", "hexmoveApp.R"),
    sysetholApp = c(
      "spline.R", "triangle.R", "substrate_triangle.R", "ewing_substrate.R",
      "ewing_ageclass.R", "temp.R", "initTemp.R", "temp.design.R", "temp.plot.R",
      "my.R", "Org.R", "organism.features.R", "future.host.R", "substrate.host.R",
      "host.parasite.R", "simdata.R", "redscale.R", "init.simulation.R", "future.events.R",
      "ewing_discrete.R", "ewing_envelopes.R", "ggplot_ewing.R", "ggplot_ewing_envelopes.R",
      "ggplot_current.R", "distPlotApp.R", "multApp.R", "inputApp.R", "origEwingApp.R", "sysetholApp.R"
    )
  )
  
  # Read and output each required R source file (stripping roxygen lines)
  for (f in app_files) {
    fp <- file.path("..", "R", f)
    if (file.exists(fp)) {
      lines <- readLines(fp, warn = FALSE)
      lines <- lines[!grepl("^\\s*#'", lines)]
      cat(paste0("# --- Source: ", f, " ---\n"))
      cat(paste(lines, collapse = "\n"))
      cat("\n\n")
    }
  }
  
  cat("# --- Launch Application ---\n")
  cat(paste0(app_name, "()\n"))
  cat("```\n")
}

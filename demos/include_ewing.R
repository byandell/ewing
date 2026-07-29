# Helper to render ewing Shiny apps cleanly in Quarto Shinylive WebAssembly

render_shinylive_app <- function(app_call, height = 800) {
  cat("```{shinylive-r}\n")
  cat("#| standalone: true\n")
  cat(paste0("#| viewerHeight: ", height, "\n"))
  cat("#| components: [viewer]\n\n")
  
  cat("library(shiny)\n")
  cat("library(bslib)\n")
  cat("library(ggplot2)\n")
  cat("library(cowplot)\n")
  cat("library(stats)\n")
  cat("library(splines)\n")
  cat("library(graphics)\n\n")
  
  r_files <- list.files(file.path("..", "R"), pattern = "\\.R$", full.names = TRUE)
  for (fp in r_files) {
    if (file.exists(fp)) {
      lines <- readLines(fp, warn = FALSE)
      lines <- lines[!grepl("^\\s*#'", lines)]
      cat(paste0("# --- Auto-Included: ", basename(fp), " ---\n\n"))
      cat(paste(lines, collapse = "\n"))
      cat("\n\n")
    }
  }
  
  cat("# --- Launch Application ---\n\n")
  cat(paste0(app_call, "\n"))
  cat("```\n")
}

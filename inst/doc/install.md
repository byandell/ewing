
All Shinylive WebAssembly demonstration applications and documentation have been refactored to the targeted **`demos/shinylive_helpers.R`** architecture.

### Summary of Architecture & Design

1. **Targeted Quarto Shinylive Demo Applications (`demos/*.qmd`)**:
   All 6 Shinylive demo documents ([sysetholApp.qmd](../demos/sysetholApp.qmd), [fivePlotApp.qmd](../demos/fivePlotApp.qmd), [fiveShowApp.qmd](../demos/fiveShowApp.qmd), [tempApp.qmd](../demos/tempApp.qmd), [triangleApp.qmd](../demos/triangleApp.qmd), [hexmoveApp.qmd](../demos/hexmoveApp.qmd)) use `render_standalone_app()` to dynamically bundle app-specific R code blocks:

   ```r
   ```{r, echo=FALSE, results='asis'}
   source("shinylive_helpers.R")
   render_standalone_app("sysetholApp", height = 880)
   ```
   ```

2. **Roxygen2 Protection & Subsetting Safety**:
   Strips all `#'` roxygen docstrings before outputting code blocks (`lines[!grepl("^\\s*#'", lines)]`) to prevent Pandoc JSON string serialization errors, following R vector subsetting safety (`!grepl`).

3. **Auto-Embedded Default Data Tables**:
   Default configuration datasets from `data/*.txt` (`organism.features`, `future.host`, `future.parasite`, `substrate.*`, `temperature.*`, `host.parasite`, `redscale`) are serialized via `dput()` directly into `.GlobalEnv` for simulation apps (`sysetholApp`, `hexmoveApp`).

4. **Zero Wasm Package Downloads**:
   Eliminates `webr::install()` network delays and heavy C++/Wasm GIS dependencies (`sf`, `leaflet`, `readxl`). Applications use only standard pre-installed webR packages (`shiny`, `bslib`, `ggplot2`, `splines`, `stats`, `graphics`).

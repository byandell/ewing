# Developer Guide (DEVELOPER.md)

This guide provides technical reference documentation for developers
extending the **ewing** Quantative Population Ethology (QPE) simulation
package, building new simulation site prototypes, and composing Shiny
applications.

------------------------------------------------------------------------

## 1. Package Architecture Overview

The `ewing` package is structured into core simulation engines, spatial
substrate representation, S3 visualization classes, and a modular Shiny
application layer.

    ewing/
    ├── R/                          # R source functions (60+ files)
    │   ├── step_controls.R         # Logarithmic step slider, axis units (Steps vs Time/Days), age class controls
    │   ├── distPlotApp.R           # Population age-class distribution Shiny module
    │   ├── inputApp.R              # Tabular dataset discovery & viewer module (excluding .rds layers)
    │   ├── substrateApp.R          # Substrate network rendering Shiny module
    │   ├── ecosystem_sim.R         # Generalized multi-landscape simulation engine & get_site_cache_file()
    │   ├── ecosystemApp.R          # Generalized multi-landscape Shiny app platform
    │   ├── isle_royale_sim.R       # Isle Royale backward-compatible wrapper shell
    │   ├── IsleRoyaleApp.R         # Isle Royale Shiny app backward-compatible wrapper shell
    │   ├── sysetholApp.R           # Systems Ethology peer application wrapper
    │   ├── ewing_ageclass.R        # Age class S3 tallying & cowplot multi-panel visualization
    │   └── ewing_substrate.R       # Spatial GIS substrate rendering S3 methods
    ├── inst/
    │   ├── extdata/
    │   │   ├── default/            # Core default simulation organism tables (organism.features.txt, etc.)
    │   │   ├── isle_royale/        # Isle Royale prototype site dataset tables & spatial RDS files
    │   │   └── watershed/          # HUC-12 watershed spatial features CSV
    │   ├── doc/isle_royale/        # Isle Royale ecological case study documentation & census CSV
    │   └── scripts/                # Utility scripts (isle_royale_habitat.R, watershedApp.R)
    ├── vignettes/
    │   ├── devel_guide/            # Developer Guide vignettes (architecture, simulation, visualization)
    │   └── tech_guide/             # Technical Guide vignettes (engine, geometry, gis, ui)
    └── DEVELOPER.md                # High-level developer documentation

------------------------------------------------------------------------

## 2. Multi-Landscape Site Prototyping Architecture

[`init_ecosystem_sim()`](https://byandell.github.io/ewing/reference/ecosystem_sim.md)
and
[`ecosystemApp()`](https://byandell.github.io/ewing/reference/ecosystemApp.md)
provide a generalized framework for building GIS-based simulation sites.
Isle Royale serves as the primary prototype template
(`ecosystem = "isle_royale"`).

### 1. Spatial & Tabular Asset Placement

To instantiate a new simulation site (e.g. `ecosystem = "yellowstone"`
or `ecosystem = "madeline_island"`), place site assets in
`inst/extdata/[ecosystem]/`: - **Organism & Transition Tables**:
`organism.features.txt`, `future.[species1].txt`,
`future.[species2].txt`, `[species1].[species2].txt`,
`substrate.[species].txt`. - **Spatial GIS Boundary Layers**:
`[ecosystem]_layer.rds` (boundary outline), `[ecosystem]_features.rds`
(habitat polygons), `[ecosystem]_landmarks.rds` (sighting POIs).

### 2. Multi-Landscape Site Resolver (`get_site_cache_file`)

File and folder paths under `extdata/[site]` are resolved dynamically
across installed package paths (`library/ewing/extdata/[site]`) and
development source trees (`inst/extdata/[site]`):

``` r

# Resolve file or directory path for any site
data_dir <- get_site_cache_file("", site = "yellowstone")
layer_file <- get_site_cache_file("yellowstone_layer.rds", site = "yellowstone")
```

### 3. Simulation & App Execution

``` r

# Run simulation for custom ecosystem
sim <- init_ecosystem_sim(ecosystem = "yellowstone", n_hosts = 400, n_predators = 25)
sim <- run_ecosystem_sim(sim, nstep = 200)

# Launch interactive Shiny app
ecosystemApp(ecosystem = "yellowstone", title = "Yellowstone Simulation Platform")
```

------------------------------------------------------------------------

## 3. Shared Shiny Module Standards & UX Layout

`IsleRoyaleApp.R` and `sysetholApp.R` delegate to `ecosystemApp.R` and
`sysetholApp.R` using standard modular components:

1.  **`step_controls`
    ([R/step_controls.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/step_controls.R))**:
    - [`step_size_slider()`](https://byandell.github.io/ewing/reference/step_controls.md):
      Logarithmic step slider (1, 2, 5, 10, 20, 50, 100, 200, 500, 1000,
      2000).
    - [`axisUnitInput()`](https://byandell.github.io/ewing/reference/step_controls.md)
      /
      [`axisUnitServer()`](https://byandell.github.io/ewing/reference/step_controls.md):
      Display units selector (`Steps` vs `Time` / `Days`).
    - [`ageClassControlInput()`](https://byandell.github.io/ewing/reference/step_controls.md)
      /
      [`ageClassControlServer()`](https://byandell.github.io/ewing/reference/step_controls.md):
      Age class display controls (`x_var`, `norm`, `total`).
2.  **`inputApp`
    ([R/inputApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/inputApp.R))**:
    - [`discover_dataset_tables()`](https://byandell.github.io/ewing/reference/inputApp.md):
      Dynamically scans input folders (`.txt`, `.csv`, `.tsv`),
      explicitly excluding spatial `.rds` layers so `inputApp` shows
      only clean data tables.
3.  **Streamlined Sidebar Layout Standards**:
    - Top-positioned `Steps per click:` slider when `nsim = 1`.
    - Single-row flex container for **Run** and **Reset** action buttons
      (`display: flex; gap: 8px;`).
    - Bottom-positioned historical baseline year selector.
    - Compact CSS form group compression (`margin-bottom: 4px;`,
      `.irs-with-grid { height: 34px; }`) ensuring sidebar height
      matches main display panels cleanly without vertical scrolling.

------------------------------------------------------------------------

## 4. Development & Testing Guidelines

- **Empirical Local Verification**: Always verify edits by running
  `devtools::document()` and local test scripts.
- **R Vector Subsetting Safety**: Use `grepl("^\\s*#'", lines)` with
  `!grepl(...)` or `grep(..., invert = TRUE)`. Never evaluate
  `!grep(...)`.
- **No Automatic Git Commit/Push**: Leave git staging, committing, and
  pushing for manual user execution.

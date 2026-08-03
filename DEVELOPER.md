# Developer Guide (`DEVELOPER.md`)

This guide provides technical reference documentation for developers extending the **ewing** Quantative Population Ethology (QPE) simulation package, building new simulation site prototypes, and composing Shiny applications.

---

## 1. Package Architecture Overview

The `ewing` package is structured into core simulation engines, spatial substrate representation, S3 visualization classes, and a modular Shiny application layer.

```
ewing/
├── R/                          # R source functions (60+ files)
│   ├── step_controls.R         # Logarithmic step slider, axis units (Steps vs Time/Days), age class controls
│   ├── distPlotApp.R           # Population age-class distribution Shiny module
│   ├── inputApp.R              # Dataset table discovery and viewer Shiny module
│   ├── substrateApp.R          # Substrate network rendering Shiny module
│   ├── IsleRoyaleApp.R         # Isle Royale peer application wrapper
│   ├── sysetholApp.R           # Systems Ethology peer application wrapper
│   ├── isle_royale_sim.R       # Isle Royale spatial simulation engine & S3 methods
│   ├── ewing_ageclass.R        # Age class S3 tallying & cowplot multi-panel visualization
│   └── ewing_substrate.R       # Spatial GIS substrate rendering S3 methods
├── inst/
│   ├── extdata/isle_royale/    # Isle Royale prototype site dataset tables & spatial RDS files
│   ├── doc/isle_royale/        # Isle Royale ecological case study documentation & census CSV
│   └── scripts/                # Utility scripts (isle_royale_habitat.R, watershedApp.R)
├── vignettes/
│   ├── devel_guide/            # Developer Guide vignettes (architecture, simulation, visualization)
│   └── tech_guide/             # Technical Guide vignettes (engine, geometry, gis, ui)
└── DEVELOPER.md                # High-level developer documentation
```

---

## 2. Site Prototyping Architecture

Isle Royale serves as the prototype template for creating new GIS-based simulation sites. To instantiate a new simulation site (e.g. Madeline Island or custom watersheds):

### 1. Interactive Site Discovery & Feature Export
Use `hexmapApp()` to interactively select a study region via NHD watershed boundaries (HUC-8 / HUC-10 / HUC-12):
- Extracts OpenStreetMap spatial feature polygons/lines (lakes, bogs, forests).
- Geocodes landmark POIs.
- Builds a hexagonal substrate mesh (`add_habitat_hex_overlay`).
- Exports custom site features (`site_features.rds`) and landmarks (`site_landmarks.rds`).

### 2. Custom Site Initializer Parameterization
Pass exported custom RDS files or `sf` objects into `init_isle_royale_sim()` or site initializers:
```r
my_sim <- init_isle_royale_sim(
  year = 2020,
  n_moose = 500,
  n_wolves = 30,
  features_rds = "path/to/site_features.rds",
  landmarks_rds = "path/to/site_landmarks.rds"
)
```

---

## 3. Shared Shiny Sub-Module Architecture

`IsleRoyaleApp.R` and `sysetholApp.R` are peer application wrappers composing shared, modular Shiny components:

1. **`step_controls` ([R/step_controls.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/step_controls.R))**:
   - `step_size_slider()`: Logarithmic step slider (1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000).
   - `parse_step_size()`: Safely converts slider indices and direct step counts.
   - `axisUnitInput()` / `axisUnitServer()`: Display units selector (`Steps` vs `Time` / `Days`).
   - `ageClassControlInput()` / `ageClassControlServer()`: Age class display controls (`x_var`, `norm`, `total`).

2. **`distPlotApp` ([R/distPlotApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/distPlotApp.R))**:
   - Renders age-class population dynamics over simulation steps or time/days.

3. **`inputApp` ([R/inputApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/inputApp.R))**:
   - `discover_dataset_tables()`: Dynamically scans input folders or Excel sheets to populate dataset table selectors with zero hardcoded table names.

4. **Tab-Aware Sidebar Decluttering**:
   - Both peer apps utilize `conditionalPanel` bound to `input.tabset` to display plot-specific controls strictly when their corresponding tab is active.

---

## 4. Development & Testing Guidelines

- **Empirical Local Verification**: Always verify edits by running `devtools::document()` and local test scripts.
- **R Vector Subsetting Safety**: Use `grepl("^\\s*#'", lines)` with `!grepl(...)` or `grep(..., invert = TRUE)`. Never evaluate `!grep(...)`.
- **No Automatic Git Commit/Push**: Leave git staging, committing, and pushing for manual user execution.

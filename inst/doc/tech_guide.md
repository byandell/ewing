# Create Technical Guide for Ewing Package

This document records the user prompts, technical design decisions, mathematical specifications, and results for building and deploying the **ewing Technical Guide** and its accompanying interactive **Shinylive WebAssembly Demos Gallery**.

---

## 1. User Prompts & Objectives

- **Technical Guide Creation**: Create an in-depth internal technical reference manual for the **ewing** package, organized under `vignettes/tech_guide/`, covering deep architectural mechanics, spatial geometry, thermal spline integration, GIS watershed overlay algorithms, and UI module composition.
- **Interactive Shinylive Demos**: Build serverless WebAssembly demonstration documents in `demos/*.qmd` to showcase each core module and interactive algorithm client-side without requiring a server backend.
- **GitHub Pages Deployment & 404 Resolution**: Diagnose and resolve GitHub Pages 404 errors (`Content not found. Please use links in the navbar`) by disabling Jekyll processing via `.nojekyll` in the GitHub Actions workflow (`.github/workflows/pkgdown.yaml`).
- **Navbar Reorganization**: Elevate **Demos** to a top-level navbar tab placed *before* **Guides** in `_pkgdown.yml`, and add a **Main Site** return link in `demos/_quarto.yml` for seamless cross-navigation.

---

## 2. Technical Domain Structure

The Technical Guide is organized into five specialized domain vignettes under `vignettes/tech_guide/`, complemented by serverless Quarto documents in `demos/`:

| Technical Vignette | Domain Scope | Code Base / R Files | Accompanying Shinylive Demo |
| :--- | :--- | :--- | :--- |
| **[index.Rmd](../../vignettes/tech_guide/index.Rmd)** | Overview & Table of Contents | N/A | [demos/index.qmd](../../demos/index.qmd) |
| **[engine.Rmd](../../vignettes/tech_guide/engine.Rmd)** | Simulation Engine & Event Scheduling | `R/community.R`, `R/Org.R`, `R/event.R` | [demos/sysetholApp.qmd](../../demos/sysetholApp.qmd) |
| **[geometry.Rmd](../../vignettes/tech_guide/geometry.Rmd)** | Substrate Geometry & Grid Movement | `R/triangle.R`, `R/substrate_triangle.R`, `R/ewing_substrate.R` | [demos/triangleApp.qmd](../../demos/triangleApp.qmd), [demos/hexmoveApp.qmd](../../demos/hexmoveApp.qmd) |
| **[thermal.Rmd](../../vignettes/tech_guide/thermal.Rmd)** | Thermal Regimes & Spline Sensitivity | `R/temp.R`, `R/spline.R`, `R/five.R` | [demos/tempApp.qmd](../../demos/tempApp.qmd), [demos/fivePlotApp.qmd](../../demos/fivePlotApp.qmd), [demos/fiveShowApp.qmd](../../demos/fiveShowApp.qmd) |
| **[gis.Rmd](../../vignettes/tech_guide/gis.Rmd)** | GIS Watersheds & Spatial Overlay | `R/leaflet.R`, `R/watershed.R`, `inst/scripts/watershed_overlay.R` | [demos/hexmapApp.qmd](../../demos/hexmapApp.qmd) |
| **[ui.Rmd](../../vignettes/tech_guide/ui.Rmd)** | Systems Ethology UI Architecture | `R/sysetholApp.R`, `R/inputApp.R`, `R/downloadApp.R` | [demos/sysetholApp.qmd](../../demos/sysetholApp.qmd) |

---

## 3. Prompts & Results History by Technical Domain

### I. Simulation Engine ([engine.Rmd](../../vignettes/tech_guide/engine.Rmd))
- **Prompts**: Document the leftist-tree event queue scheduling, competing risk stage transitions, matrix state representations (`organism.features`, `future.host`, `future.parasite`), and degree-day event clocks.
- **Results**:
  - Detailed the $O(\log N)$ event insertion/deletion mechanics of the leftist binary tree (`R/Org.R`).
  - Mapped species transition hazard functions and discrete step integration loops (`future.events()`).
  - Integrated full step execution controls (+1, +10, +100 events) into the serverless `sysetholApp.qmd` Shinylive demo.

### II. Substrate Geometry ([geometry.Rmd](../../vignettes/tech_guide/geometry.Rmd))
- **Prompts**: Formalize the triangular coordinate system $(a, b, c)$ satisfying $c = -(a+b)$, grid scaling $[0, W_{sub}]$, 6-sided hexagonal polygon overlays, and organism movement algorithms.
- **Results**:
  - Formulated `tricoord` mathematical properties and coordinate vector operators (`+`, `-`).
  - Specified `create_hex_overlay()` bounding box calculations and triangular tile geometry (`R/substrate_triangle.R`).
  - Built `demos/triangleApp.qmd` and `demos/hexmoveApp.qmd` enabling real-time interactive stepping and species spatial tracking in WebAssembly.

### III. Thermal Regimes ([thermal.Rmd](../../vignettes/tech_guide/thermal.Rmd))
- **Prompts**: Document daily thermal cycle spline interpolation (`temp.design`), degree-day accumulation (`activeTemp`), and 5-parameter mean sensitivity search algorithms (`five.plot`, `five.show`).
- **Results**:
  - Derived spline node movement math and binary search logic for target relative means.
  - Implemented interactive spline knot drag-and-drop in `demos/tempApp.qmd`, `demos/fivePlotApp.qmd`, and `demos/fiveShowApp.qmd`.

### IV. GIS Watershed Overlays ([gis.Rmd](../../vignettes/tech_guide/gis.Rmd))
- **Prompts**: Document geographic subwatershed boundary retrieval (USGS HUC12), spatial projection of simulation triangular substrate grids onto polygon shapefiles, and Leaflet interactive map rendering.
- **Results**:
  - Documented `get_huc_from_point()`, `get_hucs_from_polygon()`, and spatial intersection (`sf::st_intersects`).
  - Published `demos/hexmapApp.qmd` linking static autoplots and live Posit Connect web applications.

### V. UI Architecture ([ui.Rmd](../../vignettes/tech_guide/ui.Rmd))
- **Prompts**: Detail the modular Shiny architecture upgrading `ewingApp()`, standard `webr::install("byandell/ewing")` WebAssembly package loading for Shinylive documents, and download safety wrappers.
- **Results**:
  - Formulated the code-reuse architecture allowing `.qmd` demos to load the installed R package directly via `webr::install()` in client-side WebAssembly.
  - Eliminated build-time string concatenation hacks and manual code duplication between package R functions and WebAssembly Shinylive applications.

---

## 4. Relationship to Demos Gallery Guide

While the Technical Guide focuses on the R vignette documentation (`vignettes/tech_guide/*.Rmd`), the companion interactive applications (`demos/*.qmd`) are documented in detail in **[demo_guide.md](demo_guide.md)**.

Refer to **[demo_guide.md](demo_guide.md)** for:
- Architecture of the Quarto Shinylive WebAssembly Demos Gallery (`demos/_quarto.yml`).
- Standard WebAssembly package installation (`webr::install("byandell/ewing")`) to ensure 100% code parity with package R files.
- GitHub Pages Jekyll 404 resolution (`Content not found. Please use links in the navbar`) via `touch docs/.nojekyll`.
- Top navbar reorganization (`_pkgdown.yml`) placing **Demos** before **Guides**.
- Adding **Main Site** return navigation links (`href: ../index.html`) in Quarto headers.

---

## 5. Verification & Summary

- **Local Compilation**: `Rscript -e "pkgdown::build_site_github_pages()"` and `cd demos && quarto render` compile 100% cleanly into `docs/` and `docs/demos/`.
- **Navigation Verification**: `docs/index.html` displays `Get started | Reference | Demos | Guides`. All demo pages include `Main Site` navbar cross-links.
- **GitHub Pages Site**: Deployed static site at [https://byandell.github.io/ewing/](https://byandell.github.io/ewing/) serves both standard R package vignettes and serverless WebAssembly Shinylive applications without 404 errors.

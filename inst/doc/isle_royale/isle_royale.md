# Isle Royale Wolf-Moose Ecological Case Study Guide

This document provides a technical case-study guide for the **Isle Royale Wolf-Moose Predator-Prey Model** in `ewing`.

- [Isle Royale Wolf-Moose Study (NPS)](https://npshistory.com/series/fauna/7/sec5.htm)
- [Where are the Moose (NPS)](https://www.nps.gov/isro/learn/nature/moose.htm)
- [Population Dynamics (Wolf Center)](https://www.isleroyalewolf.org/population-dynamics)
- [dsem package (James Thorson, NOAA)](https://github.com/James-Thorson-NOAA/dsem)

---

## 1. Ecological Framework & Temporal Scaling

Isle Royale National Park (Lake Superior, Michigan) represents an isolated island predator-prey system tracked continuously since 1958.

### Life Cycle Age Classes
- **Moose (`future.moose`)**: Calves (0–1 yr, highly vulnerable), Yearlings (1–2 yrs), Adults (2–9 yrs, prime cows), Seniors (10+ yrs, tooth wear/osteoarthritis; primary hunting targets).
- **Wolves (`future.wolf`)**: Pups, Subadults, and Pack Adults. Selective attack rates (`moose.wolf.txt`) target Calves and Senior adults over prime Adults.

### Temporal Scaling & Step Calibration
- **1 Step = 1 Day** (Degree-Day / daily unit increment); **365 Steps = 1 Year**.
- Per-step demographic transition, predation, and mortality probabilities are calibrated for daily stepping so multi-step simulation runs (e.g. 200–2,000 steps per click) remain stable over multi-year trajectories.

---

## 2. Historical Census Calibration (`wolf_moose.csv`)

Simulations can be initialized from or dynamically benchmarked against the 40-year empirical census time series (1980–2019):
- **1980 Baseline**: 664 Moose, 50 Wolves
- **1995 Peak**: 2,400 Moose, 16 Wolves
- **2018 Crash**: 1,500 Moose, 2 Wolves

---

## 3. Interactive Offline Application (`IsleRoyaleApp()`)

`IsleRoyaleApp()` launches a 100% offline Shiny application built on the generalized `ecosystemApp()` architecture:

- **5 Output Panels**:
  1. **Substrate Plot**: Native `ggplot2` spatial substrate map displaying habitat features, landmark pins, hex mesh, and active organism positions.
  2. **Age Classes**: Side-by-side `cowplot` multi-panel plot displaying step-by-step population dynamics (`ewing_ageclass(sim)`).
  3. **Census Benchmarks**: Dual-panel grid rendering spatial positions alongside 40-year empirical census benchmark trajectories (`wolf_moose.csv`).
  4. **Live Demographics**: Live tabular summary of active organisms grouped by age class.
  5. **Input Data**: Composes `inputApp` to discover and inspect tabular configuration data (filtering out spatial `.rds` layers).

- **Streamlined Sidebar Layout**:
  - Top-aligned **Steps per click** slider.
  - Single-row **Run** & **Reset** buttons (`display: flex; gap: 8px;`).
  - Inlined **Moose Habitat** & **Landmarks** map overlay checkboxes.
  - Bottom-positioned **Baseline Year** selector (1980–2019).
  - Compact CSS whitespace compression matching sidebar height to main display panels.

---

## 4. Offline GIS Architecture & Multi-Landscape Cache Engine

### Standalone Spatial Overlay
Substrate grids and habitat suitability meshes are generated directly from local pre-computed `sf` spatial geometries stored in `inst/extdata/isle_royale/`:
- **`isle_royale_layer.rds`**: Complete 1-feature MULTIPOLYGON boundary outline.
- **`isle_royale_features.rds`**: Inland lakes, streams, cool shaded forests, and bogs.
- **`isle_royale_landmarks.rds`**: Geocoded landmark POIs (Windigo, Ojibway Lake, Feldtmann Lake, Tobin Harbor).

### Generalized Site Cache Resolver (`get_site_cache_file`)
Spatial datasets and configuration folders are resolved dynamically across installed package environments (`library/ewing/extdata/[site]`) and source development trees (`inst/extdata/[site]`):

```r
layer_path <- get_site_cache_file("isle_royale_layer.rds", site = "isle_royale")
```

The underlying generalized engine (`init_ecosystem_sim`, `run_ecosystem_sim`, `ecosystemApp`) allows `ewing` to scale seamlessly to future landscapes (e.g. `site = "madeline_island"`, `site = "yellowstone"`), while `init_isle_royale_sim()` and `IsleRoyaleApp()` operate as 100% backward-compatible shell wrappers.

---

## 5. Quickstart R Workflow

```r
library(ewing)

# 1. Initialize Isle Royale spatial simulation (1980 baseline)
sim_obj <- init_isle_royale_sim(year = 1980)

# 2. Run simulation steps (e.g. 200 days)
sim_obj <- run_isle_royale_sim(sim_obj, nstep = 200)

# 3. Visualize spatial landscape & empirical census benchmarks
ggplot_isle_royale_sim(sim_obj)

# 4. Launch interactive offline Shiny application
IsleRoyaleApp()
```

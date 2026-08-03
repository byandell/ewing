# Isle Royale Wolf-Moose Ecological Case Study Guide

This document provides a comprehensive user and case-study guide for the **Isle Royale Wolf-Moose Predator-Prey Model** in the `ewing` package.

---

## 1. Overview of Isle Royale Ecological System

Isle Royale National Park (Lake Superior, Michigan) represents an isolated island predator-prey system tracked by continuous wildlife census research since 1958 ([NPS Fauna Series 7](https://npshistory.com/series/fauna/7/sec5.htm)).

### Key Habitat Features & Sighting POIs
- **Washington Creek in Windigo** (`lon = -89.146, lat = 47.923`): Stream and shaded forest feeding area.
- **Ojibway Lake** (`lon = -88.618, lat = 48.113`): Inland lake with rich aquatic vegetation.
- **Feldtmann Lake** (`lon = -88.961, lat = 47.876`): Major southwest inland lake habitat.
- **Hidden Lake in Tobin Harbor** (`lon = -88.490, lat = 48.151`): Aquatic plant feeding area.

---

## 2. Ecological Framework & Temporal Scaling Rules

### Moose Life Cycle (`future.moose`)
- **Calf (0–1 yr)**: High vulnerability to wolf predation and severe winter weather.
- **Yearling (1–2 yrs)**: Transitional stage dispersing across habitat features.
- **Adult (2–9 yrs)**: Prime reproductive cows (producing 1–2 calves/yr). Low natural vulnerability to wolf predation.
- **Senior (10+ yrs)**: Aging adults with tooth wear and osteoarthritis; prime target for wolf pack hunting.

### Wolf Life Cycle & Predation (`future.wolf`)
- **Pups, Subadults, & Adults**: Pack structure with denning pups, roving subadults, and breeding pack adults.
- **Predation Functional Response**: Wolf attack rates in competing risk tables (`moose.wolf.txt`) selectively target vulnerable Calves and Senior adults over prime Adults.

### Time Units & Temporal Scaling
- **1 Time Unit (1 Step) = 1 Day** (Degree-Day / daily unit increment).
- **365 Time Units (Steps) = 1 Year**.
- Annual reproduction occurs every `365` steps. Adult Moose lifespan = `2920` steps (**8 years**); Adult Wolf lifespan = `2555` steps (**7 years**).
- Moose use Degree-Days (`units = DD`, 1 step = 1 daily fluctuation cycle); Wolves use clock hours (`units = hr`, 24 hours = 1 step / day).

---

## 3. Historical Census Calibration (`wolf_moose.csv`)

Simulations can be initialized from or dynamically benchmarked against the 40-year empirical census time series (1980–2019):
- **1980 Baseline**: 664 Moose, 50 Wolves
- **1995 Peak**: 2,400 Moose, 16 Wolves
- **2018 Crash**: 1,500 Moose, 2 Wolves

---

## 4. Interactive Offline Application (`IsleRoyaleApp()`)

The `IsleRoyaleApp()` function launches an interactive, 100% offline Shiny application with **zero external web API dependencies**:

- **5 Multi-View Output Panels**:
  1. **Substrate Plot**: Native `ggplot2` spatial GIS substrate map using local `sf` layers displaying habitat features, landmark pins, hex grid, and active organism positions.
  2. **Age Classes**: Side-by-side `cowplot` multi-panel plot displaying step-by-step population age-class dynamics over time/days (`ewing_ageclass(sim)`). Each species (Moose vs. Wolf) features a dedicated panel with an adjacent legend listing only its specific age-class states.
  3. **Census Benchmarks**: High-resolution dual-panel `cowplot` grid rendering spatial organism positions alongside 40-year empirical census trajectory benchmarks (`wolf_moose.csv`).
  4. **Live Demographics**: Tabular summary of active living organisms grouped by age class that updates dynamically as simulation steps execute.
  5. **Input Data**: Composes `inputApp` to dynamically discover and view input configuration tables (`organism.features`, `future.moose`, `future.wolf`, `moose.wolf`, `substrate.moose`, `substrate.wolf`, `substrate.substrate`, `isle_royale_features`, `isle_royale_landmarks`, and `wolf_moose` census data).

- **Tab-Aware Sidebar Decluttering**: Displays plot-specific controls (`show_habitat`, `show_landmarks`, `norm`, `total`, **`Steps` vs `Days`**) strictly when their corresponding tab is active.
- **Logarithmic Step Size Slider**: Step click choices: `1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000`.

---

## 5. Quickstart Simulation Workflow in R

```r
library(ewing)

# 1. Initialize Isle Royale spatial simulation with 1980 baseline counts (664 Moose, 50 Wolves)
sim_obj <- init_isle_royale_sim(year = 1980)

# 2. Run simulation steps (e.g. 200 days)
sim_obj <- run_isle_royale_sim(sim_obj, nstep = 200)

# 3. Visualize spatial landscape & 40-year empirical census benchmark trajectories
ggplot_isle_royale_sim(sim_obj)

# 4. Plot side-by-side per-species age class dynamics over time (days)
autoplot(ewing_ageclass(sim_obj), x_var = "time")

# 5. Launch interactive 100% offline Shiny application
IsleRoyaleApp()
```

---

## 6. Interactive Demo Gallery & Technical References

- **Interactive Shinylive WebAssembly Demo**: [`demos/IsleRoyaleApp.qmd`](../../demos/IsleRoyaleApp.qmd)
- **Developer Guide & Site Prototyping**: [DEVELOPER.md](../../DEVELOPER.md)
- **GIS Substrate Mesh Pipeline**: Vignette [`vignettes/tech_guide/gis.Rmd`](../../vignettes/tech_guide/gis.Rmd)
- **Simulation Engine Mechanics & Scaling**: Vignette [`vignettes/tech_guide/engine.Rmd`](../../vignettes/tech_guide/engine.Rmd)
- **Shiny UI Architecture & Module Design**: Vignette [`vignettes/tech_guide/ui.Rmd`](../../vignettes/tech_guide/ui.Rmd)

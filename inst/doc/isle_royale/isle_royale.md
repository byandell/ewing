# Isle Royale Wolf-Moose Ecological Case Study Guide

This document provides a user and case-study guide for the **Isle Royale Wolf-Moose Predator-Prey Model** in the `ewing` package.

---

## 1. Overview of Isle Royale Ecological System

Isle Royale National Park (Lake Superior, Michigan) represents an isolated island predator-prey system tracked by continuous wildlife census research since 1958 ([NPS Fauna Series 7](https://npshistory.com/series/fauna/7/sec5.htm)).

### Key Habitat Features & Sighting POIs
- **Washington Creek in Windigo** (`lon = -89.146, lat = 47.923`): Stream and shaded forest feeding area.
- **Ojibway Lake** (`lon = -88.618, lat = 48.113`): Inland lake with aquatic vegetation.
- **Feldtmann Lake** (`lon = -88.961, lat = 47.876`): Major southwest lake habitat.
- **Hidden Lake in Tobin Harbor** (`lon = -88.490, lat = 48.151`): Aquatic plant feeding area.

---

## 2. Demographics & Temporal Scaling Rules

### Moose Life Cycle (`moose`)
- **Calf (0–1 yr)**: High vulnerability to wolf predation and severe winter weather.
- **Yearling (1–2 yrs)**: Transitional stage dispersing across habitat features.
- **Adult (2–9 yrs)**: Prime reproductive cows (1–2 calves/yr). Low natural vulnerability to wolf predation.
- **Senior (10+ yrs)**: Aging adults with tooth wear and osteoarthritis; prime target for wolf pack hunting.

### Wolf Life Cycle (`wolf`)
- **Pups, Subadults, & Adults**: Pack structure with denning pups, roving subadults, and breeding pack adults.
- **Predation Functional Response**: Wolf attack rates in competing risk tables (`moose.wolf.txt`) selectively target vulnerable Calves and Senior adults over prime Adults.

### Time Units & Scaling
- **1 Time Unit (1 Step) = 1 Day**.
- **365 Time Units (Steps) = 1 Year**.
- Annual reproduction occurs every `365` steps. Adult Moose lifespan = `2920` steps (**8 years**); Adult Wolf lifespan = `2555` steps (**7 years**).

---

## 3. Quickstart Simulation Workflow in R

```r
library(ewing)

# 1. Initialize Isle Royale spatial simulation with 1980 baseline counts
sim_obj <- init_isle_royale_sim(year = 1980)

# 2. Run simulation steps (e.g., 200 days)
sim_obj <- run_isle_royale_sim(sim_obj, nstep = 200)

# 3. Visualize spatial landscape & benchmark trajectories
ggplot_isle_royale_sim(sim_obj)

# 4. Plot side-by-side age class dynamics
autoplot(ewing_ageclass(sim_obj), x_var = "time")

# 5. Launch interactive 100% offline Shiny application
IsleRoyaleApp()
```

---

## 4. Technical Reference Links

For deeper technical implementation details, GIS spatial mesh pipeline, and developer guidelines:
- **Developer Guide & Site Prototyping**: [DEVELOPER.md](../../DEVELOPER.md)
- **GIS Substrate Mesh Pipeline**: Vignette [`vignettes/tech_guide/gis.Rmd`](../../vignettes/tech_guide/gis.Rmd)
- **Simulation Engine Mechanics & Scaling**: Vignette [`vignettes/tech_guide/engine.Rmd`](../../vignettes/tech_guide/engine.Rmd)
- **Shiny UI Architecture & Module Design**: Vignette [`vignettes/tech_guide/ui.Rmd`](../../vignettes/tech_guide/ui.Rmd)

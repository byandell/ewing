# ewing — Quantitative Population Ethology (QPE) Simulation Package

## Package Overview

The **ewing** R package implements Bland Ewing's event-driven individual-based simulation for host-parasite ecological systems. Each individual organism is tracked through its life cycle using competing risks for stage transitions, with spatial movement on a triangular substrate grid.

- **Version:** 1.9.8
- **Author:** Brian S. Yandell (yandell@stat.wisc.edu)
- **License:** GPL-3
- **GitHub:** https://github.com/byandell/ewing
- **Live Shiny app:** https://connect.doit.wisc.edu/SystemsEthology

## Installation

```r
pak::pak("byandell/ewing", dependencies = TRUE, upgrade = TRUE)
```

## Basic Workflow

```r
library(ewing)

# Initialize simulation (default: 200 hosts, 100 parasites)
mysim <- init.simulation()

# Run 4000 steps
simres <- future.events(mysim, nstep = 4000, refresh = 200, plotit = FALSE)

# Visualize
plot(simres)                       # Age classes and substrate distribution
ggplot_current(simres, "host")     # Spatial positions of hosts
temp.plot(simres)                  # Temperature/degree-day relationship

# Multiple runs and envelopes
mysims <- ewing_discrete(nsim = 10, nstep = 4000, verbose = TRUE)
ggplot_ewing_envelopes(mysims, confidence = TRUE)

# Pre-computed example data
data(simdata)
ggplot_ewing_envelopes(simdata)

# Launch interactive Shiny app
ewingApp()
```

## Data Files

Located in `data/`:

| File | Purpose |
|------|---------|
| `default.xlsx` | Default configuration (Excel workbook with all required sheets) |
| `small.xlsx`, `smaller.xlsx`, `smallerb.xlsx` | Smaller alternative configurations |
| `simdata.rda` | Pre-computed example with 10+ simulation runs |
| `organism.features.txt`, `future.host.txt`, `future.parasite.txt` | Tab-delimited input tables |
| `host.parasite.txt`, `substrate.*.txt`, `temperature.*.txt` | Additional input tables |

### Excel Workbook Sheet Structure

Required sheets in any configuration workbook:
- `organism.features` — species, life stages, reproduction, parasitism type
- `future.{species}` — competing risks for stage transitions (one per species)
- `{host}.{parasite}` — interaction rates
- `substrate.{species}` — movement weights per species
- `substrate.substrate` — substrate connectivity
- `temperature.base`, `temperature.par` — thermal regime (optional)

## Key Functions

| Function | Purpose |
|----------|---------|
| `init.simulation(package, count, interact, ...)` | Initialize simulation; returns `ewing` object |
| `future.events(community, nstep, species, ...)` | Main simulation loop |
| `ewing_discrete(nsim, nstep, ...)` | Run multiple simulations and aggregate |
| `ewing_envelopes(object)` | Build envelope objects from multi-run results |
| `ggplot_ewing(object, step, ageclass, substrate, ...)` | Main ggplot2 visualization |
| `ggplot_ewing_envelopes(object, confidence, ...)` | Plot simulation envelopes |
| `ggplot_current(x, species, ...)` | Spatial distribution plot |
| `create_substrate(topology, width, step)` | Build geometric substrate network |
| `ewingApp()` | Launch interactive Shiny app |

## S3 Classes

- `ewing` — main simulation result (community + metadata)
- `ewing_discrete` — discretized multi-run results
- `ewing_envelopes` / `ewing_envelope` — envelope objects from multiple runs
- `ewing_ageclass` / `ewing_substrate` / `ewing_snapshot` — intermediate plot data
- `tricoord` — tridiagonal coordinate vector (with `+`/`-` operators)
- `substrate` — geometric substrate network

## Key Concepts

- **Competing risks:** Multiple possible stage transitions from any given life stage, drawn from `future.host`/`future.parasite` tables
- **Leftist tree event queue:** Binary tree structure for O(log n) event scheduling per species
- **Triangular coordinate system:** Substrate represented as interconnected triangles; coordinates (a, b, c) where c = -(a+b)
- **Degree-day integration:** Species timing can use clock hours (`hr`) or degree-days (`DD`); spline interpolation converts between them

## Core R File Locations

- `R/` — 60+ source files; key ones: `ewingApp.R`, `initApp.R`, `ewing_substrate.R`, `ewing_ageclass.R`, `distPlotApp.R`, `multApp.R`
- `inst/shinyApp/app.R` — standalone Shiny app entry point
- `inst/scripts/` — utility scripts including `watershedApp.R`, `watershed_overlay.R`
- `inst/doc/datasets.md` — dataset documentation
- `vignettes/ewing.Rmd` — main tutorial

## Dependencies

Key packages: `dplyr`, `tidyr`, `ggplot2`, `tibble`, `rlang`, `purrr`, `readxl`, `shiny`, `bslib`, `patchwork`, `leaflet`, `GET`, `sf`, `splines`, `DT`

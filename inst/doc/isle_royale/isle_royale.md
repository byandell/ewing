# Moose-Wolf Isle Royale Analysis & Spatial Habitat Model

[Dynamic structural equation models (dsem, James Thorson)](https://github.com/James-Thorson-NOAA/dsem)

Isle Royale moose can be found throughout the island. In the summer, it is common to see them feeding in inland lakes and beaver ponds. This helps them stay cool, plus they like to eat the aquatic plants that grow beneath the surface. If not in water, they tend to stay in the cool shaded forests and bogs, especially during the hottest parts of the day. They are most active during dawn and dusk.

## Notable Moose Sighting Landmarks

- **Washington Creek in Windigo** (`lon = -89.146, lat = 47.923`): Stream and shaded forest feeding area.
- **Ojibway Lake** (`lon = -88.618, lat = 48.113`): Inland lake with rich aquatic vegetation.
- **Feldtmann Lake** (`lon = -88.961, lat = 47.876`): Major southwest inland lake habitat.
- **Hidden Lake in Tobin Harbor** (`lon = -88.490, lat = 48.151`): Aquatic plant feeding area near Tobin Harbor.

---

## Spatial Habitat Overlay Model & Hexagonal Substrate Grid

The `ewing` package provides a generalizable spatial mapping framework to project hexagonal simulation substrates onto real-world island and regional geographies.

### 1. Base Geography (`sf` Polygon)
- USGS HUC12 Subwatershed boundary `041800000101` restricted to the `"Isle Royale"` island outline via `get_watershed("041800000101", feature_name = "Isle Royale")`.

### 2. Pre-Computed Spatial Feature Datasets (`inst/extdata/isle_royale/`)
To guarantee instant rendering without relying on live API calls, spatial feature datasets are pre-computed and stored:
- `inst/extdata/isle_royale/isle_royale_features.rds`: `sf` polygons and linestrings for Inland Lakes, Beaver Ponds/Waterways, Shaded Forests, and Bogs/Wetlands.
- `inst/extdata/isle_royale/isle_royale_landmarks.rds`: `sf` point collection for key moose sighting locations.

### 3. Generalizable R API Functions (`R/habitat.R`)

- `get_habitat_features(watershed_obj, categories)`: Extracts OpenStreetMap feature polygons/lines (lakes, waterways, forests, bogs) or loads pre-computed local `.rds` datasets.
- `get_moose_landmarks(watershed_obj)`: Geocodes named sighting POIs into spatial point markers.
- `add_habitat_hex_overlay(hex_obj, habitat_sf, landmarks_sf)`: Intersects habitat polygons with hex mesh cells, calculating habitat suitability scores / substrate weight vectors per hexagon for `ewing` spatial movement models.
- `autoplot(habitat_overlay)`: `ggplot2` autoplot method rendering the island outline, colored habitat feature overlays, hex grid, and landmark POI pin markers.
- `add_leaflet_habitat_overlay(map, habitat_overlay)`: Interactive Leaflet layer renderer used in `hexmapApp()`.

### 4. Standalone Reproduction Script
Run the prototype generation script to re-build and save feature data and visualization:
```bash
Rscript inst/scripts/isle_royale_habitat.R
```
This generates `isle_royale_habitat_map.png` demonstrating the complete spatial substrate overlay.

---

## Wolf-Moose Predator-Prey Landscape Simulation Model

The `ewing` package simulates the Isle Royale wolf-moose system by overlaying individual organisms onto the spatial substrate mesh weighted by habitat preferences, incorporating historical annual census counts from `wolf_moose.csv` (1980–2019) and life cycle stage transitions documented in National Park Service fauna research ([NPS Fauna Series 7](https://npshistory.com/series/fauna/7/sec5.htm)).

### 1. Ecological & Demographic Framework

#### Moose Life Cycle (`moose`)
- **Calf (0–1 yr)**: Born in late spring (May–June). High vulnerability to wolf predation and severe winter weather.
- **Yearling (1–2 yrs)**: Transitional stage dispersing across habitat features.
- **Adult (2–9 yrs)**: Prime reproductive cows (producing 1–2 calves/yr). Low natural vulnerability to wolf predation.
- **Senior (10+ yrs)**: Aging adults with tooth wear and osteoarthritis; prime target for wolf pack hunting.

#### Wolf Life Cycle & Predation (`wolf`)
- **Pups, Subadults, & Adults**: Pack structure with denning pups, roving subadults, and breeding pack adults.
- **Predation Functional Response**: Wolf attack rates in competing risk tables (`moose.wolf.txt`) are parameterized to selectively target vulnerable Calves and Senior adults over prime Adults, with spatial movement weighted across high-suitability hex cells (lakes, beaver ponds, shaded forests, bogs).

### 2. Historical Census Calibration (`wolf_moose.csv`)
Simulations can be dynamically benchmarked against the 40-year empirical time series (1980–2019):
- **1980 Baseline**: 664 Moose, 50 Wolves
- **1995 Peak**: 2,400 Moose, 16 Wolves
- **2018 Crash**: 1,500 Moose, 2 Wolves

### 3. Simulation Workflow in R (`R/isle_royale_sim.R`)

```r
library(ewing)

# 1. Initialize Isle Royale spatial simulation with 1980 baseline counts
sim_obj <- init_isle_royale_sim(year = 1980)

# 2. Run event-driven simulation steps
sim_obj <- run_isle_royale_sim(sim_obj, nstep = 1000)

# 3. Visualize spatial distribution and benchmark against 1980-2019 empirical counts
ggplot_isle_royale_sim(sim_obj)
```

### 4. Standalone Simulation Script
Run the full simulation pipeline:
```bash
Rscript inst/scripts/isle_royale_simulation.R
```
This generates `isle_royale_simulation_plot.png`, displaying both the spatial landscape positions of moose and wolves across Isle Royale habitat features and empirical census benchmark trajectories.

### 3. Spatial Movement & Organism Visualization Styling

#### Dynamic Spatial Movement Engine
Executing `run_isle_royale_sim(sim_obj, nstep)` steps organisms through time and space:
- **Habitat-Weighted Movement**: Organisms evaluate adjacent hexagonal substrate cells (`sf::st_touches`), moving to neighboring hexes with probability proportional to habitat suitability scores (`habitat_score`). Moose gravitate towards inland lakes, beaver ponds, shaded forests, and bogs, while wolves roam the landscape hunting vulnerable prey.
- **Geographic Coordinate Tracking**: Centroid coordinates (`lon`, `lat`) of target hexes are updated at each simulation step with small random spatial jitter to prevent point stacking.

#### Organism Symbol Styling
Organisms are rendered on both `ggplot2` autoplots and interactive `Leaflet` maps as smaller open circles with thick edges:
- **Moose**: Compact open green circle (`color = "#27ae60"`, `shape = 21`, `fill = NA`, `stroke = 1.0`, `size = 0.8` in `ggplot2`; `radius = 2.5`, `weight = 2.0`, `fill = FALSE` in `Leaflet`).
- **Wolf**: Open red circle (`color = "#e74c3c"`, `shape = 21`, `fill = NA`, `stroke = 1.4`, `size = 1.5` in `ggplot2`; `radius = 4.5`, `weight = 2.8`, `fill = FALSE` in `Leaflet`).

---

## Generalizable Site Prototyping & Discovery Pipeline

Isle Royale serves as the prototype model for building spatial `ewing` simulation sites across arbitrary islands, subwatersheds, or regional ecosystems.

### 1. Extracting Custom Site Geometries (`hexmapApp.R`)

The `hexmapApp()` interface allows users to discover geographic sites interactively, select HUC12 subwatersheds, clip feature boundaries, and export spatial geometries as `.rds` files:

- **Interactive Discovery**: Search USGS HUC subwatersheds or outline rubberband bounding boxes.
- **RDS Export Buttons**:
  - `Download Habitat Features (.rds)`: Exports extracted inland lakes, waterways, shaded forests, and bogs as an `sf` polygon dataframe (e.g. `site_features.rds`).
  - `Download Sighting Landmarks (.rds)`: Exports landmark point geometries as an `sf` dataframe (e.g. `site_landmarks.rds`).
- **Reactive Module Returns**: `hexmapServer()` returns a reactive list containing `hex_obj`, `habitat_sf`, `landmarks_sf`, and `huc_info` for downstream programatic composition.

### 2. Loading Custom Site Geometries into Simulation Engine

Custom site `.rds` files exported from `hexmapApp()` can be passed directly to `init_isle_royale_sim()`:

```r
library(ewing)

# Initialize simulation on a custom geographic site using exported RDS files
custom_sim <- init_isle_royale_sim(
  features_rds = "path/to/custom_site_features.rds",
  landmarks_rds = "path/to/custom_site_landmarks.rds",
  n_moose = 500,
  n_wolves = 30
)

# Step simulation forward
sim_res <- run_isle_royale_sim(custom_sim, nstep = 500)

# Render offline spatial substrate plot
ewing_substrate(sim_res)
```

---

## Interactive Offline Simulation Platform (`IsleRoyaleApp.R`)

The `IsleRoyaleApp()` function launches an interactive 100% offline Shiny application for exploring spatial predator-prey dynamics over pre-saved or imported site geometries with **zero external web API dependencies**.

### 1. Launching the App

```r
library(ewing)

# Launch interactive offline Shiny app
IsleRoyaleApp()
```

### 2. User Control Parameters (`IsleRoyaleInput`)

| Parameter | Control Type | Description |
|-----------|--------------|-------------|
| **Historical Baseline Year** | `selectInput` (1980–2019) | Automatically populates initial Moose and Wolf counts from `wolf_moose.csv` census data for the selected start year. |
| **Initial Moose Count** | `sliderInput` (50–3000) | Allows user overrides of starting moose population size. |
| **Initial Wolf Count** | `sliderInput` (0–60) | Allows user overrides of starting wolf population size. |
| **Hexagon Extent Diameter** | `sliderInput` (0.005–0.03 deg) | Adjusts the spatial resolution of the hexagonal substrate mesh over the island. |
| **Overlay Moose Habitats** | `checkboxInput` | Toggles rendering of inland lakes, beaver ponds, shaded forests, and bogs. |
| **Show Sighting Landmarks** | `checkboxInput` | Toggles landmark pins for Windigo, Ojibway Lake, Feldtmann Lake, and Tobin Harbor. |
| **Simulation Step Control** | `sliderInput` & `actionButton` | Configures steps per click (50–1000) and executes simulation steps interactively. |

### 3. Multi-View Output Panels (`IsleRoyaleOutput`)

- **Substrate Plot**: Native `ggplot2` spatial substrate map leveraging local `sf` objects (`inst/extdata/isle_royale/`) to plot habitat features, landmark pins, hex mesh, and active organism positions with **0 API calls** (an Isle Royale version of `R/substrateApp.R`).
- **Age Distributions**: Interactive simulation dynamics panel displaying step-by-step population age-class distributions over time (`ewing_ageclass(sim)`), tracking Calves, Yearlings, Adults, Seniors for Moose and Pups, Subadults, Adults for Wolves (similar to "Dist Plots" in `R/sysetholApp.R`).
- **Census Benchmarks**: High-resolution dual-panel plot rendering spatial organism positions on the habitat map alongside 40-year empirical census trajectory benchmarks (`wolf_moose.csv`).
- **Live Demographics**: Live tabular breakdown of active living organisms grouped by age class that **updates dynamically as simulation steps execute** (tracking births, aging, and wolf predation).
- **Input Data**: Interactive table explorer allowing users to inspect the underlying configuration tables (`organism.features`, `future.moose`, `future.wolf`, `moose.wolf`, `substrate.moose`, `substrate.wolf`, and historical `wolf_moose.csv` census data) governing the simulation rules.








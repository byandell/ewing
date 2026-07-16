# Graphical Visualization Modules

This sub-guide covers the three visual rendering engines designed to
display simulation results: the age-class distribution plot
(`distPlotApp`), the spatial substrate coordinate plotter
(`substrateApp`), and the multi-run envelope graph (`envPlotApp`).

------------------------------------------------------------------------

## 1. Age Class Distributions (`distPlotApp`)

Defined in
[R/distPlotApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/distPlotApp.R),
this panel shows the continuous population demographics (number of
individuals grouped by age classes over simulation steps).

### Components

- [`distPlotInput()`](https://byandell.github.io/ewing/reference/distPlotApp.md):
  Displays configuration check-boxes:
  - **Normalize Plot**: Toggle relative frequencies per stage class.
  - **Include Total**: Overlay a total population size line.
- [`distPlotOutput()`](https://byandell.github.io/ewing/reference/distPlotApp.md):
  Allocates the plot frame widget on the dashboard.
- [`distPlotServer()`](https://byandell.github.io/ewing/reference/distPlotApp.md):
  - Validates if the active simulation resides under class `ewing`
    (single run mode).
  - Triggers
    [ewing_ageclass()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_ageclass.R)
    to discretize state structures into step-wise tables.
  - Returns a standard
    [`ggplot2::autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
    object.

------------------------------------------------------------------------

## 2. Spatial Substrate Grid Mappings (`substrateApp`)

Defined in
[R/substrateApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/substrateApp.R),
this panel maps individual spatial tracking over continuous substrates
(e.g. host and parasite movements across leaves and fruits).

### Components

- [`substrateOutput()`](https://byandell.github.io/ewing/reference/substrateApp.md):
  Renders the dynamic layout sized proportionally to the active species
  count.
- [`substrateServer()`](https://byandell.github.io/ewing/reference/substrateApp.md):
  - Resolves individual coordinates from `simres()$pop`.
  - For each tracked species, it maps tridiagonal coordinates
    $`(a, b, c)`$ into Cartesian coordinates $`(x, y)`$ using
    [ewing_substrate()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_substrate.R).
  - Uses
    [cowplot::plot_grid()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewingApp.R#L12)
    to arrange and output all sub-plots in a single unified graphic.

------------------------------------------------------------------------

## 3. Multi-Simulation Envelope Intervals (`envPlotApp`)

Defined in
[R/envPlotApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/envPlotApp.R),
this panel renders the statistical bounds and confidence bands across
multi-run iterations.

### Components

- [`envPlotInput()`](https://byandell.github.io/ewing/reference/envPlotApp.md):
  Displays the **Confidence band** checkbox.
- [`envPlotOutput()`](https://byandell.github.io/ewing/reference/envPlotApp.md):
  Renders the multi-plot grid.
- [`envPlotServer()`](https://byandell.github.io/ewing/reference/envPlotApp.md):
  - Expects a multi-simulation output structure of class
    `ewing_discrete`.
  - Builds envelopes using
    [ewing_envelopes()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_envelope.R).
  - Displays trajectories and confidence bands using
    [ggplot_ewing_envelopes()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_envelope.R).
    Confidence bands are only enabled if `nsim >= 10`.

------------------------------------------------------------------------

## 4. Summary of Data Transformations & Visualizers

The visualizer components depend on dedicated data transformation
classes:

| Class Object | Creator Function | Visualizer Function | Description |
|----|----|----|----|
| `ewing_ageclass` | [ewing_ageclass()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_ageclass.R) | `autoplot.ewing_ageclass()` | Reshapes continuous logs into time-series buckets per stage. |
| `ewing_substrate` | [ewing_substrate()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_substrate.R) | [`autoplot.ewing_substrate()`](https://byandell.github.io/ewing/reference/ewing_substrate.md) | Converts tridiagonal positions onto 2D Cartesian grids. |
| `ewing_envelopes` | [ewing_envelopes()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_envelope.R) | [ggplot_ewing_envelopes()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_envelope.R) | Performs quantile calculations across multiple runs. |

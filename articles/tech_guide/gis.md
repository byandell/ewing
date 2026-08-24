# GIS Watersheds

This document details the spatial integration of simulation grids with
geographic features, spatial polygon overlays
(`add_watershed_hex_overlay`), Moose habitat suitability scoring, and
the separation of interactive Leaflet GIS discovery into the standalone
**[`hexmap`](https://github.com/byandell/hexmap)** package.

------------------------------------------------------------------------

## 1. Spatial Hexagonal Substrate Grid & Habitat Overlays

The **`ewing`** package provides an offline-capable spatial substrate
engine that constructs continuous hexagonal grid meshes across custom
landscapes (such as Isle Royale).

### 1. Hexagonal Mesh Generation & Autoplotting

- **`create_isle_royale_hex_overlay(hex_diameter)`**: Builds an S3
  spatial substrate mesh (`watershed_hex_overlay`) across the Isle
  Royale boundary, reading pre-computed `.rds` layers from
  `inst/extdata/isle_royale/`.
- **`add_habitat_hex_overlay(hex_obj)`**: Intersects habitat features
  (lakes, waterways, bogs, forests) with hex cells, assigning habitat
  suitability scores and movement weights.
- **[`autoplot.watershed_hex_overlay()`](https://byandell.github.io/ewing/reference/habitat.md)
  &
  [`autoplot.habitat_hex_overlay()`](https://byandell.github.io/ewing/reference/habitat.md)**:
  Generates static `ggplot2` autoplots of the hexagonal substrate mesh
  and habitat suitability overlays.

------------------------------------------------------------------------

## 2. Decoupled Interactive Mapping: The `hexmap` Package

To keep `ewing` lightweight and independent of heavy interactive Leaflet
dependencies (`leaflet`, `leaflet.extras`, `nhdplusTools`), all live
USGS HUC subwatershed lookup, OpenStreetMap feature clipping, and
interactive Leaflet mapping applications have been refactored into the
standalone **[`hexmap`](https://github.com/byandell/hexmap)** package.

### Package Citation & Installation

Users wishing to interactively explore USGS subwatersheds on Leaflet
maps or download custom HUC boundaries can install `hexmap`:

``` r

# install.packages("pak")
pak::pak("byandell/hexmap")
```

### `hexmap` Interactive Application Features

1.  **Leaflet Map Discovery**: Interactive reverse-geocoding and
    landmark search (`get_huc_from_point`, `get_hucs_from_polygon`).
2.  **Dynamic Feature Isolation**: Polygon clipping (`osmdata::getbb()`)
    for restricting watersheds to island landmasses.
3.  **Interactive Hex Mapping**: `hexmapApp()` provides interactive
    Leaflet controls, slider extent scaling, and `.rds` spatial dataset
    downloads.

------------------------------------------------------------------------

## 3. Offline Spatial Architecture & Multi-Landscape Cache Engine (`get_site_cache_file`)

`ewing` resolves pre-computed `.rds` spatial datasets
(`[site]_layer.rds`, `[site]_features.rds`, `[site]_landmarks.rds`)
using `get_site_cache_file(filename, site)`:

- Resolves dataset paths dynamically across installed package
  directories (`extdata/[site]`) and development source trees
  (`inst/extdata/[site]`).
- Enables `ewing` spatial simulations to scale to additional landscapes
  (e.g. `site = "yellowstone"`, `site = "madeline_island"`) with zero
  network API dependency.

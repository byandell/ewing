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


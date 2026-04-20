# Watershed Triangulation Overlay

## Prompt

- **Context**: The `ewing` predator-prey simulation uses an abstract tridiagonal coordinate system. This needs to be spatially projected onto real-world geography utilizing the [Watershed Boundary Dataset (HUC 12s)](https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about). Our specific target is Isle Royale in Lake Superior (HUC12: `041800000101`).
- **Role**: Expert R Developer and Spatial Integrator.
- **Action**: Review prior work (`inst/doc/refactor/triangle.md`, `inst/scripts/substrate_triangle.R`) to execute two foundational steps:
  1. **Algebraic Refactoring**: Architect a more elegant, object-oriented S3 methodology to simplify tri-coordinate math (i.e. replacing item-by-item grid manipulations).
  2. **Subwatershed Overlay pipeline**: Develop actionable scripts (e.g., `inst/scripts/watershed_overlay.R`) to programmatically fetch HUC12 boundaries, intersect them with specific geographic features (like Isle Royale), and overlay a scalable spatial hexagonal grid using standard bounding frameworks.
- **Format**: Tracked feedback and architectural outlines.
- **Tone**: Professional, constructive, and encouraging.

### Projection Reframing

Modify the CRAFT prompt above to begin by finding a geographic feature, such as Isle Royale, of interest to user from an efficient geographic map.
After this is identified, find the HUC12 sub-watershed(s) that contains this feature to define the projection.

### HUC References

- [Watershed Boundary Dataset (HUC 12s)](https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about)
- [Regional Hydrologic Unit Map](https://www.usgs.gov/media/images/regional-hydrologic-unit-map)
- [Watershed Boundary Dataset Structure Visualization](https://www.usgs.gov/media/images/watershed-boundary-dataset-structure-visualization)

## Expert Review & Proposed Architecture

This is a fantastic application of the `ewing` package's tridiagonal infrastructure. Transitioning from abstract ecological topologies (like plant substrates) to geographic spatial datasets (like HUC12 boundaries) heavily benefits from object-oriented refinement.

Here are the tracked changes and design propositions required to elegantly construct this overlay:

### 1. Refactoring Tri-Coordinate Algebra via S3 Classes

Addressing the `(a, b, c)` coordinate vectors item-by-item is brittle and mathematically verbose. By wrapping coordinates into a standardized `tricoord` S3 class structure in `R/triangle.R`, we can implement operator overloading. This empowers R to apply spatial topology translations natively to data.frames and vectors containing these coordinates utilizing elegant `var1 + var2` syntax!

**Feedback Tracked Details (`inst/scripts/substrate_triangle.R`):**

```diff
  # Apply geometric offset
  o_a <- cfg$offset[1]
  o_b <- cfg$offset[2]
  o_c <- cfg$offset[3]
  
- grid$a <- grid$a + o_a
- grid$b <- grid$b + o_b
- grid$c <- grid$c + o_c
+ grid <- grid + cfg$offset
  
  # Map to Cartesian Coordinates
- car_pts <- tri2car(rbind(grid$a, grid$b, grid$c))
+ car_pts <- tri2car(grid)
```

*Explanation: We extract the raw scalar math and matrix transpositions into the base class logic, significantly reducing visual noise and the likelihood of matrix dimension errors (which historically occurred with `rbind`/`cbind` mismatches).*

### 2. Developing Overlay Projections for HUC12 Subwatersheds

To project the tridiagonal substrate network atop HUC12 polygon data (like Isle Royale `041800000101`), we require a transformation pipeline converting mathematical $(a, b, c)$ bounds securely into the `sf` package's CRS (Coordinate Reference Systems):

> [!TIP]
> **Suggested Geographic Projection Pipeline:**
>
> 1. **Data Acquisition & Restriction**: Utilize `nhdplusTools` to fetch the base watershed boundary (`get_huc`). If targeting a specific geographic entity (like an island or national park), use `osmdata::getbb()` to download its bounds and spatially clip the HUC layer using `sf::st_intersection()`, removing unrelated landmasses.
> 2. **Hexagonal Mesh Generation**: Calculate the restricted bounds and pass the layer to `sf::st_make_grid(square = FALSE, cellsize = c(diameter, diameter))` to generate a mathematically uniform spatial hexagonal grid natively spanning the Coordinate Reference System.
> 3. **Topology Filtering**: Drop redundant/empty hexagons extending into the water by checking bounding box intersections via `st_intersects()`.
> 4. **Spatial Geometry Overlays**: Render both the base map constraints and the generated hexagonal mesh into one figure utilizing `ggplot2::geom_sf()`, laying the groundwork for spatial agent dispersion.

### 3. Implementation of the Geographic Pipeline

The script `inst/scripts/watershed_overlay.R` implements this advanced pipeline for the target Isle Royale HUC12. It successfully pairs `nhdplusTools::get_huc()` with `osmdata` named feature filtering (`getbb("Isle Royale")`) to isolate the exact island landmass via spatial intersection (`st_intersection`).

Crucially, it replaces the theoretical plant linkage grid with a true spatial implementation: generating a parameterized `0.01` degree hexagonal grid across the island layout using `sf::st_make_grid()`. Only segments physically touching the island are retained, completing a scalable architectural foundation for mapping localized continuous movement across arbitrary geographical topologies!

### 4. Object-Oriented Hexagonal Overlay Refactoring

Reflecting the broader repository migration towards reusable, programmatic components, the structural functions powering `inst/scripts/watershed_overlay.R` have been fully formalized and abstracted into the central package architecture within `R/watershed.R`. Former legacy bridging code routing to the `substrate_triangle.R` routines has been fully purged in favor of strict, native spatial math:

- `get_watershed(huc_id, feature_name)`: Upgraded to globally preserve spatial bindings within the data, tracking and returning embedded parameter IDs back along with parsed `$layer`, `$lon`, and `$lat` traits for downstream processing intact.
- `add_watershed_hex_overlay(huc_info, hex_diameter = 0.01)`: A dedicated data constructor. It mathematically generates the `st_make_grid` configurations overlaying bounding restrictions securely against dynamic hex parameters. Emits an S3 target of `class = "watershed_hex_overlay"`.
- `autoplot.watershed_hex_overlay(object)`: Translates abstract topology data automatically via `ggplot2`. Rendering geographically mapped interactions is now as intuitive as simply running `autoplot(hex_obj)`.

### 5. Shiny Application Interface (`watershedApp.R`)

We migrated the static mapping logic originally housed in `inst/scripts/watershed_overlay.R` into a dedicated modular Shiny application at `R/watershedApp.R`.

Following the `ewing` package's UI conventions (`ewingApp.R`), this modular application decoupled into four canonical chunks:

1. `watershedApp()`: The macro-wrapper establishing the UI shell and bridging the server invocation.
2. `watershedInput(id)`: A generic UI controller block holding basic `textInput` parameters for `huc12_id` and the `feature_name`. Crucially, an `actionButton` was bound to explicitly submit queries, preventing rapid API polling against NHD and OpenStreetMap on standard keystrokes.
3. `watershedOutput(id)`: The UI view wrapper strictly defining the resulting `plotOutput` plane.
4. `watershedServer(id)`: Generates reactive bounds that intercept the click events, calling the generalized `ewing::get_watershed` API integrations, establishing the geometry via `ewing::add_watershed_hex_overlay`, and visualizing via generic `autoplot`.

#### Geographic Dictionary Expansion

We developed a UI dictionary component handling internal lookups for standard HUC12 IDs and listing out dynamically corresponding sub-feature geometries bounds constraints. This utilizes a static CSV lookup table (`inst/extdata/watershed/huc_features.csv`) matching target HUC bounds to known physical string inputs natively.
Challenge is finding names to populate this, noting that common names
may be ambiguous and need to be resolved to specific geographic location
(county, state).

**Dynamic GIS Discovery (Option B):**
To facilitate populating this static dictionary algorithmically, we implemented a standalone backend utility `discover_watershed_features(huc_id)` natively inside `R/watershed.R`. By pulling the USGS HUC12 bounding box map and piping it directly into `osmdata`, it dynamically executes a raw Overpass XML QL union query across targets like `natural`, `waterway`, and `leisure`.
This should bypass strict API rate limits, but it seems to generate timeout failures.

**Geometry Repair & Caching Optimizations:**
Two critical reliability structures were implemented to guarantee mapping backend stability:

1. **Topological Fixes**: Because public OpenStreetMap vectors are notoriously ill-formatted (possessing self-intersecting loops that naturally crash intersection logic), the spatial pipeline now securely disables Google's strict spherical geometry engine (`sf::sf_use_s2(FALSE)`) and patches incoming structural bounds natively via `sf::st_make_valid()` prior to topological rendering.
2. **Reactive API Caching**: The Shiny UI was decoupled to drastically reduce network payloads to the USGS grid. By formally adapting `get_watershed()` to intercept pre-fetched shapes, we extracted `nhdplusTools::get_huc()` into a dedicated generic `base_huc` reactive. Now, modifying the overlay feature name simply pulls the identical map topographical foundation from internal memory rather than executing sequential 5-second internet fetches!

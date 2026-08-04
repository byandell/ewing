# GIS Watersheds

This document details the spatial integration of simulation grids with
geographic features, USGS HUC12 subwatershed boundary discovery, spatial
polygon overlays (`watershed_overlay`), and deployment architectures.

------------------------------------------------------------------------

## 1. Interactive Geographic Feature Search & Leaflet Discovery

To interactively look up and spatially identify geographic features
(like Isle Royale, Yellowstone, or specific lakes) natively within R,
you generally need to combine a **Geocoding Engine** (to translate text
to coordinates) with an **Interactive Mapping Canvas**.

Here are the most efficient, modern R packages designed to accomplish
this, starting with tools heavily optimized for OpenStreetMap and North
American datasets.

## Longer Strategy

The
[`leafletApp()`](https://byandell.github.io/ewing/reference/leafletApp.md)
interactively finds the HUC12 sub-watershed(s) that contains the feature
of interest. Now we want to isolate the geographic feature and overlay
with hexagon. Note that some code is temporarily in `inst/scripts/` and
probably should be moved back to `R/`. Ultimately we want to connect
this with predator-prey systems discussed in `inst/doc/datasets.md`;
that is, pick a dataset, find the geographic region, overly hexagons (at
appropriate scale) and place organisms on the region.

## 1. The Interactive Canvases

To explore geography interactively (zooming, panning, and clicking),
these are the gold standard packages:

- **`leaflet`**: The foundational package for interactive mapping in R.
  It bindings directly to the JavaScript Leaflet library, allowing you
  to build highly customized, interactive HTML maps that work gracefully
  inside Shiny apps or the RStudio Viewer.
- **`mapview`**: Built on top of `leaflet`, this is the “power user”
  tool for spatial data scientists. Simply passing any spatial object
  (`sf`, `raster`, etc.) into `mapview(my_data)` instantly generates a
  fully interactive, layered map without writing complex leaflet
  configuration code.

## 2. Live Search & Selection Plugins (Interactive Tools)

If you want the ability to type “Isle Royale” into a search bar *on the
map itself* and have the camera fly to the location:

- **`leaflet.extras`**: An expansion package for `leaflet`. It contains
  a magical function called **`addSearchOSM()`**. If you pipe a leaflet
  map into this function, it embeds a live OpenStreetMap search bar into
  the corner of your map widget. Users can type any North American
  landmark, and the map will instantly autoselect it.
- **`mapedit`**: If your goal is to visually explore a map, find a
  boundary, and then *extract its coordinates back into R*, `mapedit`
  allows you to click, draw polygons, or select features on a leaflet
  map. When you finish, it returns the exact spatial coordinates
  (bounding boxes) back into your R session as an `sf` object.

## 3. Programmatic Geocoders (Text-to-Geography)

If you need to programmatically search text strings to validate their
existence before mapping them:

- **`tidygeocoder`**: The absolute best modern package for
  address/landmark routing. It provides a unified, tidy interface
  (`geocode()`) to query powerful North American databases like the US
  Census API, Nominatim (OSM), or ArcGIS, returning exact
  longitudes/latitudes for geographic terms.
- **`osmdata` (`getbb()`)**: As utilized in our pipeline,
  `getbb("Isle Royale")` is fantastic for grabbing raw OpenStreetMap
  bounding polygons simply by feeding it an explicit text string.

## Recommended Experimental Workflow

If you eventually want to move away from manually editing static CSVs
(`huc_features.csv`), the ultimate interactive UI flow combines these
tools:

1.  Render a `leaflet` basemap in your Shiny App.
2.  Add
    [`leaflet.extras::addSearchOSM()`](https://rdrr.io/pkg/leaflet.extras/man/search-geocoding.html)
    so the user gets a physical search bar interface over the map.
3.  The user types “Yellowstone River” into the map’s search box. The
    interactive map zooms to it.
4.  You capture the map’s current bounding box coordinates via Shiny
    (`input$map_bounds`) and pass those bounds strictly into our
    [`discover_watershed_features()`](https://byandell.github.io/ewing/reference/watershed.md)
    utility to pull the exact topographical intersection!

------------------------------------------------------------------------

## Interactive Spatial Discovery Implementation

**Prompt**: Develop an R/leafletApp.R shiny app to interactively look
for geographic features (combining search and click methodologies).
Collect functions in R/leaflet.R (documented using Roxygen2). Modify
inst/doc/refactor/leaflet.md with the prompt and walkthrough when done.

### Architecture Walkthrough

We designed a unified dual-workflow interface that empowers users to
both search via text and interactively click on regions to automatically
fetch topographies.

#### 1. Backend Spatial Adapters (`R/leaflet.R`)

We formalized two backend utilities heavily documented using Roxygen2:

- [`build_base_map()`](https://byandell.github.io/ewing/reference/leaflet.md):
  Wraps the `leaflet` map rendering API. We initialized a default view
  zoomed out over North America, cleanly overlaying standard map tiles.
  Crucially, we embedded the `leaflet.extras::searchOSM()` widget
  directly here, enabling instantaneous global geographic string lookups
  *(Option A paradigm)* without requiring custom API hooks.
- `get_huc_from_point(lng, lat)`: A bridging wrapper that safely ingests
  decimal geographic coordinates. We utilized
  [`sf::st_point()`](https://r-spatial.github.io/sf/reference/st.html)
  to geometrically project the decimals into strict `epsg=4326` CRS
  bounds, allowing us to flawlessly query point intersections against
  the
  [`nhdplusTools::get_huc()`](https://doi-usgs.github.io/nhdplusTools/reference/get_huc.html)
  subwatershed matrix *(Option B paradigm)*.

#### 2. The Interactive Shiny Application (`R/leafletApp.R`)

Using our standardized modular paradigm, we constructed
[`leafletApp()`](https://byandell.github.io/ewing/reference/leafletApp.md)
across 4 core blocks:

- **`leafletInput(id)`**: Binds the visual `leafletOutput` UI element
  and prepares a text UI bridge (`huc_status`) to echo the intersection
  results to the user interactively.
- **`leafletOutput(id)`**: Left blank currently, preparing structural
  space if data table breakdowns are ever desired downstream.
- **`leafletServer(id)`**: The reactive core handling map telemetry.
  - We map `mapper` directly to
    [`build_base_map()`](https://byandell.github.io/ewing/reference/leaflet.md),
    rendering the interactive element locally.
  - We implement a reactive listener explicitly against
    `input$mapper_click`. If a user interacts with the map (either after
    searching for “Isle Royale” or simply scrolling), the underlying
    Javascript fires an event block.
  - Shiny parses the lat/long, renders an interactive HTML status block,
    and executes
    [`get_huc_from_point()`](https://byandell.github.io/ewing/reference/leaflet.md).
  - Automatically, if a USGS boundary mapping is mathematically
    returned, we execute a reverse `leafletProxy` call, drawing a
    semi-transparent `addPolygons()` boundary on the exact map the user
    clicked natively!

#### Summary

Users are now fully equipped to discover subwatersheds entirely
decoupled from string dictionaries! They can launch
[`leafletApp()`](https://byandell.github.io/ewing/reference/leafletApp.md),
search for a landmark on the provided widget, click the physical body of
water on the map, and instantly extract the underlying USGS HUC12
mapping configurations required for the `ewing` ecosystem simulations
locally.

Furthermore,
[`leafletServer()`](https://byandell.github.io/ewing/reference/leafletApp.md)
returns a reactive list (`huc`, `status`, `click`), making it an
adaptable spatial selector module.
[`hexmapApp()`](https://byandell.github.io/ewing/reference/hexmapApp.md)
(`R/hexmapApp.R`) composes `leafletInput` / `leafletOutput` /
`leafletServer` directly, coupling map discovery with
[`add_watershed_hex_overlay()`](https://byandell.github.io/ewing/reference/watershed.md),
feature area restriction (e.g. “Isle Royale”), and dynamic
Leaflet/ggplot hexagonal substrate rendering without duplicating code.

------------------------------------------------------------------------

## 2. HUC12 Watershed Spatial Projection & Polygon Overlay

## Prompt

- **Context**: The `ewing` predator-prey simulation uses an abstract
  tridiagonal coordinate system. This needs to be spatially projected
  onto real-world geography utilizing the [Watershed Boundary Dataset
  (HUC
  12s)](https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about).
  Our specific target is Isle Royale in Lake Superior (HUC12:
  `041800000101`).
- **Role**: Expert R Developer and Spatial Integrator.
- **Action**: Review prior work (`inst/doc/refactor/triangle.md`,
  `inst/scripts/substrate_triangle.R`) to execute two foundational
  steps:
  1.  **Algebraic Refactoring**: Architect a more elegant,
      object-oriented S3 methodology to simplify tri-coordinate math
      (i.e. replacing item-by-item grid manipulations).
  2.  **Subwatershed Overlay pipeline**: Develop actionable scripts
      (e.g., `inst/scripts/watershed_overlay.R`) to programmatically
      fetch HUC12 boundaries, intersect them with specific geographic
      features (like Isle Royale), and overlay a scalable spatial
      hexagonal grid using standard bounding frameworks.
- **Format**: Tracked feedback and architectural outlines.
- **Tone**: Professional, constructive, and encouraging.

### Projection Reframing

Modify the CRAFT prompt above to begin by finding a geographic feature,
such as Isle Royale, of interest to user from an efficient geographic
map. After this is identified, find the HUC12 sub-watershed(s) that
contains this feature to define the projection.

### HUC References

- [Watershed Boundary Dataset (HUC
  12s)](https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about)
- [Regional Hydrologic Unit
  Map](https://www.usgs.gov/media/images/regional-hydrologic-unit-map)
- [Watershed Boundary Dataset Structure
  Visualization](https://www.usgs.gov/media/images/watershed-boundary-dataset-structure-visualization)

## Expert Review & Proposed Architecture

This is a fantastic application of the `ewing` package’s tridiagonal
infrastructure. Transitioning from abstract ecological topologies (like
plant substrates) to geographic spatial datasets (like HUC12 boundaries)
heavily benefits from object-oriented refinement.

Here are the tracked changes and design propositions required to
elegantly construct this overlay:

### 1. Refactoring Tri-Coordinate Algebra via S3 Classes

Addressing the `(a, b, c)` coordinate vectors item-by-item is brittle
and mathematically verbose. By wrapping coordinates into a standardized
`tricoord` S3 class structure in `R/triangle.R`, we can implement
operator overloading. This empowers R to apply spatial topology
translations natively to data.frames and vectors containing these
coordinates utilizing elegant `var1 + var2` syntax!

**Feedback Tracked Details (`inst/scripts/substrate_triangle.R`):**

``` diff
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

*Explanation: We extract the raw scalar math and matrix transpositions
into the base class logic, significantly reducing visual noise and the
likelihood of matrix dimension errors (which historically occurred with
`rbind`/`cbind` mismatches).*

### 2. Developing Overlay Projections for HUC12 Subwatersheds

To project the tridiagonal substrate network atop HUC12 polygon data
(like Isle Royale `041800000101`), we require a transformation pipeline
converting mathematical $`(a, b, c)`$ bounds securely into the `sf`
package’s CRS (Coordinate Reference Systems):

> \[!TIP\] **Suggested Geographic Projection Pipeline:**
>
> 1.  **Data Acquisition & Restriction**: Utilize `nhdplusTools` to
>     fetch the base watershed boundary (`get_huc`). If targeting a
>     specific geographic entity (like an island or national park), use
>     [`osmdata::getbb()`](https://docs.ropensci.org/osmdata/reference/getbb.html)
>     to download its bounds and spatially clip the HUC layer using
>     [`sf::st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html),
>     removing unrelated landmasses.
> 2.  **Hexagonal Mesh Generation**: Calculate the restricted bounds and
>     pass the layer to
>     `sf::st_make_grid(square = FALSE, cellsize = c(diameter, diameter))`
>     to generate a mathematically uniform spatial hexagonal grid
>     natively spanning the Coordinate Reference System.
> 3.  **Topology Filtering**: Drop redundant/empty hexagons extending
>     into the water by checking bounding box intersections via
>     `st_intersects()`.
> 4.  **Spatial Geometry Overlays**: Render both the base map
>     constraints and the generated hexagonal mesh into one figure
>     utilizing
>     [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
>     laying the groundwork for spatial agent dispersion.

### 3. Implementation of the Geographic Pipeline

The script `inst/scripts/watershed_overlay.R` implements this advanced
pipeline for the target Isle Royale HUC12. It successfully pairs
[`nhdplusTools::get_huc()`](https://doi-usgs.github.io/nhdplusTools/reference/get_huc.html)
with `osmdata` named feature filtering (`getbb("Isle Royale")`) to
isolate the exact island landmass via spatial intersection
(`st_intersection`).

Crucially, it replaces the theoretical plant linkage grid with a true
spatial implementation: generating a parameterized `0.01` degree
hexagonal grid across the island layout using
[`sf::st_make_grid()`](https://r-spatial.github.io/sf/reference/st_make_grid.html).
Only segments physically touching the island are retained, completing a
scalable architectural foundation for mapping localized continuous
movement across arbitrary geographical topologies!

### 4. Object-Oriented Hexagonal Overlay Refactoring

Reflecting the broader repository migration towards reusable,
programmatic components, the structural functions powering
`inst/scripts/watershed_overlay.R` have been fully formalized and
abstracted into the central package architecture within `R/watershed.R`.
Former legacy bridging code routing to the `substrate_triangle.R`
routines has been fully purged in favor of strict, native spatial math:

- `get_watershed(huc_id, feature_name)`: Upgraded to globally preserve
  spatial bindings within the data, tracking and returning embedded
  parameter IDs back along with parsed `$layer`, `$lon`, and `$lat`
  traits for downstream processing intact.
- `add_watershed_hex_overlay(huc_info, hex_diameter = 0.01)`: A
  dedicated data constructor. It mathematically generates the
  `st_make_grid` configurations overlaying bounding restrictions
  securely against dynamic hex parameters. Emits an S3 target of
  `class = "watershed_hex_overlay"`.
- `autoplot.watershed_hex_overlay(object)`: Translates abstract topology
  data automatically via `ggplot2`. Rendering geographically mapped
  interactions is now as intuitive as simply running
  `autoplot(hex_obj)`.

### 5. Shiny Application Interface (`watershedApp.R`)

We migrated the static mapping logic originally housed in
`inst/scripts/watershed_overlay.R` into a dedicated modular Shiny
application at `R/watershedApp.R`.

Following the `ewing` package’s UI conventions (`ewingApp.R`), this
modular application decoupled into four canonical chunks:

1.  `watershedApp()`: The macro-wrapper establishing the UI shell and
    bridging the server invocation.
2.  `watershedInput(id)`: A generic UI controller block holding basic
    `textInput` parameters for `huc12_id` and the `feature_name`.
    Crucially, an `actionButton` was bound to explicitly submit queries,
    preventing rapid API polling against NHD and OpenStreetMap on
    standard keystrokes.
3.  `watershedOutput(id)`: The UI view wrapper strictly defining the
    resulting `plotOutput` plane.
4.  `watershedServer(id)`: Generates reactive bounds that intercept the
    click events, calling the generalized
    [`ewing::get_watershed`](https://byandell.github.io/ewing/reference/watershed.md)
    API integrations, establishing the geometry via
    [`ewing::add_watershed_hex_overlay`](https://byandell.github.io/ewing/reference/watershed.md),
    and visualizing via generic `autoplot`.

#### Geographic Dictionary Expansion

We developed a UI dictionary component handling internal lookups for
standard HUC12 IDs and listing out dynamically corresponding sub-feature
geometries bounds constraints. This utilizes a static CSV lookup table
(`inst/extdata/watershed/huc_features.csv`) matching target HUC bounds
to known physical string inputs natively. Challenge is finding names to
populate this, noting that common names may be ambiguous and need to be
resolved to specific geographic location (county, state).

**Dynamic GIS Discovery (Option B):** To facilitate populating this
static dictionary algorithmically, we implemented a standalone backend
utility `discover_watershed_features(huc_id)` natively inside
`R/watershed.R`. By pulling the USGS HUC12 bounding box map and piping
it directly into `osmdata`, it dynamically executes a raw Overpass XML
QL union query across targets like `natural`, `waterway`, and `leisure`.
This should bypass strict API rate limits, but it seems to generate
timeout failures.

**Geometry Repair & Caching Optimizations:** Two critical reliability
structures were implemented to guarantee mapping backend stability:

1.  **Topological Fixes**: Because public OpenStreetMap vectors are
    notoriously ill-formatted (possessing self-intersecting loops that
    naturally crash intersection logic), the spatial pipeline now
    securely disables Google’s strict spherical geometry engine
    (`sf::sf_use_s2(FALSE)`) and patches incoming structural bounds
    natively via
    [`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
    prior to topological rendering.
2.  **Reactive API Caching**: The Shiny UI was decoupled to drastically
    reduce network payloads to the USGS grid. By formally adapting
    [`get_watershed()`](https://byandell.github.io/ewing/reference/watershed.md)
    to intercept pre-fetched shapes, we extracted
    [`nhdplusTools::get_huc()`](https://doi-usgs.github.io/nhdplusTools/reference/get_huc.html)
    into a dedicated generic `base_huc` reactive. Now, modifying the
    overlay feature name simply pulls the identical map topographical
    foundation from internal memory rather than executing sequential
    5-second internet fetches!

### 6. Interactive Hexagonal Watershed App (`R/hexmapApp.R`)

Combining `inst/scripts/watershed_overlay.R` with `R/leafletApp.R`, we
developed `R/hexmapApp.R` using **Shiny Module Composition** to provide
a unified pipeline connecting interactive Leaflet feature discovery with
HUC12 boundary lookup, feature area restriction clipping, and hexagonal
substrate grid overlays.

#### Key Features & Architecture

1.  **Modular Shiny Composition**:
    - Rather than duplicating map handling, `hexmapApp.R` composes the
      `leafletInput()`,
      [`leafletOutput()`](https://byandell.github.io/ewing/reference/leafletApp.md),
      and
      [`leafletServer()`](https://byandell.github.io/ewing/reference/leafletApp.md)
      modules from `R/leafletApp.R`.
    - [`leafletServer()`](https://byandell.github.io/ewing/reference/leafletApp.md)
      returns a reactive list (`huc`, `status`, `click`), allowing
      [`hexmapServer()`](https://byandell.github.io/ewing/reference/hexmapApp.md)
      to seamlessly receive the user’s clicked watershed boundary.
2.  **Feature Isolation & Polygon Clipping**:
    - For HUCs containing extensive open water or surrounding land
      (e.g., Isle Royale HUC `041800000101`),
      `get_watershed(huc_id, feature_name)` uses
      `osmdata::getbb(feature_name)` to download the feature boundary
      (e.g. island polygon) and intersects it (`st_intersection`) with
      the HUC12 boundary.
    - Only the restricted geographic feature geometry is retained for
      hexagonal substrate generation.
3.  **Hexagonal Mesh Generation & Multi-View Rendering**:
    - Calculates spatial hexagonal grid via
      `add_watershed_hex_overlay(huc_info, hex_diameter)`.
    - Renders interactive vector polygons dynamically on the Leaflet map
      canvas via `leafletProxy` and
      [`add_leaflet_hex_overlay()`](https://byandell.github.io/ewing/reference/leaflet.md).
    - Simultaneously renders static `ggplot2` autoplots via
      [`autoplot.watershed_hex_overlay()`](https://byandell.github.io/ewing/reference/watershed.md).
4.  **Modular Package Export**:
    - Exported as
      [`hexmapApp()`](https://byandell.github.io/ewing/reference/hexmapApp.md),
      with modular functions
      [`hexmapInput()`](https://byandell.github.io/ewing/reference/hexmapApp.md),
      [`hexmapOutput()`](https://byandell.github.io/ewing/reference/hexmapApp.md),
      and
      [`hexmapServer()`](https://byandell.github.io/ewing/reference/hexmapApp.md).

### 7. Multi-HUC Regional Aggregation & Rubberband Polygon Selection

Building on individual HUC12 selection, we implemented user-defined
spatial region selection via interactive rubberband polygon drawing,
enabling the combination of adjacent subwatersheds into an aggregated
regional domain.

#### Architecture & Workflow

1.  **Interactive Rubberband Polygon Drawing & Toggle Control
    (`R/leaflet.R` & `R/leafletApp.R`)**:
    - Integrated
      [`leaflet.extras::addDrawToolbar()`](https://rdrr.io/pkg/leaflet.extras/man/draw.html)
      into
      [`build_base_map()`](https://byandell.github.io/ewing/reference/leaflet.md),
      providing intuitive polygon and rectangle draw tools on the
      Leaflet map widget.
    - **Drawing Mode State Tracking (`is_drawing`)**:
      [`leafletServer()`](https://byandell.github.io/ewing/reference/leafletApp.md)
      monitors `input$mapper_draw_start` and `input$mapper_draw_stop`
      events via an `is_drawing` reactive flag. This suppresses
      single-point reverse-geocoding (`input$mapper_click`) while
      placing vertex points, preventing duplicate progress bar triggers
      during drawing.
    - **Inline Control Layout & Region Hiding**: **“Search Watersheds in
      Region”**, **“Clear Region”**, and **“Hide Drawn Region”**
      controls are aligned on a single flex horizontal line. Checking
      **“Hide Drawn Region”** toggles visibility of the drawn region
      polygon without losing boundary coordinates.
2.  **Reverse-Geocoding & Auto-Scaling HUC Hierarchy
    (`get_hucs_from_polygon`)**:
    - `get_hucs_from_polygon(polygon_sf, max_hucs = 10)` projects the
      drawn rubberband region into WGS84 coordinates and queries
      `nhdplusTools::get_huc(AOI = poly, type = "huc12")`.
    - **Dynamic HUC Scaling (HUC12 $`\rightarrow`$ HUC10 $`\rightarrow`$
      HUC8)**: If the drawn region covers more than `max_hucs` (10)
      subwatersheds, the engine automatically scales up the USGS query
      from `huc12` to broader `huc10` or `huc8` levels. This guarantees
      scalable regional aggregation without overwhelming server memory
      or API limits.
3.  **Smooth Layer Group Updating & Bi-directional Syncing**:
    - **Explicit Layer Purging & Group Redrawing**: Updates invoke
      `leafletProxy` with `removeShape(layerId = ids)` and
      `clearGroup("huc_polygons")`. This explicitly purges existing SVG
      shapes from Leaflet JS internal memory, allowing instant style
      re-rendering (solid purple vs bold crimson red `#C0392B` dashed
      `"6,6"`) when adding back or removing HUCs.
    - **Robust Bi-directional Sidebar Syncing (`R/hexmapApp.R`)**:
      Populates a dynamic `selectizeInput(multiple = TRUE)` in the
      “Watershed Controls” input panel. Choices update with
      human-readable HUC IDs and feature names
      (e.g. `041800000101 (Isle Royale East)`). Observer logic uses
      `setequal(unname(as.character(...)))` to eliminate race conditions
      between dropdown updates and map shape events: adding or removing
      watershed tags in the sidebar dropdown instantly updates Leaflet
      map shape renderings, while clicking map shapes dynamically adds
      or removes tags in the sidebar dropdown.
4.  **Regional Aggregation & Topological Unioning (`R/watershed.R`)**:
    - [`get_watershed()`](https://byandell.github.io/ewing/reference/watershed.md)
      natively accepts single HUC IDs, character vectors of HUC IDs, or
      pre-fetched multi-HUC `sf` layers.
    - For multi-HUC regions,
      [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
      merges adjacent included component subwatershed polygons into a
      unified boundary representation (`$layer`), while preserving
      individual component HUC metadata (`$individual_hucs`).
5.  **Continuous Substrate Mesh Generation & Multi-View Rendering**:
    - [`add_watershed_hex_overlay()`](https://byandell.github.io/ewing/reference/watershed.md)
      generates a continuous hexagonal substrate grid spanning the
      aggregated multi-HUC regional polygon.
    - [`add_leaflet_hex_overlay()`](https://byandell.github.io/ewing/reference/leaflet.md)
      renders component HUC boundaries in dashed lines, outer combined
      region boundaries in solid blue, and the unified hex mesh.
    - [`autoplot.watershed_hex_overlay()`](https://byandell.github.io/ewing/reference/watershed.md)
      renders static `ggplot2` autoplots featuring dashed component HUC
      boundaries and continuous regional hex overlays.
6.  **Site Prototyping & GIS Feature Pipeline (Isle Royale Prototype
    Template)**:
    - **OpenStreetMap Feature Extraction**:
      [`get_habitat_features()`](https://byandell.github.io/ewing/reference/habitat.md)
      extracts OpenStreetMap polygon/line features (lakes, bogs,
      waterways, shaded forests).
    - **Landmark Geocoding**:
      [`get_moose_landmarks()`](https://byandell.github.io/ewing/reference/habitat.md)
      geocodes landmark POIs (Windigo, Ojibway Lake, Feldtmann Lake,
      Tobin Harbor).
    - **Hexagon Habitat Suitability Scoring**:
      [`add_habitat_hex_overlay()`](https://byandell.github.io/ewing/reference/habitat.md)
      intersects habitat polygons with hex cells, calculating
      suitability scores / movement weight vectors per hexagon.
    - **RDS Feature Export**:
      [`hexmapApp()`](https://byandell.github.io/ewing/reference/hexmapApp.md)
      exports `site_features.rds` and `site_landmarks.rds` allowing
      offline simulation execution
      ([`init_isle_royale_sim()`](https://byandell.github.io/ewing/reference/isle_royale_sim.md),
      [`ewing_substrate()`](https://byandell.github.io/ewing/reference/ewing_substrate.md))
      with zero API calls.
7.  **Offline Spatial Architecture & Multi-Landscape Cache Engine
    (`get_site_cache_file`)**:
    - `get_site_cache_file(filename, site)` dynamically resolves
      pre-computed `.rds` spatial boundary layers (`[site]_layer.rds`,
      `[site]_features.rds`, `[site]_landmarks.rds`) across installed
      package directories (`library/ewing/extdata/[site]`) and
      development source trees (`inst/extdata/[site]`).
    - Supports folder resolution (`filename = ""`) with
      [`dir.exists()`](https://rdrr.io/r/base/files2.html) validation.
    - Allows `ewing` spatial simulations to scale to future landscapes
      (e.g. `site = "yellowstone"`, `site = "madeline_island"`) by
      placing spatial `.rds` objects in `extdata/[site]/`.

------------------------------------------------------------------------

## 3. Quarto Shinylive & Posit Connect Deployment Architectures

## Prompt

- **User Request**: Can we create a `demos/hexmapApp.qmd`? Will it
  publish with GitHub Pages or do we need to use Posit Connect?

------------------------------------------------------------------------

## Response & Architecture Overview

Yes, we created `demos/hexmapApp.qmd` and integrated it into the demo
gallery navbar
([`demos/_quarto.yml`](https://byandell.github.io/ewing/demos/_quarto.yml))
and gallery index
([`demos/index.qmd`](https://byandell.github.io/ewing/demos/index.qmd)).

### Publishing Architecture Breakdown

#### 1. GitHub Pages (Static Quarto Website)

- **Publishing Outcome**: **Yes, `demos/hexmapApp.qmd` publishes cleanly
  on GitHub Pages!**
- **Mechanism**: Running `quarto render` compiles `demos/hexmapApp.qmd`
  into static HTML (`docs/demos/hexmapApp.html`), rendering workflow
  tutorials, code snippets, static `ggplot2` autoplots, and embedded
  links.
- **Client-Side Shinylive Comparison**:
  - Other demos in `demos/` (such as `triangleApp.qmd`,
    `fivePlotApp.qmd`, `tempApp.qmd`) utilize **Shinylive** (serverless
    WebAssembly/webR running R code completely client-side in the
    browser).
  - However, `hexmapApp` relies on compiled C++ spatial libraries (`sf`
    / GDAL / GEOS) and makes live HTTP network requests to external USGS
    (`nhdplusTools`) and OpenStreetMap (`osmdata`) APIs. Because browser
    WebAssembly sandboxes restrict arbitrary CORS API queries and native
    C++ GIS drivers, the live Shiny app cannot run client-side via
    Shinylive alone on GitHub Pages.

#### 2. Posit Connect (Live Interactive Application)

- **Publishing Outcome**: **Required for hosting the live, interactive
  server application.**
- **Mechanism**: The live interactive Shiny application
  ([`hexmapApp()`](https://byandell.github.io/ewing/reference/hexmapApp.md))
  runs on **Posit Connect** (e.g.,
  [SystemsEthology](https://connect.doit.wisc.edu/SystemsEthology)) or
  **shinyapps.io**, where an R server process executes spatial
  intersections (`sf`) and fetches live USGS / OpenStreetMap data over
  HTTP.
- **Integration**: `demos/hexmapApp.qmd` links directly to the live
  Posit Connect deployment so users reading the GitHub Pages
  documentation can seamlessly launch the interactive app.

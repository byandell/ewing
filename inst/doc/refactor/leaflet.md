# Interactive Geographic Feature Search in R

To interactively look up and spatially identify geographic features (like Isle Royale, Yellowstone, or specific lakes) natively within R, you generally need to combine a **Geocoding Engine** (to translate text to coordinates) with an **Interactive Mapping Canvas**.

Here are the most efficient, modern R packages designed to accomplish this, starting with tools heavily optimized for OpenStreetMap and North American datasets.

## Longer Strategy

The `leafletApp()` interactively finds the HUC12 sub-watershed(s) that contains the feature of interest.
Now we want to isolate the geographic feature and overlay with hexagon.
Note that some code is temporarily in `inst/scripts/`
and probably should be moved back to `R/`.
Ultimately we want to connect this with predator-prey systems discussed in
`inst/doc/datasets.md`; that is, pick a dataset, find the geographic region,
overly hexagons (at appropriate scale) and place organisms on the region.

## 1. The Interactive Canvases

To explore geography interactively (zooming, panning, and clicking), these are the gold standard packages:

* **`leaflet`**: The foundational package for interactive mapping in R. It bindings directly to the JavaScript Leaflet library, allowing you to build highly customized, interactive HTML maps that work gracefully inside Shiny apps or the RStudio Viewer.
* **`mapview`**: Built on top of `leaflet`, this is the "power user" tool for spatial data scientists. Simply passing any spatial object (`sf`, `raster`, etc.) into `mapview(my_data)` instantly generates a fully interactive, layered map without writing complex leaflet configuration code.

## 2. Live Search & Selection Plugins (Interactive Tools)

If you want the ability to type "Isle Royale" into a search bar *on the map itself* and have the camera fly to the location:

* **`leaflet.extras`**: An expansion package for `leaflet`. It contains a magical function called **`addSearchOSM()`**. If you pipe a leaflet map into this function, it embeds a live OpenStreetMap search bar into the corner of your map widget. Users can type any North American landmark, and the map will instantly autoselect it.
* **`mapedit`**: If your goal is to visually explore a map, find a boundary, and then *extract its coordinates back into R*, `mapedit` allows you to click, draw polygons, or select features on a leaflet map. When you finish, it returns the exact spatial coordinates (bounding boxes) back into your R session as an `sf` object.

## 3. Programmatic Geocoders (Text-to-Geography)

If you need to programmatically search text strings to validate their existence before mapping them:

* **`tidygeocoder`**: The absolute best modern package for address/landmark routing. It provides a unified, tidy interface (`geocode()`) to query powerful North American databases like the US Census API, Nominatim (OSM), or ArcGIS, returning exact longitudes/latitudes for geographic terms.
* **`osmdata` (`getbb()`)**: As utilized in our pipeline, `getbb("Isle Royale")` is fantastic for grabbing raw OpenStreetMap bounding polygons simply by feeding it an explicit text string.

## Recommended Experimental Workflow

If you eventually want to move away from manually editing static CSVs (`huc_features.csv`), the ultimate interactive UI flow combines these tools:

1. Render a `leaflet` basemap in your Shiny App.
2. Add `leaflet.extras::addSearchOSM()` so the user gets a physical search bar interface over the map.
3. The user types "Yellowstone River" into the map's search box. The interactive map zooms to it.
4. You capture the map's current bounding box coordinates via Shiny (`input$map_bounds`) and pass those bounds strictly into our `discover_watershed_features()` utility to pull the exact topographical intersection!

---

## Interactive Spatial Discovery Implementation

**Prompt**: Develop an R/leafletApp.R shiny app to interactively look for geographic features (combining search and click methodologies). Collect functions in R/leaflet.R (documented using Roxygen2). Modify inst/doc/refactor/leaflet.md with the prompt and walkthrough when done.

### Architecture Walkthrough

We designed a unified dual-workflow interface that empowers users to both search via text and interactively click on regions to automatically fetch topographies.

#### 1. Backend Spatial Adapters (`R/leaflet.R`)

We formalized two backend utilities heavily documented using Roxygen2:

* `build_base_map()`: Wraps the `leaflet` map rendering API. We initialized a default view zoomed out over North America, cleanly overlaying standard map tiles. Crucially, we embedded the `leaflet.extras::searchOSM()` widget directly here, enabling instantaneous global geographic string lookups *(Option A paradigm)* without requiring custom API hooks.
* `get_huc_from_point(lng, lat)`: A bridging wrapper that safely ingests decimal geographic coordinates. We utilized `sf::st_point()` to geometrically project the decimals into strict `epsg=4326` CRS bounds, allowing us to flawlessly query point intersections against the `nhdplusTools::get_huc()` subwatershed matrix *(Option B paradigm)*.

#### 2. The Interactive Shiny Application (`R/leafletApp.R`)

Using our standardized modular paradigm, we constructed `leafletApp()` across 4 core blocks:

* **`leafletInput(id)`**: Binds the visual `leafletOutput` UI element and prepares a text UI bridge (`huc_status`) to echo the intersection results to the user interactively.
* **`leafletOutput(id)`**: Left blank currently, preparing structural space if data table breakdowns are ever desired downstream.
* **`leafletServer(id)`**: The reactive core handling map telemetry.
  * We map `mapper` directly to `build_base_map()`, rendering the interactive element locally.
  * We implement a reactive listener explicitly against `input$mapper_click`. If a user interacts with the map (either after searching for "Isle Royale" or simply scrolling), the underlying Javascript fires an event block.
  * Shiny parses the lat/long, renders an interactive HTML status block, and executes `get_huc_from_point()`.
  * Automatically, if a USGS boundary mapping is mathematically returned, we execute a reverse `leafletProxy` call, drawing a semi-transparent `addPolygons()` boundary on the exact map the user clicked natively!

#### Summary

Users are now fully equipped to discover subwatersheds entirely decoupled from string dictionaries! They can launch `leafletApp()`, search for a landmark on the provided widget, click the physical body of water on the map, and instantly extract the underlying USGS HUC12 mapping configurations required for the `ewing` ecosystem simulations locally.

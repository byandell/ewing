# Moose Habitat & Substrate Overlay Utilities

Functions for extracting geographic habitat features that attract moose
(inland lakes, beaver ponds, cool shaded forests, and bogs/wetlands),
geocoding notable sighting landmarks (Washington Creek, Ojibway Lake,
Feldtmann Lake, Hidden Lake), computing habitat suitability weights on
hexagonal substrate grids, and visualizing overlays.

Retrieves key moose sighting landmarks on Isle Royale (or customizable
spatial targets): Washington Creek in Windigo, Ojibway Lake, Feldtmann
Lake, and Hidden Lake in Tobin Harbor.

Generates a spatial hexagonal grid across Isle Royale island geometry
using pre-computed local habitat features
(\`isle_royale_features.rds\`), without requiring external watershed GIS
service calls.

Intersects habitat features (lakes, waterways, forests, bogs) and
sighting landmarks with a hexagonal substrate grid overlay, calculating
habitat suitability weights per hex cell.

## Usage

``` r
get_habitat_features(
  watershed_obj = NULL,
  categories = c("lakes", "waterways", "forests", "bogs"),
  use_cache = TRUE,
  site = "isle_royale"
)

get_moose_landmarks(
  watershed_obj = NULL,
  use_cache = TRUE,
  site = "isle_royale"
)

create_isle_royale_hex_overlay(
  hex_diameter = 0.01,
  features = NULL,
  layer = NULL,
  site = "isle_royale"
)

add_habitat_hex_overlay(
  hex_obj,
  habitat_sf = NULL,
  landmarks_sf = NULL,
  features = NULL,
  landmarks = NULL
)

# S3 method for class 'habitat_hex_overlay'
autoplot(object, show_landmarks = TRUE, ...)

add_leaflet_habitat_overlay(map, object)
```

## Arguments

- watershed_obj:

  Watershed object from \`get_watershed()\`.

- categories:

  Character vector of habitat feature categories to extract.

- use_cache:

  Logical; if TRUE, uses pre-fetched local landmark definitions when
  available.

- site:

  Target simulation landscape/site folder name (default:
  \`"isle_royale"\`).

- hex_diameter:

  Diameter of hexagonal grid cells in degrees (default = \`0.01\`).

- features:

  Optional path or \`sf\` object containing habitat features.

- layer:

  Optional path or \`sf\` boundary layer object.

- hex_obj:

  An S3 object of class \`watershed_hex_overlay\`.

- habitat_sf:

  Optional pre-extracted \`sf\` data frame of habitat features.

- landmarks_sf:

  Optional pre-geocoded \`sf\` points of sighting landmarks.

- object:

  An S3 object of class \`habitat_hex_overlay\`.

- show_landmarks:

  Logical; whether to draw moose sighting POIs.

- ...:

  Additional arguments passed to plotting functions.

- map:

  A \`leaflet\` map object.

## Value

\`get_habitat_features\`: An \`sf\` data frame of habitat features
clipped to the target geometry.

\`get_moose_landmarks\`: An \`sf\` object containing landmark point
geometries and attributes.

An S3 object of class \`watershed_hex_overlay\`.

\`add_habitat_hex_overlay\`: An S3 object of class
\`habitat_hex_overlay\`.

A \`ggplot\` visualization of the Isle Royale Moose Habitat Overlay.

An updated \`leaflet\` map.

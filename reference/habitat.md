# Moose Habitat & Substrate Overlay Utilities

Functions for extracting geographic habitat features that attract moose
(inland lakes, beaver ponds, cool shaded forests, and bogs/wetlands),
geocoding notable sighting landmarks (Washington Creek, Ojibway Lake,
Feldtmann Lake, Hidden Lake), computing habitat suitability weights on
hexagonal substrate grids, and visualizing overlays.

Retrieves key moose sighting landmarks on Isle Royale (or customizable
spatial targets): Washington Creek in Windigo, Ojibway Lake, Feldtmann
Lake, and Hidden Lake in Tobin Harbor.

Intersects habitat features (lakes, waterways, forests, bogs) and
sighting landmarks with a hexagonal substrate grid overlay, calculating
habitat suitability weights per hex cell.

## Usage

``` r
get_habitat_features(
  watershed_obj,
  categories = c("lakes", "waterways", "forests", "bogs"),
  use_cache = TRUE
)

get_moose_landmarks(watershed_obj, use_cache = TRUE)

add_habitat_hex_overlay(hex_obj, habitat_sf = NULL, landmarks_sf = NULL)

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
clipped to the watershed.

\`get_moose_landmarks\`: An \`sf\` object containing landmark point
geometries and attributes.

\`add_habitat_hex_overlay\`: An S3 object of class
\`habitat_hex_overlay\`.

A \`ggplot\` visualization of the Isle Royale Moose Habitat Overlay.

An updated \`leaflet\` map.

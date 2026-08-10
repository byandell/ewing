# Moose Habitat & Substrate Overlay Utilities

Functions for extracting geographic habitat features that attract moose
(inland lakes, beaver ponds, cool shaded forests, and bogs/wetlands),
geocoding notable sighting landmarks (Washington Creek, Ojibway Lake,
Feldtmann Lake, Hidden Lake), computing habitat suitability weights on
hexagonal substrate grids, and visualizing overlays.

Retrieves notable moose sighting locations (Washington Creek, Ojibway
Lake, Feldtmann Lake, Hidden Lake).

Projects a mathematical hexagonal substrate grid overlay across an
extracted watershed or landscape boundary.

Standalone utility to construct the Isle Royale spatial hexagonal
substrate grid.

## Usage

``` r
get_habitat_features(
  watershed_obj = NULL,
  categories = c("lakes", "waterways", "forests", "bogs"),
  use_cache = TRUE,
  site = "isle_royale"
)

get_fallback_habitat_features(huc_layer = NULL)

get_moose_landmarks(
  watershed_obj = NULL,
  use_cache = TRUE,
  site = "isle_royale"
)

add_watershed_hex_overlay(huc_info, hex_diameter = 0.01)

# S3 method for class 'watershed_hex_overlay'
autoplot(object, ...)

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
  landmarks = NULL,
  site = "isle_royale"
)

# S3 method for class 'habitat_hex_overlay'
autoplot(object, show_landmarks = TRUE, ...)
```

## Arguments

- watershed_obj:

  Target spatial watershed or landscape object.

- categories:

  Character vector of habitat feature categories to extract.

- use_cache:

  Logical; if TRUE, uses local pre-fetched dataset when available.

- site:

  Target simulation landscape/site folder name (default:
  \`"isle_royale"\`).

- huc_layer:

  An \`sf\` polygon representation of the region.

- huc_info:

  Watershed object or list containing a \`layer\` geometry.

- hex_diameter:

  Hexagon extent diameter in degrees (default: \`0.01\`).

- object:

  An S3 object of class \`habitat_hex_overlay\`.

- ...:

  Additional arguments passed to plotting functions.

- features:

  Deprecated alias for \`habitat_sf\`.

- layer:

  Optional sf object of landscape boundary layer.

- hex_obj:

  S3 object returned from \`add_watershed_hex_overlay()\` or
  \`create_isle_royale_hex_overlay()\`.

- habitat_sf:

  Optional sf object of habitat features.

- landmarks_sf:

  Optional sf object of sighting landmarks.

- landmarks:

  Deprecated alias for \`landmarks_sf\`.

- show_landmarks:

  Logical; if TRUE, renders moose sighting landmarks on map.

## Value

\`get_habitat_features\`: An \`sf\` data frame of habitat features
clipped to the target geometry.

Fallback \`sf\` data frame of habitat features.

An \`sf\` data frame of point landmark geometries.

\`add_watershed_hex_overlay\`: An S3 object of class
\`watershed_hex_overlay\` containing the original geometry plus the hex
layer.

\`autoplot.watershed_hex_overlay\`: A \`ggplot\` object representing the
spatial mesh.

An S3 object of class \`watershed_hex_overlay\`.

\`add_habitat_hex_overlay\`: An S3 object of class
\`habitat_hex_overlay\`.

A \`ggplot\` object.

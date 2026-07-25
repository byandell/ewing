# Interactive Leaflet Geographic Utilities

Helper utilities for building interactive Leaflet base maps with search
capabilities, reverse-geocoding points to USGS HUC12 subwatershed
boundaries, and rendering spatial hexagonal grid overlays.

## Usage

``` r
build_base_map()

get_huc_from_point(lng, lat)

add_leaflet_hex_overlay(
  map,
  hex_obj,
  hex_color = "#C0392B",
  bound_color = "#2980B9"
)
```

## Arguments

- lng:

  Numeric longitude coordinate

- lat:

  Numeric latitude coordinate

- map:

  A \`leaflet\` map object or \`leafletProxy\` handle.

- hex_obj:

  A \`watershed_hex_overlay\` S3 object (or a list containing \`layer\`
  and \`hex_overlay\` sf objects).

- hex_color:

  Stroke color for hexagonal grid cells (default: "#C0392B").

- bound_color:

  Stroke color for watershed boundary (default: "#2980B9").

## Value

\`build_base_map\`: A \`leaflet\` HTML widget object.

\`get_huc_from_point\`: An \`sf\` polygon representation of the covering
HUC12.

\`add_leaflet_hex_overlay\`: Updated \`leaflet\` map object.

# Watershed Boundary & Hexagonal Substrate Utilities

Retrieves watershed boundary dataset (WBD) geometry for HUC12
identifiers, applies OpenStreetMap feature clipping, generates hexagonal
substrate grid overlays, and discovers named geographical features
within subwatersheds.

## Usage

``` r
get_watershed(huc_id, feature_name = NULL, huc_layer = NULL)

add_watershed_hex_overlay(huc_info, hex_diameter = 0.01)

# S3 method for class 'watershed_hex_overlay'
autoplot(object, ...)

discover_watershed_features(
  huc_id,
  feature_types = c("natural", "waterway", "leisure")
)
```

## Arguments

- huc_id:

  A character string representing the HUC12 identifier.

- feature_name:

  An optional character string specifying a geographic feature to
  restrict the watershed (via \`osmdata\`).

- huc_layer:

  An optional pre-fetched SF object boundary to intercept identical
  querying sequences dynamically.

- huc_info:

  A watershed list returned from \`get_watershed()\`.

- hex_diameter:

  Numeric representing the diameter of the hexagons in CRS units.

- object:

  An S3 object of class \`watershed_hex_overlay\`.

- ...:

  Additional arguments passed to plotting functions.

- feature_types:

  A character vector of OSM keys to query (default: c("natural",
  "waterway", "leisure")).

## Value

\`get_watershed\`: A list containing the \`huc_id\`, \`feature_name\`,
\`lon\` and \`lat\` of the centroid, and \`sf\` \`layer\`.

\`add_watershed_hex_overlay\`: An S3 object of class
\`watershed_hex_overlay\` containing the original geometry plus the hex
layer.

\`autoplot.watershed_hex_overlay\`: A \`ggplot\` object representing the
spatial mesh.

\`discover_watershed_features\`: A character vector of unique feature
names found physically within the watershed bounds.

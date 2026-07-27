# Create Hexagonal Grid Overlay Dataframe

Generates 6-vertex polygon tiles for each lattice dot in a substrate
object.

## Usage

``` r
create_hex_overlay(object, step = 1)
```

## Arguments

- object:

  An S3 object of class \`substrate\` or a dataframe with columns \`x\`,
  \`y\`, \`substrate\`.

- step:

  Numeric grid density spacing interval (default: 1).

## Value

A data.frame suitable for \`geom_polygon(group = cell_id)\`.

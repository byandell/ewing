# Define Tridiagonal Substrate Topology

Constructs the foundational topological mapping linking multiple
substrate component offsets (e.g., fruit vs. leaf elements) into an
interconnected mathematical mesh matrix.

## Usage

``` r
substrate_topology(width = 10, step = 1)
```

## Arguments

- width:

  Integer representing the radius size limits of spatial components.

- step:

  Numeric grid density spacing.

## Value

A list encompassing adjacency parameters \`offset\` and structural
\`dir\` for specific modules.

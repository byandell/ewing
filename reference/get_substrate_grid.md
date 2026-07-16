# Generate a Substrate Abstract Grid

Constructs the analytical coordinate points bounding a standard subset
of a tridiagonal geometric matrix.

## Usage

``` r
get_substrate_grid(width, step = 1, orientation = "up")
```

## Arguments

- width:

  Integer representing the radius spanning across a substrate patch.

- step:

  Integer interval density between geometric dots.

- orientation:

  Character metric "up" or "down" dictating mathematical inversion of
  coordinate arrays.

## Value

A data.frame holding the computed relative tridiagonal mappings (\`a\`,
\`b\`, \`c\`).

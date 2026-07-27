# Interactive Hex Substrate Organism Movement Application

Shiny application built on \`initParServer\`, \`initServer\`, and
\`substrateServer\` that visualizes organism positions on a hexagonal
substrate network using their global tridiagonal coordinates and allows
interactive stepping through simulation events (+1, +10, +100 steps).

## Usage

``` r
hexmoveApp(
  mysim = NULL,
  width = 10,
  step = 1,
  title = "Organism Movement on Hex Grid"
)
```

## Arguments

- mysim:

  Optional pre-initialized \`ewing\` simulation community object. If
  NULL, initializes default simulation.

- width:

  Substrate radius limit (default: 10).

- step:

  Numeric step density spacing interval (default: 1).

- title:

  Application title string.

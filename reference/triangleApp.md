# Interactive Tridiagonal Substrate Network Explorer App

Shiny application to interactively build, visualize, and inspect
tridiagonal geometric substrate spatial networks (`substrate_topology`,
`create_substrate`, `autoplot.substrate`).

## Usage

``` r
triangleApp(
  width = 10,
  step = 1,
  title = "Tridiagonal Substrate Network Explorer"
)
```

## Arguments

- width:

  Integer radius size limit of spatial components (default: 10)

- step:

  Numeric grid density spacing interval (default: 1)

- title:

  Application title

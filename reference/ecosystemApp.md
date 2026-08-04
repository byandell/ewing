# Generalized Ecosystem Simulation App

Interactive platform for exploring spatial predator-prey dynamics over
customizable real-world landscape geography, habitat suitability
features, life stage age classes, and empirical census benchmarks.

## Usage

``` r
ecosystemApp(ecosystem = "isle_royale", title = NULL)

ecosystemInput(id, ecosystem = "isle_royale")

ecosystemOutput(id)

ecosystemServer(id, ecosystem = "isle_royale")
```

## Arguments

- ecosystem:

  Target simulation landscape/site subfolder name under \`extdata/\`
  (default: \`"isle_royale"\`).

- title:

  Application title string.

- id:

  Module ID string.

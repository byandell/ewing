# Interactive Daily Temperature Design Explorer App

Shiny application to interactively adjust daily high and low temperature
splines and visualize degree-day accumulation dynamics for an `ewing`
simulation.

## Usage

``` r
tempApp(community = NULL, title = "Daily Temperature Design Explorer")
```

## Arguments

- community:

  Simulation object of class `ewing` (default:
  [`init.simulation()`](https://byandell.github.io/ewing/reference/init.simulation.md))

- title:

  Application title

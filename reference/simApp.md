# Ewing Simulation Engine App

Central evaluation module directing state flow interactions spanning
across discrete evaluation limits (\$nsim \> 1\$) and linear continuous
event evaluations (\$nsim == 1\$).

## Usage

``` r
simApp(title = "Population Ethology Simulation")

simServer(id, init_par, datafile = shiny::reactiveVal(""))

simInput(id)

simUI(id)
```

## Arguments

- title:

  Application title

- id:

  module ID string

- init_par:

  reactive list mapping initial population sizes

- datafile:

  reactive matrix filepath containing physical environment and
  configuration targets

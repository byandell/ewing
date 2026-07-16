# Ewing Initialization App

Shiny module capturing baseline evaluation traits and populating
parameter inputs dynamically based heavily on external matrix
definitions.

## Usage

``` r
initParApp(title = "Population Ethology")

initParServer(
  id,
  simres = shiny::reactiveVal(NULL),
  datafile = shiny::reactiveVal("")
)

initParInput(id)

initParUI(id)

initParOutput(id)
```

## Arguments

- title:

  Application title

- id:

  module ID string

- simres:

  reactive simulation state used to structure dynamic data grid
  evaluations

- datafile:

  reactive filepath pointing to optional target parameter overwrites

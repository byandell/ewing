# Input Data App

Shiny module for inspecting simulation input parameter data tables,
organism features, stage transition futures, and substrate interaction
matrices.

## Usage

``` r
inputApp(title = "Input Data Explorer")

inputAppInput(id)

inputAppOutput(id)

inputAppServer(
  id,
  simres = shiny::reactiveVal(NULL),
  datafile = shiny::reactiveVal("")
)
```

## Arguments

- title:

  Application title

- id:

  Module ID string

- simres:

  Reactive simulation state (\`ewing\` object)

- datafile:

  Reactive optional datafile path

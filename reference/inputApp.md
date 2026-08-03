# Input Data App

Shiny module for inspecting simulation input parameter data tables,
organism features, stage transition futures, and substrate interaction
matrices.

## Usage

``` r
inputApp(title = "Input Data Explorer")

discover_dataset_tables(datafile = "", sim = NULL)

inputAppInput(id, choices = NULL)

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

- datafile:

  Reactive optional datafile path or string

- sim:

  Simulation instance (\`ewing\` or \`isle_royale_sim\`)

- id:

  Module ID string

- choices:

  Optional vector of initial table choices

- simres:

  Reactive simulation state (\`ewing\` or \`isle_royale_sim\` object)

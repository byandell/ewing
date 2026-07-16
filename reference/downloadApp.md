# Download File App

Shiny application components for exporting evaluation metrics, data
tables, and dynamic plot graphics out of the simulation runtime.

## Usage

``` r
downloadApp(title = "Download Options")

downloadServer(
  id,
  sim_data,
  distplot = shiny::reactive(NULL),
  sppplot = shiny::reactive(NULL),
  envplot = shiny::reactive(NULL)
)

downloadInput(id)
```

## Arguments

- title:

  Application title

- id:

  module ID string

- sim_data:

  reactive list holding the primary \`simApp\` state values (\`simres\`,
  \`nsim\`)

- distplot:

  reactive evaluating to the main age class visualization

- sppplot:

  reactive evaluating to the host/parasite substrate plot grids

- envplot:

  reactive evaluating to the generalized statistical envelopes

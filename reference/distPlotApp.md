# Distribution Plot App

Shiny UI components and module server logic for visualizing population
age-class distribution distributions and demographic structures.

## Usage

``` r
distPlotApp(title = "Population Ethology")

distPlotServer(id, simres)

distPlotInput(id)

distPlotOutput(id)
```

## Arguments

- title:

  Application title

- id:

  module ID string

- simres:

  reactive simulation state holding the active \`ewing\` data object

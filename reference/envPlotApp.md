# Envelope Plot App

Shiny application modules used strictly to render and wrap statistical
envelope probability intervals generated natively by active \`nsim \>
1\` discrete operations.

## Usage

``` r
envPlotApp(title = "Envelope Plots")

envPlotServer(id, simres, nsim)

envPlotInput(id)

envPlotOutput(id)
```

## Arguments

- title:

  Application title

- id:

  module ID string

- simres:

  reactive holding evaluated multi-simulation responses

- nsim:

  reactive integer specifying iterations computed within the \`simres\`
  object

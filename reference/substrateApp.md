# Substrate Plot App

A focused graphical module aggregating structural species progression
separated distinctly across physical environment thresholds mapped by
evaluating top-level topological definitions. Includes support for both
hexagonal grid overlay mapping and classic faceted panels, as well as
simulation stepping.

## Usage

``` r
substrateApp(title = "Substrate Organism Movement Explorer")

substrateInput(id)

substrateServer(id, simres, width = 10, step_density = 1)

substrateOutput(id)
```

## Arguments

- title:

  Application title

- id:

  Module ID string

- simres:

  Reactive object returning an \`ewing\` simulation community object

- width:

  Default substrate radius limit (10)

- step_density:

  Default step density spacing interval (1)

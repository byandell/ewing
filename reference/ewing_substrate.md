# Ewing Substrate by Species

Ewing Substrate by Species with optional hexagonal topology layout,
multi-species support, and substrate-level coordinate rescaling.

## Usage

``` r
ewing_substrate(
  community,
  species,
  headstuff = c(0, "start", sum(to.plot)),
  units = getOrgFeature(community, species[1], "units"),
  right = species[1],
  adj = c(0, 0.5, 1),
  show_sub = NULL,
  step = 0,
  layout = c("facet", "hex"),
  width = 10,
  step_density = 1,
  rescale = TRUE,
  x_var = c("step", "time"),
  ...
)

ggplot_ewing_substrate(
  object,
  xlab = "horizontal",
  ylab = "vertical",
  layout = attr(object, "layout"),
  width = attr(object, "width"),
  step_density = attr(object, "step_density"),
  layers = c("poly", "hex", "organisms", "centers", "labels"),
  ...
)

# S3 method for class 'ewing_substrate'
autoplot(object, ...)
```

## Arguments

- community:

  Simulation community object (\`ewing\` S3 class)

- species:

  Species name or vector of species names (e.g. \`"host"\`,
  \`"parasite"\`, or \`c("host", "parasite")\`)

- headstuff:

  Title parameters

- units:

  Unit labels

- right:

  Right label

- adj:

  Adjustment

- show_sub:

  Substrate filter (defaults to all active substrates)

- step:

  Current step

- layout:

  Display layout (\`"facet"\` or \`"hex"\`)

- width:

  Substrate radius limit (for hex layout)

- step_density:

  Grid density spacing interval (for hex layout)

- rescale:

  Logical; if \`TRUE\` (default for \`"hex"\` layout), rescales organism
  local coordinates so they fit strictly within each substrate patch's
  surface area.

- ...:

  Additional arguments

- object:

  Object of class \`ewing_substrate\`

- xlab:

  Label for x axis (facet layout)

- ylab:

  Label for y axis (facet layout)

- layers:

  Display layers for hex layout: vector containing any of \`"poly"\`,
  \`"hex"\`, \`"organisms"\`, \`"centers"\`, \`"labels"\`

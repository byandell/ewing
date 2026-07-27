# Interactive Leaflet Mapping UI (Output)

Server logic for interactive Leaflet discovery. Returns a list of
reactives (\`huc\`, \`status\`, \`click\`, \`drawn_polygon\`) enabling
Shiny module composition.

## Usage

``` r
leafletOutput(id)

leafletServer(id)

leafletApp()
```

## Arguments

- id:

  Module ID

## Value

A list of reactive objects: \`huc\` (reactiveVal holding discovered
\`sf\` HUC polygon(s)), \`status\` (reactiveVal holding HTML status
message), \`click\` (reactive holding map click details), and
\`drawn_polygon\` (reactiveVal holding user drawn rubberband polygon
sf).

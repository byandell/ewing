# Interactive Leaflet Mapping UI (Input)

Server logic for interactive Leaflet discovery. Returns a list of
reactives (\`huc\`, \`status\`, \`click\`) enabling Shiny module
composition.

## Usage

``` r
leafletInput(id)

leafletOutput(id)

leafletServer(id)

leafletApp()
```

## Arguments

- id:

  Module ID

## Value

A list of reactive objects: \`huc\` (reactiveVal holding discovered
\`sf\` HUC polygon), \`status\` (reactiveVal holding HTML status
message), and \`click\` (reactive holding map click details).

# Interactive Hexagonal Watershed Mapping UI (Input)

UI controls for the hexagonal watershed mapping module. Allows
geographic feature search, HUC12 identification, feature boundary
clipping, and hexagon scaling.

Main visual output panel presenting interactive Leaflet map renderings
(via \`leafletInput\` module composition) and static \`ggplot2\`
autoplots.

Server logic utilizing Shiny module composition by calling
\`leafletServer("map")\` directly to handle interactive map discovery
and HUC identification.

Launches an interactive Shiny application combining Leaflet spatial
feature identification (via \`leafletApp\` module composition), USGS
HUC12 subwatershed boundary lookup, feature area restriction, and
hexagonal substrate grid overlays.

## Usage

``` r
hexmapInput(id)

hexmapOutput(id)

hexmapServer(id)

hexmapApp(title = "Ewing Hexagonal Watershed Projection")
```

## Arguments

- id:

  Module ID

- title:

  Application title string

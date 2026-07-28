# Technical Guide Overview

The **Technical Guide** serves as the comprehensive internal technical
reference manual for the `ewing` package. While the high-level
[Developer
Guide](https://byandell.github.io/ewing/articles/devel_guide/) covers
Shiny module composition and dashboard architecture, this guide details
the deep mathematical foundations, state engine physics, spatial
geometry, thermal spline integration, and GIS watershed overlay
algorithms governing the package.

------------------------------------------------------------------------

## Guide Structure & Technical Index

### I. Core Simulation Engine & State Representation

- **[dataorg.md](https://byandell.github.io/ewing/articles/tech_guide/dataorg.md)**:
  Explains the central `community` simulation state object, organism
  feature matrices, population state arrays, and operational execution
  loops.
- **[refactor.md](https://byandell.github.io/ewing/articles/tech_guide/refactor.md)**:
  Details legacy S3 object model refactoring, event queue data
  structures, and functional evaluation routines.
- **[futures.md](https://byandell.github.io/ewing/articles/tech_guide/futures.md)**:
  Leftist-tree event queue scheduling, competing risk stage transitions,
  active status tracking, and structural development timelines.

### II. Spatial Substrate Geometry & Dispersal Mathematics

- **[triangle.md](https://byandell.github.io/ewing/articles/tech_guide/triangle.md)**:
  Technical specification of the tridiagonal coordinate system
  $`(a, b, c)`$, substrate connectivity, topology definitions
  (`substrate_topology`), and triangular coordinate conversion
  (`tri2car`).
- **[substrate.md](https://byandell.github.io/ewing/articles/tech_guide/substrate.md)**:
  Standardizing `substrateApp` (`substrateInput`, `substrateOutput`,
  `substrateServer`) across `sysetholApp` and `hexmoveApp`, and the
  strategic roadmap for spatial organism movement.
- **[hexmove.md](https://byandell.github.io/ewing/articles/tech_guide/hexmove.md)**:
  Details organism spatial movement on hexagonal substrate grid overlays
  (`hexmoveApp`), per-substrate unit coordinate rescaling
  ($`[0, W_{sub\_S}]`$), and 6-sided polygon tile overlay generation
  (`create_hex_overlay`).

### III. Thermal Regimes, Splines & Degree-Day Integration

- **[time_temp.md](https://byandell.github.io/ewing/articles/tech_guide/time_temp.md)**:
  Interactive temperature-time spline design (`temp.design`, `tempApp`),
  degree-day integration (`activeTemp`), 5-parameter mean value
  sensitivity (`fivePlotApp`, `fiveShowApp`), and Shinylive WebAssembly
  demos.

### IV. GIS Watershed & Geographic Projections

- **[leaflet.md](https://byandell.github.io/ewing/articles/tech_guide/leaflet.md)**:
  Interactive geographic feature search and watershed discovery
  utilities (`leafletApp`, `hexmapApp`, `build_base_map`,
  `get_huc_from_point`, `add_watershed_hex_overlay`).
- **[watershed.md](https://byandell.github.io/ewing/articles/tech_guide/watershed.md)**:
  Spatial projection of simulation grids onto USGS HUC12 subwatershed
  boundaries, polygon clipping, and Leaflet/ggplot watershed overlays
  (`watershed_overlay`).
- **[connect.md](https://byandell.github.io/ewing/articles/tech_guide/connect.md)**:
  Architectural guide for Quarto demo publishing, comparing GitHub Pages
  static hosting, WebAssembly Shinylive, and Posit Connect live R server
  deployment.

### V. Application UI Architecture & Engineering Notes

- **[shineup.md](https://byandell.github.io/ewing/articles/tech_guide/shineup.md)**:
  Design notes and implementation details for upgrading the Systems
  Ethology continuous interactive graphical interface (`sysetholApp`,
  `ewingApp`), sidebar de-cluttering with `conditionalPanel`,
  multi-species hex overlay layouts, and `downloadApp` integration.
- **[notes.md](https://byandell.github.io/ewing/articles/tech_guide/notes.md)**
  &
  **[prompts.md](https://byandell.github.io/ewing/articles/tech_guide/prompts.md)**:
  Architectural scratchpads and AI prompt history used during system
  refactoring.

# Ewing Refactor Documentation

This directory contains crucial architectural notes, tutorials, design logs, and task trackers for the `ewing` project refactoring pipelines.

- **[connect.md](connect.md)**: Architectural guide for Quarto demo publishing, comparing GitHub Pages static hosting and Shinylive WebAssembly with Posit Connect live R server deployment.
- **[dataorg.md](dataorg.md)**: Explains major data structures, operational baselines, and functional execution loops governing the central simulation network. Includes breakdowns of the core `community` object and mapping arrays.
- **[futures.md](futures.md)**: An active status report and task tracker detailing ongoing structural development, Shiny app integration pipelines, and AI refactoring timelines.
- **[hexmove.md](hexmove.md)**: Details organism movement on hexagonal substrate grid overlays (`hexmoveApp`), per-substrate unit triangle coordinate rescaling ($[0, W_{sub\_S}]$), 6-sided polygon tile overlay generation (`create_hex_overlay`), multi-species overlay vs separate adjacent maps, and interactive simulation stepping.
- **[leaflet.md](leaflet.md)**: Interactive geographic feature search and watershed discovery utilities (`leafletApp`, `hexmapApp`, `build_base_map`, `get_huc_from_point`, `add_watershed_hex_overlay`).
- **[notes.md](notes.md)**: Architectural notes and development scratchpads on simulation design and package restructuring.
- **[prompts.md](prompts.md)**: A collection of prompt inputs and contexts used to steer AI tooling behavior.
- **[refactor.md](refactor.md)**: Architectural notes and workflows relating to deeper legacy code restructuring, S3 class functional evaluation, and standard package conventions.
- **[shineup.md](shineup.md)**: Design notes and implementation details for upgrading the Systems Ethology continuous interactive graphical interface (`sysetholApp`, `ewingApp`), sidebar de-cluttering with `conditionalPanel`, multi-species hex overlay layouts, and `downloadApp` integration.
- **[substrate.md](substrate.md)**: Standardizing `substrateApp` module (`substrateInput`, `substrateOutput`, `substrateServer`) across `sysetholApp` and `hexmoveApp`, and strategic roadmap for mapping organism spatial movement onto geographic watershed maps (`hexmapApp`).
- **[time_temp.md](time_temp.md)**: Degree-day time integration, thermal regime splines (`temp.plot`), and temperature conversion utilities (`temp.design`).
- **[triangle.md](triangle.md)**: Technical overview of the tridiagonal coordinate system, substrate connectivity, topology definitions (`substrate_topology`), and triangular grid reconstruction logic.
- **[watershed.md](watershed.md)**: Details spatial projection of simulation grids onto HUC12 watershed boundaries, polygon clipping, and Leaflet/ggplot watershed overlays (`watershed_overlay`).

# AGENTS.md — ewing

## Context

- **Repository**: `ewing` — Individual-based event-driven simulation for
  host-parasite systems on triangular grids (Quantitative Population
  Ethology).
- **Key Directories**: `R/` (simulation engine & Shiny modules),
  `inst/shinyApp/` (standalone apps), `demos/` (Quarto Shinylive demos),
  `data/` (Excel configurations & sample data), `vignettes/` (package
  guide).
- **Core Abstractions**: Leftist tree event queue (`future.events`),
  triangular coordinate system (`tricoord`, `substrate`), S3 classes
  (`ewing`, `ewing_discrete`, `ewing_envelopes`).

## Role

Act as an expert R package developer, ecological modeler, and
Shiny/Shinylive systems architect.

## Action & Verification

- **Package Verification**: Run `devtools::document()`,
  `devtools::test()`, and `devtools::check()`.
- **Shinylive Verification**: When editing `demos/*.qmd`, verify with
  `quarto render demos/`. Ensure `{shinylive-r}` blocks strip roxygen
  comments and load via `webr::install("byandell/ewing")`.
- **Documentation**: Never edit `man/` directly; update roxygen tags in
  `R/` and reference guides in `vignettes/`.

## Format & Conventions

- Modular Shiny structure (`bslib` UI, `moduleServer` server logic, `DT`
  tables).
- Explicit namespacing (`pkg::func()`) across all internal and exported
  functions.
- Relative link hygiene: All markdown/vignette links must use relative
  paths.

## Tone & Collaboration

- Direct, concise, and mathematically precise. Provide complete drop-in
  replacements and verify simulation output locally before declaring
  complete.

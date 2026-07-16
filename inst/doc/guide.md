# Create Developer Guide for Ewing Package

This document records the prompts, architectural decisions, and steps taken to build the Developer Guide for the **ewing** package. It mirrors the process followed for the [qtl2shiny Developer Guide](https://byandell-sysgen.github.io/qtl2shiny/articles/devel_guide/).

---

## 1. Prompts

- Create a Developer Guide for this package that is similar to the
[qtl2shiny Developer Guide](https://byandell-sysgen.github.io/qtl2shiny/articles/devel_guide/)
following the prompts in
[Create Developer’s Guide to qtl2shiny](https://byandell.github.io/Documentation/prompts/devel_guide.html).
Document the process in `inst/doc/guide.md`.
- Migrate this Developer Guide to `vignettes` and set up `pkgdown` following the steps in
[Use pkgdown to Auto-Build GitHub Website](https://byandell.github.io/Documentation/github/pkgdown.html).
Again, document the process in `inst/doc/guide.md`.

---

## 2. Prototype History & References

Prior to writing the full Developer Guide, several individual-module documentation prototypes and refactoring logs existed in [inst/doc/refactor/](file:///Users/brianyandell/Documents/Research/ewing/ewing/inst/doc/refactor/):

- **`refactor/shineup.md`**: Explains modularizing `ewingApp.R` into submodules (`simApp.R`, `initParApp.R`, `distPlotApp.R`, `substrateApp.R`, `envPlotApp.R`, `downloadApp.R`) to resolve caching, datatable render bugs, and open PDF device locks.
- **`refactor/dataorg.md`**: Outlines the core `community` state object and matrix layout mappings.
- **`refactor/triangle.md`**: Documents the tridiagonal coordinate system mathematics and dispersal movements.
- **`refactor/leaflet.md` & `refactor/watershed.md`**: Tracks watershed boundary spatial intersections using USGS APIs.

These design records served as the functional specifications and prototypes used to generate the final guides.

---

## 3. Steps Performed

1. **Workspace Inspection**:
   Analyzed all Shiny modules (`*App.R`) on the system and compared their structures to the legacy dashboard ([origEwingApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/origEwingApp.R) and [multApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/multApp.R)).
2. **Directory Initialization**:
   Created the target folder `inst/doc/devel_guide/` to host the guides.
3. **Index Creation ([README.md](file:///Users/brianyandell/Documents/Research/ewing/ewing/inst/doc/devel_guide/README.md))**:
   Formulated the entry point outlining folder organization, package datasets (`default.xlsx`), and a comprehensive index mapping every Shiny source file to its dashboard roles.
4. **Architecture Documentation ([architecture.md](file:///Users/brianyandell/Documents/Research/ewing/ewing/inst/doc/devel_guide/architecture.md))**:
   Created visual Mermaid charts tracing data flows through `ewingServer` and mapped the parameters exposed by each submodule.
5. **Detailed Panel Sub-Guides**:
    - Written [simulation.md](file:///Users/brianyandell/Documents/Research/ewing/ewing/inst/doc/devel_guide/simulation.md) mapping parameter worksheets loading to active step-wise runs.
    - Written [visualization.md](file:///Users/brianyandell/Documents/Research/ewing/ewing/inst/doc/devel_guide/visualization.md) charting demographic distributions, spatial substrates, and envelopes.
    - Written [utilities.md](file:///Users/brianyandell/Documents/Research/ewing/ewing/inst/doc/devel_guide/utilities.md) detailing download safety wrappers, Leaflet GIS explorer features, and developer inspection widgets.
6. **Vignette Migration**:
   - Relocated files from the `inst/doc/devel_guide/` directory to the `vignettes/devel_guide/` directory to prevent them from being deleted during site builds.
   - Renamed developer guide files in `vignettes/devel_guide/` from `.md` to `.Rmd` and injected standard package vignette YAML headers. This enables `pkgdown` to automatically recognize and build them as articles.
   - Removed the duplicate `inst/doc/devel_guide/` directory to clean up the repository.
7. **Relative Link Adjustments**:
   - Updated the relative links in `vignettes/devel_guide/index.Rmd` (formerly `README.md`) to point to `.html` destinations.
   - Updated the link in root `README.md` to point to the rendered `articles/devel_guide/index.html` file.
8. **pkgdown & CI/CD Setup**:
   - Created `_pkgdown.yml` configuration: Configured website structure, custom navigation dropdown menus under the "Guides" section, and mapped custom articles for the user and developer documentation.
   - Updated `.Rbuildignore`: Configured patterns to ignore the `_pkgdown.yml` configuration, `.github/` folder, `vignettes/devel_guide/` source files, and `docs` output from the R package bundle.
   - Updated `.gitignore`: Added `docs` to ignore the local compiled website output directory.
   - Created `.github/workflows/pkgdown.yaml`: Set up the official `pkgdown` GitHub Action workflow to build the website and deploy it to the `gh-pages` branch on every push.
   - Ran `Rscript -e "pkgdown::build_site()"` locally and successfully built the site, checking references and page links.

---

## 4. Rationale for Module Categorization

To assist developers in navigation, the modules are categorized into three distinct layers:

- **Simulation & Configuration Layer**: Governs input state reading and stepping through stochastic simulations. These modules directly modify the active `community` state.
- **Visual Rendering Layer**: Purely read-only graphics display wrappers. They accept simulation results reactively and render plots using ggplot2.
- **Utility & Companion Layer**: External integration layers (such as saving to disk, mapping geolocations, or inspecting initial states) that operate on top of simulation values.

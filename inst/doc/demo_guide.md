# Shinylive Demos Guide for ewing Package

This document records the structure, WebAssembly patterns, and navigation integration for the interactive **Quarto Shinylive Demos Gallery** in the **ewing** package.

For detailed deployment workflows and general engineering patterns, refer to [Deploy with GitHub Actions](https://byandell.github.io/Documentation/github/actions.html) and [Use pkgdown to Auto-Build GitHub Website](https://byandell.github.io/Documentation/github/pkgdown.html).

---

## 1. Directory Structure

```
ewing/
├── _pkgdown.yml              # Main pkgdown navbar & article layout
├── demos/                    # Quarto Shinylive source project directory
│   ├── _quarto.yml           # Quarto website config (output-dir: ../docs/demos)
│   ├── index.qmd             # Gallery landing page
│   ├── sysetholApp.qmd       # Systems Ethology platform launcher
│   ├── fivePlotApp.qmd       # 5-parameter spline curve sensitivity explorer
│   ├── fiveShowApp.qmd       # Target relative mean binary search utility
│   ├── tempApp.qmd           # Daily temperature-time spline knot editor
│   ├── triangleApp.qmd       # Triangular coordinate substrate geometry demo
│   ├── hexmapApp.qmd         # USGS watershed GIS spatial projection tutorial
│   └── hexmoveApp.qmd        # Hexagonal grid organism dispersal tracker
└── docs/                     # Compiled static website directory published to gh-pages
    ├── .nojekyll             # Disables GitHub Pages Jekyll processing
    ├── index.html            # Main pkgdown site homepage
    └── demos/                # Compiled Quarto Shinylive static site
        ├── index.html        # Demos gallery landing page
        └── *.html            # Compiled application pages
```

---

## 2. WebAssembly Design Pattern (`demos/shinylive_helpers.R`)

All `.qmd` demonstration applications in `demos/` use the targeted **`render_standalone_app()`** architecture defined in [`shinylive_helpers.R`](../demos/shinylive_helpers.R). Rather than forcing webR to download `byandell/ewing` along with 120+ heavy R dependencies (such as `sf`, `leaflet`, and `readxl`) via `webr::install()`, this pattern dynamically bundles only the specific R source files required for each interactive application using standard pre-installed webR libraries (`shiny`, `bslib`, `ggplot2`, `splines`, `stats`, `graphics`).

```r
```{r, echo=FALSE, results='asis'}
source("shinylive_helpers.R")
render_standalone_app("sysetholApp", height = 880)
```
```

### Key Technical Details

1. **Roxygen2 Protection & Vector Safety**:
   To prevent Pandoc JSON string serialization errors during Quarto rendering, `shinylive_helpers.R` strips all `#'` roxygen docstrings before outputting code blocks:
   ```r
   lines <- lines[!grepl("^\\s*#'", lines)]
   ```
   This uses `!grepl(...)` (rather than `!grep(...)`) to strictly follow R vector subsetting safety.

2. **Auto-Embedded Default Data Tables**:
   For simulation apps (`sysetholApp` and `hexmoveApp`), `shinylive_helpers.R` reads configuration tables from `data/*.txt` (`organism.features`, `future.host`, `future.parasite`, `substrate.*`, `temperature.*`, `host.parasite`, `redscale`) and embeds them as pre-parsed data frames via `dput()` directly in `.GlobalEnv`. When `init.simulation()` or `mydata()` runs in the browser, all datasets are immediately available offline.

3. **Zero Network & Installation Overhead**:
   Applications initialize in milliseconds in the browser with 100% code parity and zero network dependency download delays.

---

## 3. Navigation Design

- **Main Site Navbar (`_pkgdown.yml`)**: Displays **Demos** prominently (`href: demos/index.html`).
- **Demos Return Navigation (`demos/_quarto.yml`)**: Points **Home** back to `../index.html` for seamless navigation back to the main `pkgdown` portal.

For publishing troubleshooting (such as Jekyll 404 resolutions and `.nojekyll` setup), see [pkgdown Guide](https://byandell.github.io/Documentation/github/pkgdown.html).

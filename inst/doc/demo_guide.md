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

## 2. WebAssembly Design Pattern (`webr::install`)

All `.qmd` demonstration applications in `demos/` use `webr::install()` to load the package namespace directly in browser WebAssembly (`webR`):

```r
```{shinylive-r}
#| standalone: true
#| viewerHeight: 880
#| components: [viewer]

webr::install("byandell/ewing")
library(ewing)

sysetholApp()
```
```

This ensures 100% code parity between package functions and browser applications without code duplication.

---

## 3. Navigation Design

- **Main Site Navbar (`_pkgdown.yml`)**: Displays **Demos** prominently (`href: demos/index.html`).
- **Demos Return Navigation (`demos/_quarto.yml`)**: Points **Home** back to `../index.html` for seamless navigation back to the main `pkgdown` portal.

For publishing troubleshooting (such as Jekyll 404 resolutions and `.nojekyll` setup), see [pkgdown Guide](https://byandell.github.io/Documentation/github/pkgdown.html).

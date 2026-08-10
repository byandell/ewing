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
│   └── hexmoveApp.qmd        # Hexagonal grid organism dispersal tracker
└── docs/                     # Compiled static website directory published to gh-pages
    ├── .nojekyll             # Disables GitHub Pages Jekyll processing
    ├── index.html            # Main pkgdown site homepage
    └── demos/                # Compiled Quarto Shinylive static site
        ├── index.html        # Demos gallery landing page
        └── *.html            # Compiled application pages
```

---

## 2. WebAssembly Design Pattern & `shinylive_helpers.R` Advice

Rather than requiring `webr::install()` to download heavy compiled dependencies (such as `sf`, `leaflet`, and `readxl`) inside the browser, demonstration applications use the targeted **`render_standalone_app()`** helper in [`shinylive_helpers.R`](../demos/shinylive_helpers.R):

```r
```{r, echo=FALSE, results='asis'}
source("shinylive_helpers.R")
render_standalone_app("sysetholApp", height = 880)
```
```

### Best Practices & Advice

1. **Bypass Heavy Package Downloads**:
   Bundle only pure R source files required for each specific app. Rely on standard pre-installed webR packages (`shiny`, `bslib`, `ggplot2`, `splines`, `stats`, `graphics`) for instant load times.

2. **Robust Working Directory Resolution**:
   Detect `R/` and `data/` paths dynamically to ensure rendering succeeds whether invoked from root or `demos/`:
   ```r
   r_dir <- if (dir.exists("R")) "R" else if (dir.exists("../R")) "../R" else file.path("..", "..", "R")
   ```

3. **Strip Roxygen Comments**:
   Prevent Pandoc JSON serialization errors by stripping docstring comments using safe vector evaluation (`!grepl`):
   ```r
   lines <- lines[!grepl("^\\s*#'", lines)]
   ```

4. **Auto-Embed Default Datasets**:
   Serialize required configuration tables (`data/*.txt`) via `dput()` directly into `.GlobalEnv` so simulation routines (`init.simulation()`, `mydata()`) execute offline seamlessly.

5. **Pixel Specifications for Graphic Outputs**:
   Use standard CSS pixel strings (`height = "400px"`, `height = "500px"`) in `shiny::plotOutput()` rather than inch strings (e.g. `"4in"`) to prevent webR graphic device initialization errors (`invalid 'width' argument`).

6. **Un-name Numeric Input Values**:
   Cast named numeric vectors using `as.numeric(...)[1]` when setting reactive states or calling `updateNumericInput()` to prevent `jsonlite` from serializing named vectors as JSON objects (`[object Object]`).

---

## 3. Navigation Design

- **Main Site Navbar (`_pkgdown.yml`)**: Displays **Demos** prominently (`href: demos/index.html`).
- **Demos Return Navigation (`demos/_quarto.yml`)**: Points **Home** back to `../index.html` for seamless navigation back to the main `pkgdown` portal.

For publishing troubleshooting (such as Jekyll 404 resolutions and `.nojekyll` setup), see [pkgdown Guide](https://byandell.github.io/Documentation/github/pkgdown.html).

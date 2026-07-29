# GitHub Actions & Publishing Guide for ewing

This document outlines the deployment workflow and package-specific configurations for publishing the **ewing** package documentation, vignettes, and interactive serverless Quarto Shinylive applications to GitHub Pages.

For universal guidelines and architecture details on deploying R packages and Quarto Shinylive apps, refer to the [General GitHub Actions Guide](https://byandell.github.io/Documentation/github/actions.html) and [pkgdown Guide](https://byandell.github.io/Documentation/github/pkgdown.html).

---

## 1. Published URLs

When changes are pushed to `main`, GitHub Actions automatically builds and deploys the site:

| Section | URL | Description |
| :--- | :--- | :--- |
| **Main Site & Vignettes** | `https://byandell.github.io/ewing/` | `pkgdown` package documentation & vignettes. |
| **Demos Gallery Index** | `https://byandell.github.io/ewing/demos/index.html` | Interactive Shinylive gallery homepage. |
| **sysetholApp** | `https://byandell.github.io/ewing/demos/sysetholApp.html` | Systems Ethology platform launcher. |
| **fivePlotApp** | `https://byandell.github.io/ewing/demos/fivePlotApp.html` | Spline curve sensitivity explorer. |
| **fiveShowApp** | `https://byandell.github.io/ewing/demos/fiveShowApp.html` | Target relative mean search utility. |

---

## 2. Package-Specific Configurations & Diagnosis

- **WebAssembly Font Compatibility**: `bslib::font_google()` relies on native `libcurl` network calls which crash inside `webR`. In `ewing` apps, Google Fonts are loaded via CSS `<link>` imports in `shiny::tags$head`.
- **Server-Side Build**: All WebAssembly assets (`docs/demos/`) are built dynamically by CI/CD and are kept untracked in `.gitignore`.
- **Workflow Location**: Pipeline logic is defined in [.github/workflows/pkgdown.yaml](../../.github/workflows/pkgdown.yaml).

For workflow diagrams, full YAML examples, WASM security requirements (`embed-resources: false`), and `.nojekyll` configuration details, see [GitHub Actions Documentation](https://byandell.github.io/Documentation/github/actions.html).

---

## 3. Configuration Snapshots

### `demos/_quarto.yml`
```yaml
project:
  type: website
  output-dir: ../docs/demos

engine: knitr

website:
  title: "ewing Demos"
  navbar:
    left:
      - href: ../index.html
        text: Home
      - href: sysetholApp.qmd
        text: sysetholApp
      - href: fivePlotApp.qmd
        text: fivePlotApp
      - href: fiveShowApp.qmd
        text: fiveShowApp
    right:
      - icon: github
        href: https://github.com/byandell/ewing

format:
  html:
    theme: cosmo
    toc: true
    toc-depth: 2
    embed-resources: false
    self-contained: false

filters:
  - quarto-ext/shinylive
```

### `_pkgdown.yml`
```yaml
navbar:
  structure:
    left: [intro, reference, demos, articles, news]
    right: [search, github]
  components:
    demos:
      text: Demos
      href: demos/index.html
```

---

## 4. Local Preview

To build and preview `ewing` docs and demos locally:

```bash
Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)"
mkdir -p docs/demos && cd demos && quarto render && cd ..
touch docs/.nojekyll
python3 -m http.server 8000 --directory docs
```

See [Local Verification](https://byandell.github.io/Documentation/github/actions.html#local-verification--server-testing) for troubleshooting details.

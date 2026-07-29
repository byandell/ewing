# Building & Publishing Shinylive Demos for Ewing Package

This document records the architecture, design patterns, build processes, publishing challenges, and navigation solutions for building and deploying the interactive **Quarto Shinylive Demos Gallery** for the **ewing** R package.

---

## 1. Demos Gallery Overview & Architecture

The **ewing** Demos Gallery hosts serverless interactive applications running completely client-side in the user's web browser using **Shinylive** (WebAssembly / `webR`). This allows users and reviewers to explore full simulation models, spatial substrate networks, thermal splines, and parameter search utilities without requiring a backend R server or Posit Connect deployment.

### File & Directory Structure

```
ewing/
├── _pkgdown.yml              # Main pkgdown site navbar & article layout
├── demos/                    # Quarto Shinylive source project directory
│   ├── _quarto.yml           # Quarto website configuration (output-dir: ../docs/demos)
│   ├── index.qmd             # Gallery landing page with interactive grid listing
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
        ├── site_libs/        # Shinylive & Bootstrap WebAssembly JS/CSS assets
        └── *.html            # Compiled application pages
```

---

## 2. Key Design Patterns & Engineering Solutions

### A. Dynamic R Source File Inlining (`inc_files`)
To eliminate code and data duplication between package source files (`R/*.R`, `data/*.txt`) and the serverless WebAssembly Shinylive documents, all `.qmd` demonstration applications use dynamic `results='asis'` knitr code blocks.

```r
```{r, echo=FALSE, results='asis'}
inc_files <- c("R/triangle.R", "R/community.R", "R/Org.R", 
               "R/substrate_triangle.R", "R/ewing_substrate.R", "R/sysetholApp.R")

cat("```{shinylive-r}\n")
cat("#| standalone: true\n")
cat("#| viewerHeight: 880\n\n")

cat("library(shiny)\nlibrary(bslib)\nlibrary(ggplot2)\n\n")

# Inlining R source files directly into Shinylive block
for (f in inc_files) {
  if (file.exists(f)) {
    cat(paste0("# --- Inlined: ", f, " ---\n"))
    cat(readLines(f), sep = "\n")
    cat("\n\n")
  }
}

cat("shinyApp(ui = sysetholUI(), server = function(input, output, session) { sysetholServer(...) })\n")
cat("```\n")
```
```

### B. Non-Self-Contained WebAssembly Output (`embed-resources: false`)
Shinylive applications execute browser WebAssembly (`webR`) workers and service workers (`shinylive-sw.js`). Modern browser security models block WebWorkers when embedded inside standalone data URIs. Setting `embed-resources: false` in `demos/_quarto.yml` ensures Quarto emits modular static web files (`.html`, `.js`, `.css`) compatible with browser security requirements.

---

## 3. Publishing Challenges & Jekyll 404 Resolution

### Problem Statement
When navigating to `https://byandell.github.io/ewing/demos/` on GitHub Pages, users received a 404 error:
> **Page not found (404)**
> Content not found. Please use links in the navbar.

### Root Cause Diagnosis
1. **Default Jekyll Processing**: By default, GitHub Pages processes all deployed repositories using the Jekyll site generator.
2. **Ignored Directories**: Jekyll ignores directories and files starting with `_` (such as Quarto's `demos/_quarto.yml`, `demos/_extensions/`, `docs/demos/_metadata.yml`, and `site_libs/`).
3. **Liquid Template Errors**: Jekyll attempts to parse compiled WebAssembly JavaScript and HTML files as Liquid templates, causing build failures.
4. **Fallback Behavior**: When Jekyll fails or omits `demos/`, GitHub Pages returns pkgdown's custom `404.html` page.

### Solution: CI/CD Jekyll Disablement
Update the GitHub Actions deployment workflow ([.github/workflows/pkgdown.yaml](../../.github/workflows/pkgdown.yaml)) to explicitly create `docs/.nojekyll` prior to deploying to the `gh-pages` branch:

```yaml
      - name: Render Quarto Demos
        run: |
          mkdir -p docs/demos
          cd demos
          quarto add quarto-ext/shinylive --no-prompt
          quarto render
        shell: bash

      - name: Disable Jekyll for GitHub Pages
        run: touch docs/.nojekyll
        shell: bash

      - name: Deploy to GitHub pages 🚀
        if: github.event_name != 'pull_request'
        uses: JamesIves/github-pages-deploy-action@v4.8.0
        with:
          clean: false
          branch: gh-pages
          folder: docs
```

---

## 4. Navigation Design & Cross-Site Links

### A. Main Site Navbar Prominence ([_pkgdown.yml](../../_pkgdown.yml))
To make the Demos Gallery easily discoverable, **Demos** is placed directly on the top navbar of the main package website before **Guides**:

```yaml
navbar:
  structure:
    left:  [intro, reference, demos, articles, news]
    right: [search, github]
  components:
    demos:
      text: Demos
      href: demos/index.html
    articles:
      text: Guides
      menu:
        - text: "User Guides"
        - text: "Tutorial Vignette"
          href: articles/ewing.html
        - text: "-------"
        - text: "Developer Guide"
          href: articles/devel_guide/index.html
        - text: "Technical Guide"
          href: articles/tech_guide/index.html
```

### B. Demos Site Return Navigation ([demos/_quarto.yml](../../demos/_quarto.yml))
To allow users viewing any demo to seamlessly navigate back to the main pkgdown site homepage, the **Home** link in Quarto's navbar points directly to `../index.html`:

```yaml
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
      - href: tempApp.qmd
        text: tempApp
      - href: triangleApp.qmd
        text: triangleApp
      - href: hexmapApp.qmd
        text: hexmapApp
      - href: hexmoveApp.qmd
        text: hexmoveApp
    right:
      - icon: github
        href: https://github.com/byandell/ewing
```

---

## 5. Local Build & Verification Workflow

To test both the pkgdown package documentation and Quarto Shinylive demos locally before pushing:

```bash
# 1. Build pkgdown documentation site
Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)"

# 2. Render Quarto Shinylive demos into docs/demos
mkdir -p docs/demos
cd demos
quarto render
cd ..

# 3. Create .nojekyll file
touch docs/.nojekyll

# 4. Preview locally using a local HTTP server
python3 -m http.server 8000 --directory docs
```

Navigating to `http://localhost:8000/` displays the main pkgdown site with the **Demos** tab, and clicking **Demos** smoothly loads the WebAssembly gallery with **Main Site** return navigation.

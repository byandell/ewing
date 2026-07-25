# Quarto Demos & Publishing Deployment: GitHub Pages vs. Posit Connect

## Prompt

- **User Request**: Can we create a `demos/hexmapApp.qmd`? Will it publish with GitHub Pages or do we need to use Posit Connect?

---

## Response & Architecture Overview

Yes, we created `demos/hexmapApp.qmd` and integrated it into the demo gallery navbar ([`demos/_quarto.yml`](../../demos/_quarto.yml)) and gallery index ([`demos/index.qmd`](../../demos/index.qmd)).

### Publishing Architecture Breakdown

#### 1. GitHub Pages (Static Quarto Website)

- **Publishing Outcome**: **Yes, `demos/hexmapApp.qmd` publishes cleanly on GitHub Pages!**
- **Mechanism**: Running `quarto render` compiles `demos/hexmapApp.qmd` into static HTML (`docs/demos/hexmapApp.html`), rendering workflow tutorials, code snippets, static `ggplot2` autoplots, and embedded links.
- **Client-Side Shinylive Comparison**:
  - Other demos in `demos/` (such as `triangleApp.qmd`, `fivePlotApp.qmd`, `tempApp.qmd`) utilize **Shinylive** (serverless WebAssembly/webR running R code completely client-side in the browser).
  - However, `hexmapApp` relies on compiled C++ spatial libraries (`sf` / GDAL / GEOS) and makes live HTTP network requests to external USGS (`nhdplusTools`) and OpenStreetMap (`osmdata`) APIs. Because browser WebAssembly sandboxes restrict arbitrary CORS API queries and native C++ GIS drivers, the live Shiny app cannot run client-side via Shinylive alone on GitHub Pages.

#### 2. Posit Connect (Live Interactive Application)

- **Publishing Outcome**: **Required for hosting the live, interactive server application.**
- **Mechanism**: The live interactive Shiny application (`hexmapApp()`) runs on **Posit Connect** (e.g., [SystemsEthology](https://connect.doit.wisc.edu/SystemsEthology)) or **shinyapps.io**, where an R server process executes spatial intersections (`sf`) and fetches live USGS / OpenStreetMap data over HTTP.
- **Integration**: `demos/hexmapApp.qmd` links directly to the live Posit Connect deployment so users reading the GitHub Pages documentation can seamlessly launch the interactive app.

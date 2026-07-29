
All Shinylive WebAssembly demonstration applications and documentation have been refactored to the standard **`webr::install()`** architecture.

### Summary of Changes

1. **Refactored Quarto Shinylive Demo Applications (`demos/*.qmd`)**:
   Replaced all `knitr` `readLines()` / `inc_files` code-splicing blocks across all 6 demo documents ([sysetholApp.qmd](../demos/sysetholApp.qmd), [fivePlotApp.qmd](../demos/fivePlotApp.qmd), [fiveShowApp.qmd](../demos/fiveShowApp.qmd), [tempApp.qmd](../demos/tempApp.qmd), [triangleApp.qmd](../demos/triangleApp.qmd), [hexmoveApp.qmd](../demos/hexmoveApp.qmd)) with standard, 5-line `webr::install()` blocks:

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

2. **Updated Technical Guide Vignette ([ui.Rmd](../vignettes/tech_guide/ui.Rmd))**:
   Updated Section 1 (Serverless WebAssembly Integration) to document the `webr::install("byandell/ewing")` pattern and function launcher mappings.

3. **Updated Package Meta-Documentation ([demo_guide.md](../inst/doc/demo_guide.md) & [tech_guide.md](../inst/doc/tech_guide.md))**:
   Updated the design pattern sections to reflect the clean `webr::install()` pattern.

4. **Pushed to GitHub ([4aff1a6](https://github.com/byandell/ewing/commit/4aff1a6))**:
   Pushed commit `4aff1a6` to `master`. GitHub Actions is building and deploying the simplified static pages to `gh-pages`.

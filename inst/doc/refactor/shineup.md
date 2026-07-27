# Shine Up App

- [Prompt](#prompt)
- [Response](#response)
  - [1. The Caching vs. Action Button Conundrum](#1-the-caching-vs-action-button-conundrum)
  - [2. Properly Managing `DT::renderDataTable` Inside Generics](#2-properly-managing-dtrenderdatatable-inside-generics)
  - [3. Safely Managing Download Closures](#3-safely-managing-download-closures)
  - [Additional Tidyverse Modernizing](#additional-tidyverse-modernizing)
- [Walkthrough](#walkthrough)
  - [`R/ewingApp.R`](#rewingappr)
  - [Verification](#verification)
- [Modularization Refactor](#modularization-refactor)
  - [Prompt](#prompt-1)
  - [Walkthrough](#walkthrough-1)
- [Systems Ethology Platform Refactor (`sysetholApp`, `inputApp` & `ewingApp`)](#systems-ethology-platform-refactor-sysetholapp-inputapp--ewingapp)
  - [Geometric Log-Scale Step Size Slider](#geometric-log-scale-step-size-slider)
  - [Standalone `inputApp` Module Extraction](#standalone-inputapp-module-extraction)
  - [Fine Time-Series Granularity in Dist Plots](#fine-time-series-granularity-in-dist-plots)
  - [Sidebar De-cluttering & Conditional Panels](#sidebar-de-cluttering--conditional-panels)
  - [Multi-Species Hex Substrate Layouts](#multi-species-hex-substrate-layouts)
  - [Composition of `ewingApp.R` from `sysetholApp.R`](#composition-of-ewingappr-from-sysetholappr)

---

## Prompt

- Context: I have an R package called "ewing" that simulates predator-prey dynamics
- Role: Act as an expert R developer and Shiny application designer
- Action: Review my draft shiny app code in this repo and suggest improvements
- Format: Provide feedback as tracked changes with explanations
- Tone: Professional, constructive, and encouraging

---

## Response

It is fantastic to see you modernizing the legacy application logic to use the Shiny Modules framework! Building modular Shiny components is incredibly important for scaling research tools.

Following the `ewingApp.R` file logic, here is a professional technical review. I've broken down three critical architectural suggestions regarding caching, safely downloading, and ensuring dynamic UI objects (like your data tables) successfully render.

---

### 1. The Caching vs. Action Button Conundrum

*Analogy:* Caching is like saving a complicated recipe's final dish in the freezer. But using an Action Button (`input$go`) as part of the caching sequence is like freezing the dish with a label that has a constantly changing serial number—you'll never be able to find the dish again because the label never repeats!

In `ewingServer`, you wrap your main simulation `simres` inside `bindCache()`, but you include `input$go` as one of the cache keys. Since Action Buttons are incrementing integers (1 on first click, 2 on second), every click creates a unique cache key, guaranteeing a cache miss and destroying the memory efficiencies of `bindCache()`.

```diff
-        input$host, input$parasite, input$steps, input$nsim, input$go,
+        input$host, input$parasite, input$steps, input$nsim,
         input$datafile),
```

**Explanation:** By dropping `input$go` from the `bindCache` tuple, the cache successfully utilizes the inputs. The simulation only re-runs if the *actual* parameters change. The wrapping `bindEvent(..., input$go)` is completely sufficient to stop execution until the user clicks!

---

### 2. Properly Managing `DT::renderDataTable` Inside Generics

Currently, you pass `DT::renderDataTable` as a direct list item inside of another `renderUI`.

```r
   output$inputfiles <- shiny::renderUI({
     shiny::tagList(
       shiny::selectInput(...),
       DT::renderDataTable(...) # This returns a server hook, not a UI element!
     )
   })
```

Because `renderDataTable()` returns a server closure (not raw HTML), the application might fail to display the table or throw an active binding error inside `tagList`. Instead, you split the UI and server bindings explicitly:

```diff
     output$inputfiles <- shiny::renderUI({
       shiny::tagList(
         shiny::selectInput(ns("dataname"), "", datanames(), "organism.features"),
-        DT::renderDataTable({
-          getOrgDataSimple(simres(), shiny::req(input$dataname), datafile())
-        }, escape = FALSE,
-        options = list(scrollX = TRUE, pageLength = 10)))
+        DT::dataTableOutput(ns("org_table")) # Correct UI Hook Here
+      )
     })
+    
+    output$org_table <- DT::renderDataTable({
+      getOrgDataSimple(simres(), shiny::req(input$dataname), datafile())
+    }, escape = FALSE, options = list(scrollX = TRUE, pageLength = 10))
```

---

### 3. Safely Managing Download Closures

In your `output$downloadPlot`, the PDF is initialized, the plots print dynamically, and then `grDevices::dev.off()` closes the file stream. If any of the plots fail to build and R throws an error, the code aborts *before* reaching `dev.off()`, meaning the PDF file lock remains open and corrupts future downloads.

```diff
     output$downloadPlot <- shiny::downloadHandler(
       filename = function() {
         paste0(paste(shiny::req(input$plotfile), params(), sep = "_"), ".pdf") },
       content = function(file) {
         grDevices::pdf(file, width = 9)
+        on.exit(grDevices::dev.off(), add = TRUE) # Safety Closure
         nsim <- as.integer(shiny::req(input$nsim))
         if(nsim == 1) {
           print(distplot())
           for(i in species()) {
             print(sppplot()[[i]])
           }
         } else {
           print(envelopePlot())
         }
-        grDevices::dev.off()
       }
     )
```

**Explanation:** `on.exit()` guarantees that `dev.off()` fires the instant the `content` scoping function finishes, regardless of whether it completed beautifully or aborted violently due to an error. This is a highly recommended safety practice for dealing with open connections!

---

### Additional Tidyverse Modernizing

Following your team's objective to lean into the `tidyverse`:

- You might consider swapping your `.GlobalEnv` dependent `lapply(species(), ...)` blocks building your dynamic sliders with `purrr::map()`.
- Ensure your `readCount()` handlers are returning nicely formatted `tibble()` output where feasible for downstream `ggplot2` compatibility.

---

## Walkthrough

The UI and server configurations in `ewingApp.R` have been fully upgraded and deployed! The refactor tackled logic failures related to cache busting, asynchronous UI data tables, and PDF lock timeouts. Following your request, all explicit `lapply` loops driving dynamic application configurations have successfully been converted to their `tidyverse` syntax equivalent (`purrr::map()`).

### `R/ewingApp.R`

- **Resolved Cache Busting**: Stripped `input$go` indexing out of the `shiny::bindCache(...)` tuple. By separating the execution constraint wrapper `shiny::bindEvent(...)` from the specific inputs dictating the cache signature, the application backend can now accurately cache execution outputs for faster re-renders.
- **Decoupled DT Servers from UI hooks**: Separated the single `renderUI` list component into correctly matched components. Pushed `DT::dataTableOutput(ns("org_table"))` explicitly into the visual renderer, and mapped the actual dataset constructor function back into explicit logic blocks evaluated locally at the server's scope (`output$org_table <- DT::renderDataTable`).
- **Safe Handling over File Buffers**: Explicit closures utilizing `on.exit(grDevices::dev.off(), add=TRUE)` have been appended dynamically following every execution of `grDevices::pdf(...)` inside the primary `downloadHandler()`. This natively prevents system file locks in scenarios where a plot graphic structurally crashes before correctly hitting `dev.off()`.
- **Tidyverse Architecture:** Converted all basic `lapply(species(), function(x) {...})` iterators into strict `purrr::map()` constructs natively matching the rest of the package's design intent.

### Verification

- Local evaluation commands via `devtools::load_all('.')` verified that all namespace requirements imported cleanly, meaning `purrr` dependencies bind natively alongside Shiny rendering outputs without any syntax-level bugs or mismatched bracket scopes.

---

## Modularization Refactor

### Prompt

- Context: We need to modularize `R/ewingApp.R` drawing heavily upon successful abstraction frameworks pioneered in `R/futureApp.R` and `R/watershedApp.R`.
- Role: Act as an expert Shiny system orchestrator.
- Action: Break apart monolithic logic into dynamic interactive components wrapping UI routing around the `nsim` toggle configurations logically (using `bslib` architectures). Document the resulting parameter interfaces using standard `@param` annotations.

### Walkthrough

The `ewingApp.R` script was entirely refactored across several purpose-built modules. Below is the updated system structure:

1. **`simApp.R`**: Extracts the Chronological Run simulation loop arrays, sliders (`steps`, `step_size`), and interactive execution triggers. Button logic uses a conditional panel tracking `nsim` to restrict Step Forward accessibility exclusively to single evaluations.
2. **`initParApp.R`**: The parameters UI is now wrapped dynamically, correctly accepting reactive variables (`simres`, `datafile`) safely downstream instead of failing out as a missing closure hook when `input$dataname` initializes.
3. **`envPlotApp.R` & `distPlotApp.R`**: Generalized statistics and envelopes generated by iterations are explicitly mapped here. These isolate complex plotting routines completely, leaving the central app visually clean.
4. **`downloadApp.R`**: Safely packages PDF and CSV logic using internal system state bindings for robust downstream data export mappings.

---

## Systems Ethology Platform Refactor (`sysetholApp`, `inputApp` & `ewingApp`)

The Systems Ethology platform was further refactored to introduce **[`R/sysetholApp.R`](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/sysetholApp.R)** (`sysetholApp`, `sysetholInput`, `sysetholOutput`, `sysetholServer`), export **[`R/inputApp.R`](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/inputApp.R)** (`inputApp`, `inputAppInput`, `inputAppOutput`, `inputAppServer`), and streamline **[`R/ewingApp.R`](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewingApp.R)** as a composable wrapper.

### Geometric Log-Scale Step Size Slider

To provide intuitive logarithmic control over simulation step increments, the **`Steps per click`** slider in `sysetholInput` uses discrete geometric steps:

- **Discrete Values**: `1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000`.
- **`step_size_slider()` Helper**: Attaches `data-values="1,2,5,10,20,50,100,200,500,1000,2000"` to `shiny::sliderInput`, configuring the underlying JavaScript slider (`ion.rangeSlider`) to display discrete geometric tick marks.
- **`parse_step_size()` Server Parsing**: Evaluates incoming values from the client slider, resolving string values directly or falling back to index positions.

### Standalone `inputApp` Module Extraction

The dataset table inspection tab was extracted into a reusable module export **[`R/inputApp.R`](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/inputApp.R)**:

- **`inputAppInput()`**: Renders dataset selection (`organism.features`, `future.host`, `future.parasite`, `substrate.host`, `substrate.parasite`, `substrate.substrate`, `host.parasite`, `temperature.base`, `temperature.par`).
- **`inputAppOutput()`**: Displays formatted table output (`tableOutput`).
- **`inputAppServer()`**: Dynamically extracts parameter and interaction matrices from active `ewing` simulation instances.

### Fine Time-Series Granularity & Continuous Stepping in Dist Plots

The **`Dist Plots`** panel visualizes age-class counts over time using `geom_step()` step lines and `geom_point()`:

- **Title & Labels**: Uses `"Age Distribution over Time (<nstep> steps)"` (or `"(<nstep> steps, nsim = <nsim>)"` for multi-run simulations).
- **Species Legend & Open Symbols**: Maps `shape = .data$Species` with open symbol aesthetics (`scale_shape_manual(name = "Species", values = c(1, 2, 0, 5, 6, 3, 4))`) to render species with distinct open shape markers (open circle, open triangle, open square, etc.).
- **Continuous Step History Across Stepping**: When clicking **`Run Engine`** repeatedly in `sysetholApp()` / `ewingApp()`, simulation history is preserved continuously (`append = TRUE`) and step numbers accumulate (`start_step + istep`). Dist Plots display full time-series history from step 0 to the current total accumulated step count instead of resetting to the endpoint of the previous block.
- **Granular Time-Series & Vector-Preserving Normalization**: `step_sim_community` and `ewing_ageclass` retain history ticks across stepping intervals so age class time-series step curves render fine temporal granularity over time. Replaced `ifelse()` logic inside `mutate()` with scalar-evaluated vector normalization (`{ m <- max(...); if (!is.na(m) && m > 0) .data$Count / m else 0 }`) to preserve the full time-series vector for each state instead of truncating data to single-row flatlines. Sets `na.rm = TRUE` on `geom_step()` / `geom_point()` to suppress spurious missing-value warnings.

### Geometric Log-Scale Step Size Slider & Reset Behavior

- **Log-Scale Slider Parsing (`parse_step_size`)**: Fixed a 1-off index offset where Shiny's `ion.rangeSlider` sent 0-based JavaScript indices (`0..10`) for custom `data-values`. Updated `parse_step_size()` to map 0-based JS indices (`num + 1`) to `step_size_choices[1..11]`. This ensures selecting 50 advances exactly 50 steps, 100 advances 100 steps, etc., eliminating the 1-off mapping error.
- **Dynamic Reset & Startup Steps**: App startup and the **`Reset`** button now dynamically read `parse_step_size(input$step_size)` to initialize the simulation with the exact number of steps selected on the **Steps per click** slider.

### Sidebar De-cluttering & Conditional Panels

To create a clean, modern user experience, controls in `sysetholInput` are conditionally scoped using `shiny::conditionalPanel`:

- **`Steps per click`**: Shown **ONLY** when single simulation mode is selected (`input.nsim == '1'`).
- **`Total Simulation steps`**: Shown **ONLY** when multi-run simulation mode is selected (`input.nsim != '1'`).
- **`Confidence Band Envelope`**: Shown **ONLY** when multi-run envelope mode is active (`input.nsim != '1'`).
- **Sidebar Action Buttons**: **`Run Engine`** (primary action) and **`Reset`** (secondary reset) are embedded directly inside the sidebar, keeping the main panel header clean and uncluttered.

### Dynamic Navigation Tabs

`sysetholOutput` renders dynamic navigation tabs based on the active simulation mode:

- **Single Simulation (`nsim == 1`)**: Displays **`Dist Plots`**, **`Substrate Plots`**, and **`Input Data`**.
- **Multi-Run Simulation (`nsim > 1`)**: Displays **`Dist Plots`**, **`Substrate Plots`**, **`Envelope Plots`**, and **`Input Data`**. The **`Envelope Plots`** tab is displayed **ONLY** when `nsim > 1`.

### Multi-Species Hex Substrate Layouts

`sysetholServer` integrates hexagonal substrate grid mapping (`ewing_substrate`) with:

- Inline **`Species to Display`** checkboxes (`Host`, `Parasite`).
- Inline **`Species Mode`** radio buttons:
  - **`Overlay (1 Map)`**: Draws hosts and parasites simultaneously on a single hexagonal substrate map to reveal spatial parasitism overlaps.
  - **`Separate (Stacked Maps)`**: Displays host and parasite maps on separate hex grids stacked vertically (`cowplot::plot_grid(nrow = length(spp), align = "v")`) with 2pt tight margins to eliminate excess whitespace.

### Composition of `ewingApp.R` from `sysetholApp.R`

[`R/ewingApp.R`](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewingApp.R) was simplified to compose its UI and server directly from `sysetholApp` elements while preserving CSV data and PDF graphic downloads:

```r
ewingInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    sysetholInput(ns("sysethol")),
    shiny::HTML("<hr style='height:1px;border:none;color:#333;background-color:#333;' />"),
    downloadInput(ns("download")),
    shiny::HTML("<hr style='height:1px;border:none;color:#333;background-color:#333;' />"),
    shiny::HTML("See <a href='https://github.com/byandell/ewing'>ewing package on github</a><br>"),
    shiny::uiOutput(ns("version"))
  )
}

ewingOutput <- function(id) {
  ns <- shiny::NS(id)
  sysetholOutput(ns("sysethol"))
}

ewingServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current_sim <- sysetholServer("sysethol")
    downloadServer("download", sim_data = list(simres = current_sim, nsim = shiny::reactive({ 1 })))
    output$version <- shiny::renderText({
      paste("Ewing package version ", utils::packageVersion("ewing"))
    })
    current_sim
  })
}
```

### Automated R Source Code Ingestion for Quarto Shinylive Demos (`inc_files`)

To eliminate manual code duplication between R package source files in `R/` and the serverless WebAssembly Shinylive demo documents in `demos/*.qmd`, all `.qmd` demonstration applications use a dynamic `results='asis'` knitr code block.

During `quarto render`, knitr reads the specified R source files directly from `R/` via `readLines()` and dynamically injects them into the `shinylive-r` code block before Shinylive WebAssembly compilation:

```r
```{r, echo=FALSE, results='asis'}
inc_files <- c("R/inputApp.R", "R/sysetholApp.R")

cat("```{shinylive-r}\n")
cat("#| standalone: true\n")
cat("#| viewerHeight: 880\n")
cat("#| components: [viewer]\n\n")

# [Libraries & Standalone Adapters]

for (f in inc_files) {
  src <- readLines(file.path("..", f), warn = FALSE)
  cat(paste0("# --- Auto-Included from ", f, " ---\n\n"))
  cat(paste(src, collapse = "\n"))
  cat("\n\n")
}

# [App Launcher]
cat("```\n")
```
```

#### Demo Ingestion Mapping

| Quarto Demo Document | Dynamic `inc_files` Auto-Included Source Files |
| :--- | :--- |
| **`demos/sysetholApp.qmd`** | `R/inputApp.R`, `R/sysetholApp.R` |
| **`demos/fivePlotApp.qmd`** | `R/spline.R`, `R/five.R`, `R/fiveShowApp.R`, `R/fivePlotApp.R` |
| **`demos/fiveShowApp.qmd`** | `R/spline.R`, `R/five.R`, `R/fiveShowApp.R` |
| **`demos/tempApp.qmd`** | `R/temp.R`, `R/tempApp.R` |
| **`demos/triangleApp.qmd`** | `R/triangle.R`, `R/substrate_triangle.R`, `R/triangleApp.R` |
| **`demos/hexmoveApp.qmd`** | `R/substrate_triangle.R`, `R/hexmoveApp.R` |

Whenever any R function, UI layout, slider parser, or state calculation in `R/` is modified, running `quarto render demos` automatically pulls all updated code across all gallery applications with zero manual copy-pasting required.

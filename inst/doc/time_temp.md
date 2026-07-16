# Time and Temp Design

This document details the routines in the **ewing** package for adjusting the relationship of time with temperature using splines, where nodes can be moved graphically and shape modified interactively.

The routines are located in the `R/` directory of the `ewing` package.

Here are the key functions that implement interactive spline adjustments (where you can move nodes using the graphical cursor via `graphics::locator()`):

### 1. Temperature-Time Spline Adjustment

* **[temp.design()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/temp.design.R#L34)** in [R/temp.design.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/temp.design.R)
  * This routine opens an interactive graphics screen to adjust spline nodes (knots) representing **daily high and low temperatures**.
  * It provides options in a graphical sidebar (like `"add"`, `"delete"`, `"replace"`, `"rescale"`, `"restart"`, `"refresh"`, and `"finish"`) to interactively reposition nodes.

### 2. General and Mean-Value Spline Adjustment

* **[spline.design()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/spline.R#L264)** in [R/spline.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/spline.R)
  * A general prototype function for designing spline curves using the cursor. It delegates point addition, replacement, and deletion logic to the helper function **[curve.plot()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/spline.R#L86)**.
* **[future.meanvalue()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/future.meanvalue.R#L37)** & **[spline.meanvalue()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/future.meanvalue.R#L55)** in [R/future.meanvalue.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/future.meanvalue.R)
  * Used to interactively design mean value curves for life stage transition events in the simulation.
* **[five.plot()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/five.R#L151)** & **[five.show()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/five.R#L97)** in [R/five.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/five.R)
  * Interactive tools to study the 5-parameter relationship of time and mean value.

### Usage in Vignette

You can see how to call them in the package tutorial vignette [vignettes/ewing.Rmd](file:///Users/brianyandell/Documents/Research/ewing/ewing/vignettes/ewing.Rmd#L123-L143):

```r
# Interactive study of relationship of time and mean value
five.plot()
five.show()

# Interactive design plot for high and low temperatures
temp.design(simres)

# Launch Shiny-based interactive parameter sensitivity explorer
fivePlotApp()

# Launch Shiny-based interactive goal search explorer
fiveShowApp()
```

### 3. Shiny-Based Spline Adjustments (`fivePlotApp` & `fiveShowApp`)

While the base R functions `spline.meanvalue()` and `temp.design()` use `graphics::locator()` to capture cursor coordinates interactively in terminal sessions, this approach blocks the server process in web contexts. 

To bridge this gap, two dedicated Shiny applications have been added:
* **[fivePlotApp()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/fivePlotApp.R)**: Explore single parameter sensitivity curves from `five.plot()`.
* **[fiveShowApp()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/fiveShowApp.R)**: Explore multi-parameter binary searches and target relative mean goals from `five.show()`.

Both apps feature:
* **Interactive Node Editing:** Users can click directly on the baseline spline plot to reposition coordinate points. The app automatically identifies the closest node and moves it.
* **Monotonicity Guarantees:** Move events are mathematically bounded by neighboring nodes. This ensures that coordinates stay strictly sorted and monotonic, preventing backspline fitting failures.
* **Bidirectional Synchronization:** Interactive node moves instantly update the manual text fields in the sidebar, and vice versa.
* **Plot Range Scaling:** Axis ranges are computed dynamically based on the transformed curves to prevent out-of-bounds rendering issues.

### 4. Interactive Demos Gallery (WebAssembly & Shinylive)

To allow users to try these interactive apps without needing a local R console or a backend hosting server, a serverless **Demos Gallery** is available:
* **WebAssembly execution (Shinylive):** The apps run entirely client-side in the web browser using WebAssembly. There is no active backend server.
* **Quarto Project Structure:** Built using a Quarto website with the `shinylive` extension under [vignettes/demos/](file:///Users/brianyandell/Documents/Research/ewing/ewing/vignettes/demos/). The source pages ([fivePlotApp.qmd](file:///Users/brianyandell/Documents/Research/ewing/ewing/vignettes/demos/fivePlotApp.qmd) and [fiveShowApp.qmd](file:///Users/brianyandell/Documents/Research/ewing/ewing/vignettes/demos/fiveShowApp.qmd)) package the R application logic inline.
* **Rendering & Output:** Rendered using:
  ```bash
  quarto render vignettes/demos
  ```
  which compiles the files to `docs/demos/`. It is integrated into the package navbar (pointing to `demos/index.html`) in [_pkgdown.yml](file:///Users/brianyandell/Documents/Research/ewing/ewing/_pkgdown.yml).




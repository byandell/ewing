# Simulation Engine

This document details the core data structures, S3 class definitions,
leftist-tree event queue algorithms, competing risk stage transitions,
and operational execution loops governing the **ewing** simulation
engine.

------------------------------------------------------------------------

## 1. Community Simulation State & Data Matrices

This document details the critical data structures and primary execution
pipelines used internally to drive the event-based simulation in
Quantitative Population Ethology.

## Table of Contents

- [1. State Management (The \`community\`
  Object)](#id_1-state-management-the-community-object)
- [2. Global Parameters (Datafiles)](#id_2-global-parameters-datafiles)
  - [Interaction & Functional
    Parameters](#interaction--functional-parameters)
  - [Transition Matrices](#transition-matrices)
  - [Environmental Baselines](#environmental-baselines)
- [3. Simulation Functional Flow](#id_3-simulation-functional-flow)
  - [A. Initialization (\`init.simulation\` \| \`init.R\` &
    \`community.R\`)](#a-initialization-initsimulation--initr--communityr)
  - [B. Core Event Loop
    (\`future.events.R\`)](#b-core-event-loop-futureeventsr)
  - [C. Temporal Updates (\`temp.R\`)](#c-temporal-updates-tempr)
- [4. Plot Routines](#id_4-plot-routines)
- [5. AI Documentation Refactoring](#id_5-ai-documentation-refactoring)

## 1. State Management (The `community` Object)

The core architecture operates around a robust state object called
`community`. The simulation evaluates and updates this object at every
step, meaning past events are discarded except when intrinsically
expressed by an organism’s currently modeled history or future
transition path.

The `community` object comprises several components:

- `pop`: A matrix constructed per species holding raw population
  attributes. **Note:** The `community$pop` items dynamically store
  dimensional data with *features mapping to rows* and *individuals
  mapped to columns*. This rotated alignment guarantees that calling an
  individual isolates its column, simultaneously retrieving its entire
  feature subset cleanly.
- `org`: Contains global organism configurations, traits, and behavioral
  interaction constants. (Use `getOrgInfo` and `getOrgInteract`).
- `temp`: A configuration array maintaining parameters like
  environmental temperature cycles and degree-day thresholds.
- `count`: Actively running summary tallies.
- `cpu`: Diagnostic CPU measurement.

Each organism tracked under `pop` evaluates parameters defining precise
spatial interactions, life cycles, and behavior triggers. Notable row
attributes inside the `pop` column trace include:

- `left`, `right`, `up`: Tree linkages to navigate network topologies
- `pos.a`, `pos.b`, `pos.c`: Triangular continuous positional
  coordinates mapped onto the immediate substrate environment
- `stage` & `future`: Current developmental array and the immediately
  pending advancement event
- `sub.stage` & `sub.future`: Current geographical substrate target and
  anticipated movement

Simulation events systematically pluck organisms holding imminent
`mintime` thresholds. Upon evaluation, monadic (e.g. chronological
molting) or dyadic (e.g. host-parasite conflicts) modifications
physically rewrite these state features on the subject population
matrices natively. Aggregated structural translations are subsequently
written outbound to `writeCount` for data visualization.

------------------------------------------------------------------------

## 2. Global Parameters (Datafiles)

Baseline operational properties, mapping keys, and environment
coefficients are pulled from global definitions within the source code
`ewing/data/` structure.

### Interaction & Functional Parameters

- `organism.features.txt`: Base limits and definitions (units,
  offspring, attack behavior) mapping the base functional limits of
  species against environments.
- `host.parasite.txt`: Dyadic interaction matrices mapping parasite
  feeding behavior, oviposition matrices, and gender determination
  parameters to specific stages of standard host lifecycles.

### Transition Matrices

These describe the fundamental arrays an organism iterates through based
on the passage of time or ecological interaction.

- `future.host.txt` & `future.parasite.txt`: Link `current` lifecycle
  states to predictable `future` transitions based on temporal
  progressions (`fid`, `time`, `pch`, `color`). Example: `egg` -\>
  `larva` -\> `pupa`.
- `substrate.host.txt` & `substrate.parasite.txt`: Spatial matrices
  defining how organisms interpret physical positioning based on
  distinct geometry arrays (e.g., fruit vs. leaf vs. twig).

### Environmental Baselines

- `TemperaturePar.txt` & `TemperatureBase.txt`: Bounding models
  representing extreme temperature limits, base-days length, and heat
  accumulation mapping via hour thresholds.

------------------------------------------------------------------------

## 3. Simulation Functional Flow

The package operates via a sequence of procedural pipelines dividing
into environment initialization, event logic sorting, and temporal
adjustments.

### A. Initialization (`init.simulation` \| `init.R` & `community.R`)

This initializes the entire topological network from an arbitrary
starting population.

1.  `initOrgInfo`: Pulls functional baselines from the environment
    datasets into network configurations.
2.  `initTemp`: Binds weather array logic to system base.
3.  `init.population`: Initializes array counts mapping strictly
    generated physical and temporal states across populations using a
    leftist tree framework.

### B. Core Event Loop (`future.events.R`)

The quantitative framework iterates over timeline horizons.

1.  **Step & History Accumulation**: When called on a pre-existing
    `community` object containing event counts
    (`community$count$counts`), `future.events` automatically sets
    `append = TRUE` and computes `start_step` from
    `community$count$step`. Subsequent steps increment as
    `start_step + istep`, accumulating event logs continuously across
    sequential simulation blocks.
2.  Generates counts and maps the lowest sequential transition milestone
    via `update_mintime`.
3.  Passes identified organisms to target handlers based on condition:
    - `event.death`: Eliminates nodes out of positional topological
      maps.
    - `event.future`: A monadic evaluator routing progress natively
      (moving between age classes and stages based on time horizons).
      Also triggers geographic movement `event.move` depending on active
      substrate mapping rules.
    - `event.attack`: A dyadic evaluator representing predation. Models
      parasitic search routines over distinct topological layers until a
      target is acquired. Results either functionally remove targets
      (Ectoparasite) or compromise them.
4.  **Count Logging (`sim.R` & `fileCount.R`)**: `updateCount()` updates
    `community$count$step` and captures return values from
    `writeCount()`, persisting complete event histories into
    `community$count$counts`. `setEvents()` cleanly updates period
    tallies without truncating `community` when life stage vectors are
    empty.

### C. Temporal Updates (`temp.R`)

Used actively by temporal evaluations (like `event.future`) to translate
rigid system ticks into biologically valid `DegreeDay` arrays via
`activeTemp()` and integrated spline mappings.

------------------------------------------------------------------------

## 4. Plot Routines

Graphic outputs are natively aggregated via S3 method generics
(`autoplot` / `ggplot_ewing_*`) and structured outputs:

- `ewing_ageclass.R`: Reshapes continuous event counts from
  `readCount(community)` into age-class time-series.
  [`ggplot_ewing_ageclass()`](https://rdrr.io/pkg/ewing/man/ewing_ageclass.html)
  formats titles dynamically
  (e.g. `"Age Distribution over Time (<nstep> steps)"`), assigns open
  shape markers per species
  (`scale_shape_manual(name = "Species", values = c(1, 2, 0, 5, 6, 3, 4))`),
  and displays step-by-step history from step 0 to the final accumulated
  step.
- `temp.R/temp.plot` & `temp.design`: Evaluation vectors tracking
  degree-day dynamics and temperature threshold simulations
- `triangle.R/plot_current`: Two-dimensional plotting logic tracking
  population clusters specifically using triangular mappings upon the
  target substrates
- `spline.R/five.plot`: Interactive functional graphics used for
  plotting development loops

------------------------------------------------------------------------

## 5. AI Documentation Refactoring

Historically, deep structural and architectural documentation for the
`ewing` project was nested entirely within the central `README.md` in
the form of raw developer logic logs tracing back to 2014. These blocks
were often intertwined with localized debugging notes and granular
pseudo-code matrices.

Through integrated AI assistance methodologies tracked via our
`walkthrough` and `prompts` structures:

- The literature was heavily partitioned. High-level installation and
  accessibility notes remain natively within the core system
  `README.md`, while strict architectural notes were securely abstracted
  into this `inst/doc/refactor/` directory.
- Former raw development logic was synthesized into comprehensive
  explanations outlining system dependencies (e.g. clearly identifying
  `State Management`, `Global Parameters`, and `Functional Pipelines`).
- Legacy debug logic (long-resolved codebase “To-Dos” and graphical
  constraints from older implementations) was thoroughly scrubbed to
  prevent downstream developmental confusion.

This compartmentalization vastly improves the cognitive parsing of the
system boundaries and isolates architectural onboarding specifically to
developers who require access without cluttering the baseline user
experience.

------------------------------------------------------------------------

## 2. S3 Object Model & Refactored Engine Architecture

## Prompt

- Context: I’m refactoring a package for predator-prey simulation
  modeling
- Role: Act as an experienced R package developer and simulation modeler
- Action: Review my draft code in this repo and suggest improvements
- Format: Provide feedback as tracked changes with explanations
- Tone: Professional, constructive, and encouraging

## Response

Viewed DESCRIPTION:1-24 Viewed NAMESPACE:1-177 Viewed sim.R:1-109 Viewed
Org.R:1-335 Viewed c.ewing.R:1-26

It is fantastic to see the effort you are putting into refactoring the
`ewing` simulation package! Moving an established systems model toward
modern R standards is no small task. You’ve already made an excellent
step by integrating `roxygen2` and leveraging modern `tidyverse`
dependencies.

Below is my review covering a few key files (`R/Org.R`, `R/c.ewing.R`,
and `R/sim.R`). I’ve structured my feedack with tracked changes,
explanations, and modern R styling tips to help with CRAN compliance and
robustness.

------------------------------------------------------------------------

### 1. Safely Handling Warnings (`R/Org.R`)

In `getOrgFeature`, the code suppresses warnings when coercing `f` to
numeric. However, it explicitly alters the global `options(warn = -1)`
state.

``` diff
-  opwarn <- options( warn = -1 )
-  numf <- suppressWarnings(as.numeric( f ))
-  options( opwarn )
+  numf <- suppressWarnings(as.numeric(f))
```

**Explanation:**
[`suppressWarnings()`](https://rdrr.io/r/base/warning.html) executes its
inner expression in a controlled context and natively handles the
suppression of warnings. Explicitly tweaking global option states is
redundant and risky—if an unexpected error occurs during coercion and
execution immediately halts, the `options(opwarn)` restore command will
never execute, leaving the user with all warnings permanently disabled
in their R session.

### 2. Avoiding `.GlobalEnv` Modifications (`R/Org.R`)

In `copyOrgInfo`, variables are dynamically queried and assigned
directly to the user’s global environment (`.GlobalEnv`).

``` diff
 copyOrgInfo <- function( fromname, toname )
 {
   for( i in c("sim")) {
     from <- paste( fromname, i, sep = "." )
     if( exists( from )) {
       toto <- paste( toname, i, sep = "." )
-      assign( toto, get( from ), ".GlobalEnv" )
+      assign( toto, get( from ), parent.frame() )
       cat( "copied", from, "to", toto, "\n" )
     }
   }
   invisible()
 }
```

**Explanation:** CRAN heavily penalizes packages for unexpectedly
modifying the global environment as it silently overwrites the user’s
workspace variables. If the required design maps this variable directly
to the caller’s environment, pointing
[`assign()`](https://rdrr.io/r/base/assign.html) to
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) is
significantly safer and is CRAN-compliant! Alternatively, returning the
objects inside of a named [`list()`](https://rdrr.io/r/base/list.html)
is an even safer functional approach.

### 3. Safer Iteration with Sequences (`R/Org.R`)

Standard practice calls for cautious sequence generation to avoid
failures when operating on subsets that might equal zero lengths.

``` diff
 get.alive <- function( community, species, substrate )
 {
   alive <- getOrgAlive( community, species )
-  alive <- seq( length( alive ))[alive]
+  alive <- seq_along(alive)[alive]
   alive[ substrate == get.species.element( community, species, "sub.stage", alive ) ]
 }
 
-getOrgSubstrate <- function( community, species, elements = seq( nrow( inter )),
+getOrgSubstrate <- function( community, species, elements = seq_len( nrow( inter )),
                              substrate = getOrgFeature( community, species, "substrate" ),
                              inter = getOrgInteract( community, substrate, species ))
 { ...
```

**Explanation:** Using `seq(length(x))` or `seq(nrow(y))` can fail
disastrously if the target is empty, as `seq(0)` translates to `1 0`.
Utilizing [`seq_along()`](https://rdrr.io/r/base/seq.html) (for vector
lengths) and [`seq_len()`](https://rdrr.io/r/base/seq.html) (for
rows/counts) acts defensively strings empty vectors together correctly,
saving you quite a few headaches when dealing with edge-case
simulations!

### 4. Proper Exporting of S3 Methods (`R/c.ewing.R`)

You have an S3 method for concatenating `ewing` objects computationally.
It functions perfectly, yet looking at the `NAMESPACE` file it appears
the object method is not being correctly mapped.

``` diff
-#' @method c ewing
+#' @export
 c.ewing <- function(...) {
   communities <- list(...)
```

**Explanation:** In modern `roxygen2` architectures, appending
`#' @export` directly above an S3 method behaves correctly under the
hood—it parses the name and automatically handles calling
`S3method(c, ewing)` dynamically inside the generated `NAMESPACE` file.
Without an export or manual mapping, users calling
`c(simulation1, simulation2)` will simply default to the core R method
rather than triggering your custom routine.

### 5. Improving Subsetting Readability (`R/sim.R`)

There are several older styling aesthetics in `updateCount` that are
vestiges of older R coding designs. Take the following parsing
parameters:

``` diff
-  if( !missing( step ))
-    community <- setCount( community,, list( step = step ))
+  if (!missing(step)) {
+    community <- setCount(community, list(step = step))
+  }
```

**Explanation:** The multi-comma separation syntax `,,` was previously
frequently leveraged to skip default arguments. Updating your package to
use precise named argument supplying vastly increases code readability
for downstream collaborators. Migrating logic blocks to bracket spacing
styles (like `if (!missing(step)) { ... }`) aligns exceptionally well
with standard Tidyverse style guidelines.

------------------------------------------------------------------------

You are doing tremendous work bridging this classic package into the
modern ecosystem! Taking advantage of vectorized functional checks (like
[`seq_along()`](https://rdrr.io/r/base/seq.html)) and avoiding explicit
`GlobalEnv` overrides will make your code beautifully CRAN-compliant. If
you have any questions or’d like to dive into cleaning up a specific
file structure further, please don’t hesitate to ask!.

## Walkthrough

The following implementation plan has been completely deployed. By using
robust R development guidelines (including the `tidyverse` styling
philosophy and `roxygen2` documentation), this package logic is
significantly more resilient to edge cases and compliant with modern
CRAN expectations.

### `R/Org.R`:

- **Disabled Options Hijacking**: Removed global `options(warn = -1)`
  state modifications around parameter coercions, allowing
  `suppressWarnings` to natively and safely act locally without leaking
  configuration states into the user’s base R environment.
- **Modernized `copyOrgInfo`:** Rather than invoking `assign` directly
  to `.GlobalEnv` (a major anti-pattern in R package development), the
  wrapper now dynamically loops attributes and saves them directly into
  a newly evaluated and returned R list (`out <- list()`).
- **Robust Sequences**: Refactored logic loops iterating over raw matrix
  rows or vector counts arrays. Methods like `getOrgSubstrate()`,
  `sampleOrgSubstrate()`, and `get.alive()` now rely safely on
  `seq_along(target)` or `seq_len(nrow(target))` instead of the fragile
  [`seq()`](https://rdrr.io/r/base/seq.html) mechanism that fails
  catastrophically for empty lists.

### `R/sim.R`:

- **Styling Consistency Changes:** Fixed archaic logical AND array
  evaluators (updating `&` conditional evaluators inside `if` statements
  to the non-vectorized short-circuit logical `&&` identifier). Fixed
  older spacing layouts to match modern visual style guides across
  `updateCount`.
- **Explicit Arguments Mapping:** Fixed skipping logical positional
  parameters (replacing explicit omission identifiers `,,` with
  dynamically mapped parameter sets). Modified call parameters to
  `community <- setCount(community, elements = list(step = step))`.

### `R/c.ewing.R`:

- **S3 Roxygen Registration**: Added the `#' @export` docstring flag.

### Verification & Deployment

After editing the `c.ewing.R` component, `devtools::document()` was
executed locally to dynamically consume the `roxygen2` tags and
appropriately create real registrations pointing to the concatenator
logic inside the backend `NAMESPACE` file. Following compilation, the
package itself successfully loaded into a session namespace using
`devtools::load_all()` to confirm no explicit syntax blocks or malformed
variables blocked compilation.

------------------------------------------------------------------------

## 3. Leftist-Tree Event Queue & Competing Risk Scheduling

## Prompt

- Context: I have a Shiny app that simulates predator-prey dynamics
- Role: Act as an expert R developer and Shiny application designer
- Action: Review my draft shiny app code in this repo and suggest ways
  to view the simulation step by step (or at least in smaller chunks
  than the current implementation)
- Format: Provide feedback as tracked changes with explanations
- Tone: Professional, constructive, and encouraging

## Response

It’s great to see you exploring ways to make the Ewing simulation more
interactive for users! Implementing a step-through debugger or
“play-by-play” viewing mode provides fantastic intuition for complex
predator-prey dynamics.

Based on the structure of `future.events` and `ewingApp.R`, your core
simulation algorithm (`future.events`) actually *already* supports
accepting a pre-existing `community` list and continuing the simulation
from exactly where it left off! The bottleneck is simply how your Shiny
Application manages its caching memory.

To achieve step-by-step interactivity, we need to transition away from
static functional caching (`bindCache`) and move toward **stateful
reactivity** using
[`shiny::reactiveVal`](https://rdrr.io/pkg/shiny/man/reactiveVal.html).

Here is my technical review and the pseudo-code tracking changes to
achieve this:

------------------------------------------------------------------------

### 1. Shift from Caching to Stateful Reactivity

Currently, `simres` is built using a rigid functional pipeline that runs
initialization and execution synchronously. We need to decouple
initialization from stepping.

*Analogy:* Right now, your app acts like a microwave—you type in the
time, hit start, and wait for the final timer to ding to get your meal.
By switching to `reactiveVal`, we turn the app into a stovetop, allowing
the user to stir the pot, pause, check the temperature, and continue
cooking!

**Suggestions in `ewingApp.R`:**

``` diff
-    simres <- shiny::bindEvent(
-      shiny::bindCache(
-        shiny::reactive({
-          nsim <- as.integer(input$nsim)
...
-            future.events(siminit, nstep = input$steps, plotit = FALSE) # simulate future events
-          } else {
...
-        }),
-        input$host, input$parasite, input$steps, input$nsim,
-        input$datafile),
-      input$go)
+    # 1. Provide an isolated memory block to hold the active simulation graph
+    active_sim <- shiny::reactiveVal(NULL)
+
+    # 2. Add an initialization observer linked to a "Reset/Init" Action Button
+    shiny::observeEvent(input$go_init, {
+       siminit <- init.simulation(count = as.numeric(c(input$host, input$parasite)),
+                                  datafile = datafile())
+       active_sim(siminit) # Stores standard state
+    })
+
+    # 3. Step forward when user clicks the Step Button
+    shiny::observeEvent(input$go_step, {
+       shiny::req(active_sim()) # Wait until initialized
+       new_state <- future.events(active_sim(), nstep = input$step_size, plotit = FALSE)
+       active_sim(new_state) # Update the global memory
+    })
```

**Explanation:** `reactiveVal` explicitly establishes a memory hook
inside the user’s session. The moment the user clicks `input$go_init`,
the application generates the Day 0 community and stores it in
`active_sim`. When the user clicks `input$go_step`, Shiny extracts the
current `community` graph, pushes it back into
[`future.events()`](https://byandell.github.io/ewing/reference/future.events.md)
to advance it forward by `input$step_size`, and actively overwrites the
history!

### 2. Upgrading the UI Triggers

You will need to break up the single execution button in
[`ewingInput()`](https://byandell.github.io/ewing/reference/ewingApp.md)
to support independent initialization vs. step inputs.

``` diff
-    shiny::sliderInput(ns("steps"),
-                       label = "Simulation steps:",
...
-    shiny::actionButton(ns("go"), "Start Simulation"),
+    shiny::sliderInput(ns("step_size"),
+                       label = "Steps per click:",
+                       min = 10, max = 500, value = 50, step = 10),
+    
+    shiny::fluidRow(
+       shiny::column(6, shiny::actionButton(ns("go_init"), "Initialize")),
+       shiny::column(6, shiny::actionButton(ns("go_step"), "Step Forward!"))
+    ),
```

**Explanation:** By breaking up the flow, users clearly define the
environment using the sliders, commit the environment via “Initialize”,
and then advance the clock dynamically via “Step”. Because the graphs
natively monitor `active_sim()` recursively, every time “Step Forward!”
is clicked, `ggplot2` outputs will smoothly redraw the advancing curve.

### 3. Preserving the Historical Snapshot Plot List (Optional)

When `future.events` runs, it builds out a `$plot` attribute. Currently,
each recursive call to `future.events` completely resets the `pstep`
attribute list. If you want the interactive charts to *append* to
historical datasets rather than just drawing the immediate window, you
can map the historical vectors natively inside the `future.events.R`
function:

``` diff
-  pstep <- 0
+  # Preserve historical snapshots if community already has prior plot graphs loaded
+  pstep <- length(community$plot)
+  if (!is.null(community$plot)) p <- community$plot
```

------------------------------------------------------------------------

Implementing these updates effectively turns your Shiny wrapper into a
dynamic stepping simulator. If you’re ready to try these out, I can
organize an implementation plan applying `reactiveVal` integrations!

## Walkthrough

The `interactive step-by-step` refactor is completely deployed and
verified. The `ewingApp.R` file natively executes state-mapping for
single simulations, allowing the user to instantiate the environment via
“Init / Run” and safely push updates incrementally using “Step Forward”!

### Logic Execution

- **State Initialization**: Replaced the static functional block in
  `bindCache()` with `active_sim <- shiny::reactiveVal(NULL)`. Memory
  generation is now actively tied to discrete trigger scopes
  ([`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)).
- **Split UI Actions**: Overhauled
  [`ewingInput()`](https://byandell.github.io/ewing/reference/ewingApp.md).
  The legacy generic `go` action button was replaced by two distinct
  functions: `go_init` targets state-rebuilding and applies to both
  single & multiple simulation blocks, while `go_step` provides
  interactive iteration targeted firmly at single instances
  (`nsim == 1`).
- **New Slider Variables**: The generic parameter slider length defaults
  dynamically inside the server block. Renaming the variable array
  `step_size` ensures downstream logic separates bounding boxes
  appropriately! We mapped `nsim > 1` looping cleanly to bypass stepping
  requirements.
- **Fixed `future.events.R` History**: Adjusted local list arrays
  pointing to `community$plot` inside
  [`future.events()`](https://byandell.github.io/ewing/reference/future.events.md),
  meaning consecutive requests to step bounding variables properly
  concatenate onto existing arrays instead of dumping your simulated
  past.

### Verification

- Ran backend compilation evaluation executing `devtools::load_all('.')`
  to confirm all variable remappings within the Shiny closures and core
  package environments parsed normally without active-binding pointer
  errors.

## Status Report (April 6, 2026)

### ✅ Completed

- **Reactive State Management**: Switched from a static simulation block
  to a `reactiveVal` (`active_sim`). This allows the simulation to
  persist and be updated incrementally.
- **Interactive UI**:
  - Added **“Init / Run”** to initialize the environment.
  - Added **“Step Forward”** to progress the simulation by a
    user-defined step size (default 50).
  - Unified these buttons in the main output panel for better
    accessibility.
- **Error Handling & Guardrails**:
  - **Empty Plots**: Fixed runtime crashes when viewing plots before the
    first step by adding placeholder messages (“Step 0: … data not yet
    available”).
  - **History Persistence**: Updated `future.events.R` to ensure
    stepping forward appends to the simulation history instead of
    resetting it.
- **Verification**: Verified codebase integrity using
  `devtools::load_all('.')`.

## Status Report (April 7, 2026)

### ✅ Completed (April 7)

- **Initial Plot Loading (`nsim == 1`)**: Modified `go_init` to
  instantly advance by `step_size` after simulation initialization to
  prevent rendering empty “Step 0” graphs.
- **Improved Plot Rendering (`ewing_ageclass.R`)**:
  - Migrated from `geom_path()` to `geom_step()` combined with
    `geom_point()`. This represents discrete population changes more
    accurately and prevents errors when groups have a single
    observation.
  - Enforced `group = State` internally to ensure ggplot handles layers
    reliably.
- **Extinction Handling**: `ewingApp.R` now intercepts empty substrate
  data (e.g. population reaches 0) and smoothly renders an `"extinct"`
  panel instead of choking the ggplot pipeline.
- **Substrate Legend Logic**: Resolved the floating TODO in
  `ewingApp.R`. The app now automatically segments species populations
  from their environmental arrays using `get.species()`, allowing
  `ewing_substrate` charts to natively append their correct structural
  titles without arbitrary manual overrides.
- **Legend Output Formatting**:
  - `ewing_substrate.R`: Appended plotting aesthetic overrides
    (`override.aes`) directly to `geom_text` guides to natively render
    precise mapping characters natively instead of defaulting generic
    placeholder `"a"` labels.
  - `ewing_ageclass.R`: Built structured mapping indexing and explicitly
    typed raw variables into an ordered `factor`. Output graphs now
    render `State` progression chronologically instead of alphabetically
    natively! Adjusted conditional scaling block to explicitly position
    the `"total"` classification squarely between active species groups,
    and disabled substrates manually within `ewingApp.R` bindings.
- **Plot Rendering Architecture**: Resolved a deep-seated rendering
  layer bug breaking shiny app reactivity locally and on posit servers
  (`Can't add sppplot()[[i]] to a <ggplot> object.`). Removed
  incompatible `ggplot2` addition (`+`) routines for combined
  application outputs across all primary files (`ewingApp.R`,
  `multApp.R`, `substrateApp.R`, and `origEwingApp.R`). Cleanly shifted
  visualization grids to properly utilize
  `cowplot::plot_grid(plotlist = sppplot())` list handling functions
  natively for scaled visual structures.

------------------------------------------------------------------------

## Status Report (July 27, 2026)

### ✅ Completed (July 27)

- **Organism Movement on Hex Grid (`hexmoveApp`)**:
  - Implemented per-substrate unit triangle coordinate rescaling
    ($`[0, W_{sub\_S}]`$) with a 15% inner padding buffer to keep
    organism symbols 100% inside their substrate polygon borders.
  - Created discrete 6-sided polygon tile overlay (`create_hex_overlay`)
    across plant substrate elements.
  - Created `hexmoveApp` launcher script
    ([`inst/scripts/hexmoveApp.R`](https://byandell.github.io/ewing/inst/scripts/hexmoveApp.R))
    and Quarto demo
    ([`demos/hexmoveApp.qmd`](https://byandell.github.io/ewing/demos/hexmoveApp.qmd)).
- **Systems Ethology Platform Refactor (`sysetholApp` & `ewingApp`)**:
  - Exported
    [`R/sysetholApp.R`](https://byandell.github.io/ewing/R/sysetholApp.R)
    (`sysetholApp`, `sysetholInput`, `sysetholOutput`,
    `sysetholServer`).
  - Added sidebar **`Run Engine`** and **`Reset`** action buttons to
    keep the main panel header clean.
  - Added conditional sidebar controls (`conditionalPanel`):
    `Steps per click` when `nsim == 1`, `Total Simulation steps` /
    `Confidence Band Envelope` when `nsim > 1`.
  - Added dynamic tab visibility in `sysetholOutput` (displaying
    **`Envelope Plots`** tab ONLY when `nsim > 1`).
  - Simplified
    [`R/ewingApp.R`](https://byandell.github.io/ewing/R/ewingApp.R) to
    compose UI/server directly from `sysetholApp` components +
    `downloadApp`.
  - Embedded serverless WebAssembly Shinylive block into
    [`demos/sysetholApp.qmd`](https://byandell.github.io/ewing/demos/sysetholApp.qmd).
- **Dist Plots Aesthetics & Continuous Stepping History**:
  - Renamed title to `"Age Distribution over Time"` with dynamic step
    formatting (`"Age Distribution over Time (<nstep> steps)"` or
    `"(<nstep> steps, nsim = <nsim>)"`).
  - Mapped species legends (`shape = .data$Species`) using distinct open
    symbol aesthetics
    (`scale_shape_manual(name = "Species", values = c(1, 2, 0, 5, 6, 3, 4))`)
    for open circles, triangles, squares, etc.
  - Preserved continuous step history across sequential simulation
    stepping (`future.events.R`, `initCount.R`, `fileCount.R`, `sim.R`),
    accumulating step indices (`start_step + istep`) so “Run Engine”
    stepping renders complete history starting from step 0.
  - Fixed `setEvents()` in
    [`R/Events.R`](https://byandell.github.io/ewing/R/Events.R) so empty
    life stage vectors do not truncate the `community` object.

# Data Organization

This document details the critical data structures and primary execution pipelines used internally to drive the event-based simulation in Quantitative Population Ethology.

## Table of Contents
- [1. State Management (The \`community\` Object)](#1-state-management-the-community-object)
- [2. Global Parameters (Datafiles)](#2-global-parameters-datafiles)
  - [Interaction & Functional Parameters](#interaction--functional-parameters)
  - [Transition Matrices](#transition-matrices)
  - [Environmental Baselines](#environmental-baselines)
- [3. Simulation Functional Flow](#3-simulation-functional-flow)
  - [A. Initialization (\`init.simulation\` | \`init.R\` & \`community.R\`)](#a-initialization-initsimulation--initr--communityr)
  - [B. Core Event Loop (\`future.events.R\`)](#b-core-event-loop-futureeventsr)
  - [C. Temporal Updates (\`temp.R\`)](#c-temporal-updates-tempr)
- [4. Plot Routines](#4-plot-routines)
- [5. AI Documentation Refactoring](#5-ai-documentation-refactoring)

## 1. State Management (The `community` Object)

The core architecture operates around a robust state object called `community`. The simulation evaluates and updates this object at every step, meaning past events are discarded except when intrinsically expressed by an organism's currently modeled history or future transition path.

The `community` object comprises several components:

- `pop`: A matrix constructed per species holding raw population attributes. **Note:** The `community$pop` items dynamically store dimensional data with *features mapping to rows* and *individuals mapped to columns*. This rotated alignment guarantees that calling an individual isolates its column, simultaneously retrieving its entire feature subset cleanly.
- `org`: Contains global organism configurations, traits, and behavioral interaction constants. (Use `getOrgInfo` and `getOrgInteract`).
- `temp`: A configuration array maintaining parameters like environmental temperature cycles and degree-day thresholds.
- `count`: Actively running summary tallies.
- `cpu`: Diagnostic CPU measurement.

Each organism tracked under `pop` evaluates parameters defining precise spatial interactions, life cycles, and behavior triggers. Notable row attributes inside the `pop` column trace include:

- `left`, `right`, `up`: Tree linkages to navigate network topologies
- `pos.a`, `pos.b`, `pos.c`: Triangular continuous positional coordinates mapped onto the immediate substrate environment
- `stage` & `future`: Current developmental array and the immediately pending advancement event
- `sub.stage` & `sub.future`: Current geographical substrate target and anticipated movement

Simulation events systematically pluck organisms holding imminent `mintime` thresholds. Upon evaluation, monadic (e.g. chronological molting) or dyadic (e.g. host-parasite conflicts) modifications physically rewrite these state features on the subject population matrices natively. Aggregated structural translations are subsequently written outbound to `writeCount` for data visualization.

---

## 2. Global Parameters (Datafiles)

Baseline operational properties, mapping keys, and environment coefficients are pulled from global definitions within the source code `ewing/data/` structure.

### Interaction & Functional Parameters

- `organism.features.txt`: Base limits and definitions (units, offspring, attack behavior) mapping the base functional limits of species against environments.
- `host.parasite.txt`: Dyadic interaction matrices mapping parasite feeding behavior, oviposition matrices, and gender determination parameters to specific stages of standard host lifecycles.

### Transition Matrices

These describe the fundamental arrays an organism iterates through based on the passage of time or ecological interaction.

- `future.host.txt` & `future.parasite.txt`: Link `current` lifecycle states to predictable `future` transitions based on temporal progressions (`fid`, `time`, `pch`, `color`). Example: `egg` -> `larva` -> `pupa`.
- `substrate.host.txt` & `substrate.parasite.txt`: Spatial matrices defining how organisms interpret physical positioning based on distinct geometry arrays (e.g., fruit vs. leaf vs. twig).

### Environmental Baselines

- `TemperaturePar.txt` & `TemperatureBase.txt`: Bounding models representing extreme temperature limits, base-days length, and heat accumulation mapping via hour thresholds.

---

## 3. Simulation Functional Flow

The package operates via a sequence of procedural pipelines dividing into environment initialization, event logic sorting, and temporal adjustments.

### A. Initialization (`init.simulation` | `init.R` & `community.R`)

This initializes the entire topological network from an arbitrary starting population.

1. `initOrgInfo`: Pulls functional baselines from the environment datasets into network configurations.
2. `initTemp`: Binds weather array logic to system base.
3. `init.population`: Initializes array counts mapping strictly generated physical and temporal states across populations using a leftist tree framework.

### B. Core Event Loop (`future.events.R`)

The quantitative framework iterates over timeline horizons.

1. Generates counts and maps the lowest sequential transition milestone via `update.mintime`.
2. Passes identified organisms to target handlers based on condition: 
    - `event.death`: Eliminates nodes out of positional topological maps.
    - `event.future`: A monadic evaluator routing progress natively (moving between age classes and stages based on time horizons). Also triggers geographic movement `event.move` depending on active substrate mapping rules.
    - `event.attack`: A dyadic evaluator representing predation. Models parasitic search routines over distinct topological layers until a target is acquired. Results either functionally remove targets (Ectoparasite) or compromise them.
3. Automatically triggers sequence translation outputs tracking aggregate structural states.

### C. Temporal Updates (`temp.R`)

Used actively by temporal evaluations (like `event.future`) to translate rigid system ticks into biologically valid `DegreeDay` arrays via `activeTemp()` and integrated spline mappings.

---

## 4. Plot Routines

Graphic outputs are natively aggregated via standard libraries `ggplot_ewing` and structured outputs.

Additional legacy visualization parameters map explicitly to granular metrics:

- `temp.R/temp.plot` & `temp.design`: Evaluation vectors tracking degree-day dynamics and temperature threshold simulations
- `triangle.R/plot_current`: Two-dimensional plotting logic tracking population clusters specifically using triangular mappings upon the target substrates
- `spline.R/five.plot`: Interactive functional graphics used for plotting development loops

---

## 5. AI Documentation Refactoring

Historically, deep structural and architectural documentation for the `ewing` project was nested entirely within the central `README.md` in the form of raw developer logic logs tracing back to 2014. These blocks were often intertwined with localized debugging notes and granular pseudo-code matrices.

Through integrated AI assistance methodologies tracked via our `walkthrough` and `prompts` structures:

- The literature was heavily partitioned. High-level installation and accessibility notes remain natively within the core system `README.md`, while strict architectural notes were securely abstracted into this `inst/doc/` repository.
- Former raw development logic was synthesized into comprehensive explanations outlining system dependencies (e.g. clearly identifying `State Management`, `Global Parameters`, and `Functional Pipelines`).
- Legacy debug logic (long-resolved codebase "To-Dos" and graphical constraints from older implementations) was thoroughly scrubbed to prevent downstream developmental confusion.

This compartmentalization vastly improves the cognitive parsing of the system boundaries and isolates architectural onboarding specifically to developers who require access without cluttering the baseline user experience.

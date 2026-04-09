# Triangular Coordinate System

This document outlines the triangular coordinate system used in the `ewing` simulation package for modeling substrates and organism movement, as described in the `vignettes/ewing.Rmd` and implemented via the `substrate.*.txt` files.

## Prompts

Start a new document inst/doc/refactor/triangle.md.
This will be used to document the triangular coordinate system described in `vignettes/ewing.Rmd` subsection "Substrates and movement around triangular grid".
Refer also to `data/substrate.*.txt` files.
Add to this discussion of `R/triangle.R` routines and their use
(notably `rtri()`) in other R routines.

## Overview

The `ewing` simulation system allows individuals to disperse across a set of interconnected `substrate` elements. This environment is structured upon a triangular coordinate system.

The triangular grid captures the majority (~95%) of movement dynamics while significantly simplifying computational overhead. Approximations using minimum and maximum calculations replace the more intensive quadratic (Pythagorean) calculations that would be necessary in a standard rectangular coordinate system.

Each substrate patch on the grid is modeled as an interconnected triangle, with an effective diameter of 10 units (hardwired in the internal routine `event.move`).

## Substrate Connectivity (`substrate.substrate`)

Connectivity between different substrate segments is determined by the interaction matrix defined in `data/substrate.substrate.txt`.

For example, a typical grid consists of fruits (`fr1`, `fr2`, `fr3`, `fr4`), `twig`, and leaves (`lftop`, `lfbot`), with the configuration indicating pairwise connections. A value of `1` signifies an available path between components, whereas `0` (typically the diagonal) meaning no self-loop transition is explicitly defined on the graph level.

## Organism Movement Arrays (`substrate.host` and `substrate.parasite`)

Movement options and biases for species traversing the grid are directed by the `data/substrate.host.txt` and `data/substrate.parasite.txt` matrices.

These tables parameterize:

* **`substrate`**: The type of substrate component (e.g., `fruit`, `twig`, `leaf`).
* **`side`**: Substrate elements may have complex, multi-sided topographies. For instance, fruits might have sides `1, 2, 3, 4`, and leaves may be categorized into `top` and `bottom`.
* **`init`**: Relative weight determining the probability of an individual's initial placement on this element.
* **`find`**: The relative probability parameter related to a parasite finding a host.
* **`move`**: The relative weight dictating the probability an individual will choose to traverse from the current element.
* **Relative Destination Weights**: The final columns (e.g., `fruit`, `twig`, `leaf`) specify the comparative preference/weight of an organism transitioning from its current substrate to an adjacent one of the specified type.

By configuring these text files, users can fully customize the graph of the environment, establishing behavioral traits like affinity to specific plant structures (e.g., parasites preferring the top of a leaf compared to the underside) or aggregation dynamics.

## Implementation of Tridiagonal Coordinates (`R/triangle.R`)

The mathematical backing of the triangular coordinate system resides in `R/triangle.R`, converting simulation parameters into physical space. The fundamental characteristic of the tridiagonal system is that coordinates consist of three axes values ($a, b, c$) which intrinsically sum to zero ($a + b + c = 0$). Note that the terms `triangle`, `tridiagonal`, and `tri` are used interchangeably in the codebase to reference this same 3-axis space.

### Core Routines

* **`rtri(n, width, tri, roundoff)`**: Generates randomized adjustments to coordinate positions for $n$ individuals. Within the spatial dimension bounds `[0, width]`, it calculates randomized uniform variable transformations onto the 3 tridiagonal axes.
* **`car2tri(xy)`** and **`tri2car(tri)`**: Helper transformation matrices parsing objects between conventional 2D Cartesian ($x,y$) mappings and the native tridiagonal format ($a, b, c$). This translation is especially helpful for downstream plotting limits (`plot.current`) or interactions needing euclidean geometric interpretations.
* **`tridist(tri)`** and **`cardist(xy)`**: Evaluate distance in 3-axis versus 2-axis formats. Distance in triangular bounds computationally reduces to retrieving the `max` value of the matrices across an axis, further enabling performance optimizations mentioned earlier (~95% approximation with minimum processing cost).

### Usages within Simulation State

The coordinate engine interacts closely with initialization and organism event handling:

* **`R/init.population.R`**: During baseline generation via `init.population()`, an initial positional dispersion on the substrate space is computed utilizing `rtri(n, width = 100)`. Thus, individuals manifest randomly displaced throughout the substrate plane up to a radius of 100 units prior to assignment to nodes `pos.a`, `pos.b`, and `pos.c`.
* **`R/move.R`**: Individuals progressing to an `event.move` scheduled activity can hop across the substrate grid. Assuming they do not transfer directly entirely between disjoint components (`sub.future`), their physical step size traversing within their local substrate updates via `rtri(n, width = 10)`. This encapsulates standard micro-movements of scale 10 simulation units.

## Substrate Triangle Reconstruction

### Prompts

Develop an R script under `inst/scripts/substrate_triangle.R` to programmatically reconstruct the tridiagonal grid image from `Documents/plant_triangle.jpg`. The image depicts a network composed of 8 connected triangles: `lftop`, `lfbot`, `tw1`, `tw2`, `fr1`, `fr2`, `fr3`, and `fr4`.

Instead of using random noise generation via `rtri()`, we will generate a regular geometric lattice using native triangular coordinates $(a, b, c)$, and apply topology offsets to map the interconnected substrate shapes.

### Walkthrough

- **Fixed `car2tri.default` (`R/triangle.R`)**: Discovered and patched a silent matrix dimension bug. `rbind(x, y)` was improperly generating a `2xN` matrix instead of the expected `Nx2` matrix. This caused parsing errors and translation failures in the tridiagonal coordinate conversions. The function now appropriately runs `cbind`, feeding correctly oriented memory to `car2tri()`.
- **Created `substrate_triangle.R`**: Added the new tridiagonal topological visualization tool `inst/scripts/substrate_triangle.R`. This tool generates a localized mesh array $(a, b, c)$ modeling interlocking geometries for standard upward and downward component orientations, correctly restricted to precisely 10 dots per edge using mathematical delta `- step` adjustments.
- **Topology Mappings**: Formulated a refined tridiagonal topology matrix spanning the visual plane. This explicitly builds the hexagonal layout dictated by the adjacency limits, positioning `fr2` as the downward central triangle seamlessly abutting (`fr1`, `fr3`, `fr4`), and sequentially attaching the `twig` and `leaf` modules to form proper continuous branches without empty grid gaps.
- **Visual Overlays & Alignment**: In addition to resolving a 1-dot alignment slip (calibrating the adjacency anchors exactly to the offset bound distances `W = 9`), the structure generates explicit bounding polygons spanning the substrate borders with black outer lines. Finally, it dynamically calculates side boundary midpoints and interpolates them 25% inward towards the substrate centroid to properly overlay the `1, 2, 3` numeric axis boundary identifiers right inside their respective sides.

By passing the aggregate topology to the package's internal `tri2car()` geometry transformer, the code accurately maps the layout to standardized Euclidean spatial data $(x,y)$ enabling robust visualization.

### Object-Oriented Refactoring

To maximize reusability across the simulation suite, the core construction loops from the prototype script have been completely abstracted into the package's source directory under `R/substrate_triangle.R`. This explicitly decouples the network structure into formal functional operations:

- `substrate_topology(width, step)`: Isolates mathematical configuration offsets and coordinate boundary limits globally. We explicitly transitioned away from "plant" nomenclature to the "substrate" standard to support generalized interaction meshes.
- `create_substrate(topology, width, step)`: Iterates across the defined configuration to compute analytical geometry components (extracting Euclidean mesh dots, defining polygon bounds via topological vertices, and interpolating numerical boundary indicators automatically). Returns an S3 object of format `class = "substrate"`.
- `autoplot.substrate(object)`: Implements a scalable visual overlay handler utilizing `ggplot2`. Users can instantiate the tridiagonal substrate network internally and map it instantly utilizing native commands like `autoplot(my_substrate)`!

The `inst/scripts/substrate_triangle.R` script now exclusively invokes these native package functions (via `library(ewing)`) to render the reconstruction!

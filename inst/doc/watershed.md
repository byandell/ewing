# Watershed Triangulation Overlay

## Prompt

- **Context**: The `ewing` predator-prey simulation uses an abstract tridiagonal coordinate system. This needs to be spatially projected onto real-world geography utilizing the [Watershed Boundary Dataset (HUC 12s)](https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about). Our specific target is Isle Royale in Lake Superior (HUC12: `041800000101`).
- **Role**: Expert R Developer and Spatial Integrator.
- **Action**: Review prior work (`inst/doc/triangle.md`, `inst/scripts/plant_triangle.R`) to execute two foundational steps:
  1. **Algebraic Refactoring**: Architect a more elegant, object-oriented S3 methodology to simplify tri-coordinate math (i.e. replacing item-by-item grid manipulations).
  2. **Subwatershed Overlay pipeline**: Develop actionable scripts (e.g., `inst/scripts/watershed_overlay.R`) to programmatically fetch HUC12 boundaries, calculate geographic centroids, and overlay our abstract triangular networks using standard geographic mapping packages.
- **Format**: Tracked feedback and architectural outlines.
- **Tone**: Professional, constructive, and encouraging.

### HUC References

- [Watershed Boundary Dataset (HUC 12s)](https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about)
- [Regional Hydrologic Unit Map](https://www.usgs.gov/media/images/regional-hydrologic-unit-map)
- [Watershed Boundary Dataset Structure Visualization](https://www.usgs.gov/media/images/watershed-boundary-dataset-structure-visualization)

## Expert Review & Proposed Architecture

This is a fantastic application of the `ewing` package's tridiagonal infrastructure. Transitioning from abstract ecological topologies (like plant substrates) to geographic spatial datasets (like HUC12 boundaries) heavily benefits from object-oriented refinement.

Here are the tracked changes and design propositions required to elegantly construct this overlay:

### 1. Refactoring Tri-Coordinate Algebra via S3 Classes

Addressing the `(a, b, c)` coordinate vectors item-by-item is brittle and mathematically verbose. By wrapping coordinates into a standardized `tricoord` S3 class structure in `R/triangle.R`, we can implement operator overloading. This empowers R to apply spatial topology translations natively to data.frames and vectors containing these coordinates utilizing elegant `var1 + var2` syntax!

**Feedback Tracked Details (`inst/scripts/plant_triangle.R`):**

```diff
  # Apply geometric offset
  o_a <- cfg$offset[1]
  o_b <- cfg$offset[2]
  o_c <- cfg$offset[3]
  
- grid$a <- grid$a + o_a
- grid$b <- grid$b + o_b
- grid$c <- grid$c + o_c
+ grid <- grid + cfg$offset
  
  # Map to Cartesian Coordinates
- car_pts <- tri2car(rbind(grid$a, grid$b, grid$c))
+ car_pts <- tri2car(grid)
```

*Explanation: We extract the raw scalar math and matrix transpositions into the base class logic, significantly reducing visual noise and the likelihood of matrix dimension errors (which historically occurred with `rbind`/`cbind` mismatches).*

### 2. Developing Overlay Projections for HUC12 Subwatersheds

To project the tridiagonal substrate network atop HUC12 polygon data (like Isle Royale `041800000101`), we require a transformation pipeline converting mathematical $(a, b, c)$ bounds securely into the `sf` package's CRS (Coordinate Reference Systems):

> [!TIP]
> **Suggested Geographic Projection Pipeline:**
>
> 1. **Data Acquisition Layer**: Utilize native packages like `nhdplusTools` (e.g., `get_huc(id = "041800000101", type = "huc12")`) to fetch the `MULTIPOLYGON` watershed bound.
> 2. **Euclidean Extraction**: Process the abstract substrate interaction grid using `tri2car()`, retrieving planar coordinates.
> 3. **Affine Spatial Tie-Point**: Define an anchor mapping centroid of the extracted HUC12 polygon via `sf::st_centroid()`.
> 4. **Scale Calibration**: Multiply the planar distance matrix by an appropriate geographic scale multiplier ($S$) effectively translating theoretical simulation units.
> 5. **Spatial Geometry Overlays**: Render both systems cooperatively into one figure utilizing `ggplot2::geom_sf()`, mapping abstract paths structurally to topological realities.

### 3. Implementation of the Geographic Pipeline

The script `inst/scripts/watershed_overlay.R` has been updated to successfully implement this pipeline for the target Isle Royale HUC12. It dynamically acquires the spatial outline via `nhdplusTools::get_huc()`, determines the required `huc_lon` and `huc_lat` from the structural centroid (`sf::st_centroid()`), and maps the tridiagonal simulated data strictly atop the true map utilizing `geom_sf()`, finalizing the mathematical-geospatial integration!

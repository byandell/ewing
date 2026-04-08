# Watershed Triangulation Overlay

## Prompt

New conversation on watershed triangulation overlay, with walkthrough file being `inst/doc/watershed.md` Here is the CRAFT prompt:

- Context: I have a triangular coordinate system for predator-prey simulations that could be overlaid on the [Watershed Boundary Dataset (HUC 12s)](https://resilience.climate.gov/datasets/esri::watershed-boundary-dataset-huc-12s/about).
For concreteness, consider HUC12 070700050206, which is the Prairie du Sac Dam-Wisconsin River subwatershed in Wisconsin.
- Role: Act as an expert R developer and Shiny application designer.
- Action: Review the `inst/doc/triangle.md` work and the script in `inst/scripts/plant_triangle.R` to do the following:
  - Find a more elegant way to set up the algebra with tri-coordinate systems. That is, the code requires 3 lines per point set to address a,b,c. What if there was an object class and operations (such as addition) that would simplify operations?
  - Begin developing ways to overlay the triangular coordinate system on maps based on subwatersheds (HUC12) and finer scale.
- Format: Provide feedback as tracked changes with explanations
- Tone: Professional, constructive, and encouraging

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

To project the tridiagonal substrate network atop HUC12 polygon data (like Prairie du Sac Dam-Wisconsin River `070700050206`), we require a transformation pipeline converting mathematical $(a, b, c)$ bounds securely into the `sf` package'CRS (Coordinate Reference Systems):

> [!TIP]
> **Suggested Geographic Projection Pipeline:**
> 1. **Data Acquisition Layer**: Utilize native packages like `nhdplusTools` or `tigris` (e.g. `get_huc12(id = "070700050206")`) or a loaded static `.shp` file to fetch the `MULTIPOLYGON` object representing the watershed bound.
> 2. **Euclidean Extraction**: Process the abstract substrate interaction grid using `tri2car()`, retrieving abstract $(x, y)$ planar coordinates.
> 3. **Affine Spatial Tie-Point**: Define an anchor metric. Mathematically locate the centroid of the extracted HUC12 polygon via `sf::st_centroid()`. Map $(x=0, y=0)$ abstract origin strictly to the true $(longitude, latitude)$ of this centroid.
> 4. **Scale Calibration**: Multiply the planar distance matrix by an appropriate geographic scale multiplier ($S$) effectively translating theoretical simulation units into true physical meters.
> 5. **Spatial Geometry Overlays**: Render both systems cooperatively into one figure utilizing `ggplot2::geom_sf()`, mapping abstract paths structurally to topological realities.

These abstractions are excellent enhancements targeting both code legibility and spatial applicability for real-world environmental matrices!

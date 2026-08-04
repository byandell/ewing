# Generalized Ecosystem Spatial Predator-Prey Simulation

Initializes, runs, and visualizes an individual-based spatial
predator-prey simulation model for any target ecosystem or landscape
(e.g. \`"isle_royale"\`, \`"yellowstone"\`). Incorporates spatial
habitat preferences, life stage transitions, and optional empirical
census benchmarking data.

## Usage

``` r
init_ecosystem_sim(
  ecosystem = "isle_royale",
  year = 1980,
  n_hosts = NULL,
  n_predators = NULL,
  hex_diameter = 0.01,
  datafile = "",
  features_rds = NULL,
  landmarks_rds = NULL
)

run_ecosystem_sim(sim_obj, nstep = 1000, refresh = 10, ...)

ggplot_ecosystem_sim(x, ...)
```

## Arguments

- ecosystem:

  Target simulation landscape/site subfolder name under \`extdata/\`
  (default: \`"isle_royale"\`).

- year:

  Target baseline year from census benchmark file if available (default:
  1980).

- n_hosts:

  Initial number of host/prey individuals (default: looked up from
  benchmark CSV or 664).

- n_predators:

  Initial number of predator individuals (default: looked up from
  benchmark CSV or 50).

- hex_diameter:

  Diameter of hexagonal substrate mesh (default: 0.01 degrees).

- datafile:

  Custom path to data directory or Excel workbook.

- features_rds:

  Optional path or \`sf\` object for custom site habitat features.

- landmarks_rds:

  Optional path or \`sf\` object for custom site landmarks.

- sim_obj:

  An object of class \`ecosystem_sim\` or derived species simulation
  object.

- nstep:

  Number of simulation steps to run (default: 1000).

- refresh:

  Refresh step interval (default: 10).

- ...:

  Additional plot options.

- x:

  An object of class \`ecosystem_sim\`.

## Value

\`init_ecosystem_sim\`: An S3 object of class \`c("\[ecosystem\]\_sim",
"ecosystem_sim", "ewing")\`.

Updated simulation object.

A \`ggplot\` visualization.

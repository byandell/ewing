# Isle Royale Wolf-Moose Predator-Prey Simulation

Initializes, runs, and visualizes an individual-based spatial
predator-prey simulation model of wolves and moose on Isle Royale.
Incorporates habitat preferences (inland lakes, beaver ponds, shaded
forests, bogs), life stage transitions (Calf, Yearling, Adult, Senior),
and empirical benchmarking against historical 1980-2019 annual census
data.

Executes simulation steps for the Isle Royale wolf-moose spatial model.

## Usage

``` r
init_isle_royale_sim(
  year = 1980,
  n_moose = NULL,
  n_wolves = NULL,
  hex_diameter = 0.01,
  datafile = "",
  features_rds = NULL,
  landmarks_rds = NULL
)

run_isle_royale_sim(sim_obj, nstep = 1000, refresh = 10, ...)

ggplot_isle_royale_sim(x, ...)
```

## Arguments

- year:

  Target baseline year from \`wolf_moose.csv\` (1980-2019, default:
  1980).

- n_moose:

  Initial number of moose individuals (default: looked up from
  \`wolf_moose.csv\`).

- n_wolves:

  Initial number of wolf individuals (default: looked up from
  \`wolf_moose.csv\`).

- hex_diameter:

  Diameter of hexagonal substrate mesh (default: 0.01 degrees).

- features_rds:

  Optional path or \`sf\` object for custom site habitat features (e.g.
  exported from \`hexmapApp\`).

- landmarks_rds:

  Optional path or \`sf\` object for custom site landmarks (e.g.
  exported from \`hexmapApp\`).

- sim_obj:

  An object of class \`isle_royale_sim\`.

- nstep:

  Number of simulation steps to run (default: 1000).

- refresh:

  Step interval for progress reporting (default: 100).

- ...:

  Additional arguments.

- x:

  An object of class \`isle_royale_sim\`.

## Value

\`init_isle_royale_sim\`: An S3 object of class \`isle_royale_sim\`
containing the initialized \`ewing\` simulation community and spatial
habitat metadata.

\`run_isle_royale_sim\`: Updated \`isle_royale_sim\` object.

A \`ggplot\` object displaying spatial individual distributions and
historical census benchmarking.

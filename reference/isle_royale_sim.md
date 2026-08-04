# Isle Royale Wolf-Moose Predator-Prey Simulation (Wrapper Shell)

Backward-compatible wrapper for initializing, running, and visualizing
the Isle Royale wolf-moose spatial predator-prey model using the
generalized \`ecosystem_sim\` engine.

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

- datafile:

  Optional custom datafile directory path.

- features_rds:

  Optional path or \`sf\` object for custom site habitat features.

- landmarks_rds:

  Optional path or \`sf\` object for custom site landmarks.

- sim_obj:

  An object of class \`isle_royale_sim\` or \`ecosystem_sim\`.

- nstep:

  Number of simulation steps to run (default: 1000).

- refresh:

  Refresh step interval (default: 10).

- ...:

  Additional plot options.

- x:

  An object of class \`isle_royale_sim\`.

## Value

\`init_isle_royale_sim\`: An S3 object of class \`c("isle_royale_sim",
"ecosystem_sim", "ewing")\`.

Updated simulation object.

A \`ggplot\` visualization.

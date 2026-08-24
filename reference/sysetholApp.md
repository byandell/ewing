# Systems Ethology App

Interactive Systems Ethology platform orchestrating host-parasite
individual-based simulations, age class distributions, hexagonal spatial
substrate networks, variance envelopes, and input data tables.

Unified platform wrapper launching the Systems Ethology simulation
application for any target ecosystem (e.g. \`"default"\`,
\`"isle_royale"\`).

## Usage

``` r
sysetholApp(title = "Systems Ethology Platform", ecosystem = "default")

sysetholSystem(ecosystem = "default", title = NULL)

sysetholInput(id)

sysetholOutput(id)

sysetholServer(id, ecosystem = "default")
```

## Arguments

- title:

  Optional application title.

- ecosystem:

  Target ecosystem name (default: \`"default"\`).

- id:

  Module ID string

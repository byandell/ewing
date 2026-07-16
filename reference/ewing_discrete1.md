# Discrete time simulation for Ewing Envelope

Do one simulation and save only by discrete \`time\` and \`increment\`

## Usage

``` r
ewing_discrete1(
  siminit = init.simulation(interact = FALSE, messages = FALSE, ...),
  increment = 0.5,
  ...
)
```

## Arguments

- siminit:

  initialize simulation

- increment:

  increment for discrete simulation time

- ...:

  additional parameters

- nsim:

  number of simulations to run

- verbose:

  show \`.\` for each simulation if \`TRUE\`

- object:

  object of class \`ewing_envelope\` or \`ewing_envelopes\`

## Details

Wrapper to set up class and attributes of \`ewing_envelope\` object.

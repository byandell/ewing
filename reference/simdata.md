# Simulated Data with Multiple Runs

Result of \`ewing_discrete\` with 100 runs of

## Format

Object of class \`ewing_discrete\`. Contains \`nsim\` runs each of
\`nstep\` steps. Attributes include the following

- nsim:

  number of simulations

- nstep:

  number of steps per simulation

- count:

  vector with initial population count by species

- species:

  bector of species names

- items:

  list by species of age classes (used in summary and plot methods)

- ordinate:

  ordinate for simulation sequesnce (usually \`time\`)

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`ewing_discrete`](https://rdrr.io/pkg/ewing/man/ewing_discrete.html)

## Examples

``` r

data(simdata)
```

# organism features master table

Identifies organisms and their key features for Ewing's Quantitative
Population Ethology simulation. Each row corresponds to a \`species\` to
be used in the simulation. Note that \`subclass\` names must correspond
to possible \`current\` stage for that \`species\`.

## Format

Data frame with one row per organism potentially in the study. By
default, the first two organisms are selected for simulation
initialization in `init.simulation`. Missing values indicate this
feature is not relevant for this species.

- units:

  time units (DD=degree days, hr=hours)

- offspring:

  Poisson mean number of offspring if numeric, or reference to host if
  character; see `host.parasite` object.

- attack:

  character name of host for attack or feeding

- birth:

  stage for birth of offspring if host; missing if parasite

- substrate:

  substrate on which organism lives

- deplete:

  energy depletion rate over time of offspring load in units of number
  of offspring

- subclass:

  ageclass of species to use for substrate movements in `future.events`

- parasite:

  type of parasite or predator (ecto=ectoparasitoid,
  endo=endoparasitoid)

- move:

  stage when species can move around substrate

## Source

Lisa D. Forster and Robert F. Luck, Entomology, UC Riverside.

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md),[`future.events`](https://byandell.github.io/ewing/reference/future.events.md)

## Examples

``` r

data(organism.features)
```

# host-parasite interaction table for simulation

Parasites select hosts for ovipositing (egg-laying) or feeding based on
their stage of development. The risk of male offspring depends on stage,
as does the initial offspring load for the successfully reared parasite.
These tables are internalized via `init.simulation`.

## Format

- ovip:

  risk of life stage being selected for oviposition (egg-laying)

- feed:

  risk of life stage being selected for feeding

- offspring:

  Poisson mean number of offspring load for new parasite reared in host
  of this stage

- male:

  risk of parasite laying a male egg in this host stage

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md)

## Examples

``` r

data(host.parasite)
data(host.comperiella)
#> Warning: data set ‘host.comperiella’ not found
data(host.encarsia)
#> Warning: data set ‘host.encarsia’ not found
```

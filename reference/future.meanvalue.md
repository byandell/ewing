# design mean value curves for future events in Ewing simulation models

Uses interactive spline tool to design curves.

## Usage

``` r
future.meanvalue(community, species, event = future$current[1], data)
```

## Arguments

- community:

  object with population data by species

- species:

  name of species

- event:

  name of future event

- data:

  data if available

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md),
[`future.events`](https://byandell.github.io/ewing/reference/future.events.md)

## Author

Brian S. Yandell, <yandell@stat.wisc.edu>

## Examples

``` r


if (FALSE) { # \dontrun{
future.meanvalue( community, "host", "first.instar" )
} # }

```

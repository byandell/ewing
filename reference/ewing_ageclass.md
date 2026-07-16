# Ravel count from Ewing simulation models

Ravel counts to produce one comprehensive data frame across species

## Usage

``` r
ewing_ageclass(
  community,
  substrate = TRUE,
  total = TRUE,
  normalize = TRUE,
  ...
)

ggplot_ewing_ageclass(object, ...)

# S3 method for class 'ewing_ageclass'
autoplot(object, ...)
```

## Arguments

- community:

  object of class `ewing` with population data by species

- substrate:

  include substrate if `TRUE`

- total:

  add total column if `TRUE`

- normalize:

  normalize counts to 1 if `TRUE`

- ...:

  extra parameters not used

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md),
[`future.events`](https://byandell.github.io/ewing/reference/future.events.md),
[`summary.ewing`](https://byandell.github.io/ewing/reference/summary.ewing.md)

## Author

Brian S. Yandell, <yandell@stat.wisc.edu>

## Examples

``` r


if (FALSE) { # \dontrun{
ewing_ageclass( community )
} # }

```

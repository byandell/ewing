# ggplot of Ewing simulation model age classes

GGPlot of various aspects of simulation.

## Usage

``` r
ggplot_ewing(object, step = 0, ageclass = TRUE, substrate = !ageclass, ...)

# S3 method for class 'ewing'
autoplot(object, ...)

# S3 method for class 'ewing'
plot(x, ...)
```

## Arguments

- ...:

  other plot parameters

- x:

  object of class `ewing` with population data by species

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
ggplot_ewing( community )
} # }

```

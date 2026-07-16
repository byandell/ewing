# graphical adjustment of daily high and low temperature

Graphical interface updates daily high and low temperatures, along with
rescaling the days and temperature range.

## Usage

``` r
temp.design(
  community,
  nspline = 8,
  n = 1,
  horizontal = TRUE,
  col = c(low = "blue", high = "red")
)
```

## Arguments

- community:

  object with population data by species

- nspline:

  number of spline nodes

- n:

  number of steps to perform

- horizontal:

  use horizontal orientation if TRUE

- col:

  colors of `low` and `high` temperature ranges

- lo.hour:

  lower limit of hours to plot

- hi.hour:

  upper limit of hours to plot

- length:

  number of interpolation points for plot

- derivative:

  show derivative of temperature

- ...:

  other arguments to pass to plot

- printit:

  print out hours and temperatures if `TRUE`

## References

See
[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing).

## See also

[`TemperatureBase`](https://rdrr.io/pkg/ewing/man/TemperatureBase.html)

## Author

Brian S. Yandell

## Examples

``` r


if (FALSE) { # \dontrun{
temp.design()
temp.plot()
} # }

```

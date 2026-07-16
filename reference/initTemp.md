# temperature data tables for simulation

Used internally by `init.simulation` to initialize temperature regime
for simulation.

## Usage

``` r
initTemp(
  community,
  lo.hour = 0,
  hi.hour = getTemp(community, "Unit"),
  days = TemperaturePar["Days"],
  messages = TRUE,
  datafile = "",
  ...
)
```

## Format

`TemperatureBase` is a data frame with one or more daily baseline
temperatures.

- Day:

  day on which baseline begins

- Time:

  time in hours from midnight

- Base:

  temperature offset in percent between daily lows and highs

- value:

  value of parameter

- description:

  description of parameter (same as column descriptions below)

&nbsp;

- Unit:

  hours in day

- Days:

  number of days

- Min:

  minimum temperature for degree-day calculation (Fahrenheit)

- LowBeg:

  begining daily low temperature

- LowEnd:

  ending daily low temperature

- HighBeg:

  begining daily high temperature

- HighEnd:

  ending daily high temperature

- Length:

  number of nodes for interpolating spline

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md)

## Examples

``` r


data(TemperatureBase)
#> Warning: data set ‘TemperatureBase’ not found
data(TemperaturePar)
#> Warning: data set ‘TemperaturePar’ not found
```

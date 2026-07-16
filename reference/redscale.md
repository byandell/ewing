# Red Scale simulation initiation

Sample simulation initiation setup.

## Format

`redscale` is a data frame with the following columns

- DD:

  mean degree days to future event

- aphytis.feed:

  relative risk of being killed by an Aphytis that is feeding

- aphytis:

  relative risk of being killed by an Aphytis that is laying eggs

- comperiella:

  relative risk for comperiella

- encarsia:

  relative risk for encarsia

- gender:

  gender (only females modeled here)

- pch:

  plot character

- color:

  plot color

- stage:

  life stage of organism

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.population`](https://byandell.github.io/ewing/reference/init.population.md)

## Examples

``` r

data(redscale)
```

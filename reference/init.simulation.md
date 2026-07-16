# Initialize simulation run

Initializes simulation using organism features. Default data are used
unless user provides global data or uses the hidden argument
\`datafile\` to specify a folder with user data.

## Usage

``` r
init.simulation(
  package = "ewing",
  count = 200,
  interact = FALSE,
  messages = TRUE,
  ...
)
```

## Arguments

- package:

  package where community data features can be found

- count:

  number of individuals per species (single number or count in order of
  \`species\`)

- interact:

  ask for species count if \`TRUE\` and interactive

- messages:

  show messages if \`TRUE\` (default)

- ...:

  additional arguments for \`init.population\`

## Value

Object with elements for each species that are created by
init.population.

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.population`](https://byandell.github.io/ewing/reference/init.population.md)

## Author

Brian S. Yandell

## Examples

``` r


if (FALSE) init.simulation(myrun) # \dontrun{}

```

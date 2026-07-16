# Initialize Organism Population

Create data structure with n organisms distributed across life stages.
Includes leftist tree structure

## Usage

``` r
init.population(
  community,
  species,
  n = 200,
  width = 100,
  units = getOrgFeature(community, species, "units"),
  timeit = FALSE,
  reject = Inf,
  position = rtri(n, width),
  colnames = c(leftistnames, paramnames, posnames, eventnames),
  init.stage = istage,
  init.weight = getOrgFuture(community, species, "init"),
  messages = TRUE,
  ...
)
```

## Arguments

- community:

  community structure

- species:

  species name(s) as character string vector

- n:

  number of organisms in population

- width:

  maximum width of dispersal on tridiagonal coordinate system

- units:

  time units (default is `getOrgFeature(community,species,"units")`)

- timeit:

  print timing information if `TRUE`

- reject:

  rejection time (default is `Inf`)

- position:

  initial positions in triangular coordinate system (generated randomly)

- colnames:

  names of columns in data structure (do not change)

- init.stage:

  initial stage of all organisms (default values generated randomly)

- init.weight:

  initial weights for life stages (default taken for organism features)

- ...:

  not used

## Value

- comp1 :

  Description of \`comp1'

- comp2 :

  Description of \`comp2'

...

## Details

Does various initializations based in information in `future.xxx`, where
`xxx` is replaced by the name of the dataset. Normally,
`init.population` is called by `init.simulation`.

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md)

## Author

Brian S. Yandell

## Examples

``` r


if (FALSE) init.population( redscale ) # \dontrun{}

```

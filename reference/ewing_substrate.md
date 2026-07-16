# Ewing Substrate by Species

Ewing Substrate by Species

## Usage

``` r
ewing_substrate(
  community,
  species,
  headstuff = c(0, "start", sum(to.plot)),
  units = getOrgFeature(community, species, "units"),
  right = species,
  adj = c(0, 0.5, 1),
  show_sub = substrates,
  step = 0,
  ...
)

ggplot_ewing_substrate(object, xlab = "horizontal", ylab = "vertical", ...)

# S3 method for class 'ewing_substrate'
autoplot(object, ...)
```

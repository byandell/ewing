# Get HUC12 From Map Click Coordinate

Automatically retrieves the underlying USGS HUC12 subwatershed boundary
polygon given an arbitrary Longitude and Latitude pair (usually derived
from a user click). This powers the Option B interaction paradigm.

## Usage

``` r
get_huc_from_point(lng, lat)
```

## Arguments

- lng:

  Numeric longitude coordinate

- lat:

  Numeric latitude coordinate

## Value

An \`sf\` polygon representation of the covering HUC12.

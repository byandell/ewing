# Resolve Cached Spatial Site Datasets

Resolves file paths for pre-computed spatial GIS datasets (\`.rds\`) for
a target simulation site or landscape (e.g. \`"isle_royale"\`), checking
installed package directories (\`extdata/\[site\]\`) and local
development source trees (\`inst/extdata/\[site\]\`).

## Usage

``` r
get_site_cache_file(filename, site = "isle_royale")

get_isle_royale_cache_file(filename)
```

## Arguments

- filename:

  File name string (e.g. \`"isle_royale_layer.rds"\`).

- site:

  Target site/landscape directory name under \`extdata/\` (default:
  \`"isle_royale"\`).

## Value

Path to target cached dataset file.

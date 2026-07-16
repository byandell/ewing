# Create Envelope of Ewing Simulations

Create envelope object for plotting from multiple runs of Ewing
simulation.

## Usage

``` r
ewing_envelope(object, species, item, ordinate = "time", increment = 0.5)

ewing_envelopes(object)

# S3 method for class 'ewing_envelopes'
summary(object, species = NULL, verbose = TRUE, ...)

# S3 method for class 'ewing_envelopes'
print(x, species = NULL, ...)
```

## Arguments

- object:

  object of class \`ewing_envelope\` or \`ewing_envelopes\`

- species:

  subset on \`species\` if not \`NULL\`

- item:

  name of item in \`species\` to build envelope

- ordinate:

  name of ordinate (X axis) to build envelope

- increment:

  increament for discretizing

- verbose:

  print settings if \`TRUE\`

- ...:

  additional parameters

- x:

  object of class \`ewing_envelope\` or \`ewing_envelopes\`

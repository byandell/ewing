# realize future events in quantitative population ethology simulation

Steps through future events for community with one or more species.
Keeps track of counts by age classes and substrates.

## Usage

``` r
future.events(
  community,
  ...,
  nstep = 4000,
  species = get.species(community),
  refresh = nstep/20,
  cex = 0.5,
  substrate.plot = TRUE,
  extinct = TRUE,
  timeit = TRUE,
  debugit = FALSE,
  messages = TRUE
)
```

## Arguments

- community:

  object with population data by species

- ...:

  additional arguments passed to \`initCount\`

- nstep:

  number of steps to perform

- species:

  list of species to simulate

- refresh:

  plot refresh rate

- cex:

  character expansion

- substrate.plot:

  show plots of substrate use

- extinct:

  stop when first specied becomes extinct

- timeit:

  record timing by event

- debugit:

  detailed debug for advanced users

- plotit:

  new plot at each refresh

- ggplots:

  use ggplot if \`TRUE\` and \`plotit\` is \`TRUE\`

## Value

List containing the following items:

- pop:

  updated community of species

- org:

  organism information (from `init.simulation`

- temp:

  temperature and time curve structure

- count:

  simulation counts

- cpu:

  CPU use summary

## Details

This is the main routine for Ewing's Quantitative Population Ethology.
It steps through future events for individuals starting with the next
minimum future event time.

All individuals have a \`current\` stage and are organized into an event
queue, which is a triply-linked leftist tree, based on the scheduled
time for their next \`future\` event. Each species has its own leftist
tree, with the tops of those trees identifying the individuals with the
closest (in time) next \`future\` event. Internal routine
\`put.species\`, in conjuction with \`leftist.update\`,
\`leftist.remove\` or \`leftist.birth\`, modify the leftist trees when
there is an individual event update, death (remove) or birth(s),
respectfully.

An individual in a species will progress from \`current\` to \`future\`
stage when its event time is at the top of the event queue. The \`fid\`
points to the row in this table corresponding to the \`future\` stage,
which would then become the \`current\` stage. The code uses numeric
\`fid\` because it ends up in a vector of other numeric values.

Note that sometimes there are multiple rows with the same \`current\`
value, which are competing risks. For instance \`future.host\` has
competing risks from the \`current\` stage \`second.3\` of becoming
\`female\` or \`male\`, while \`future.parasite\` has competing risk
from the current stage \`adult\` to \`feed\` or \`ovip\`osit, with
return lines from \`feed\` and \`ovip\` to \`adult\`. That is, an adult
parasite might feed or oviposit, which have different health and
population consequences: feeding prolongs life while ovipositing
produces new offspring and depletes life.

The \`time\` entry is used to schedule the time of the \`future\` event.
That is, when and individual appears at the top of the event queue.

A plot is created periodically unless `plotit=FALSE`. If argument
\`file\` is set to a file name, an external file is written with
simulation counts for re-plotting.

## References

See
[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing).

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md),
[`event.future`](https://byandell.github.io/ewing/reference/event.future.md)

## Author

Brian S. Yandell

## Examples

``` r


if (FALSE) { # \dontrun{
  init.simulation(mystuff)
## step through 4000 future events
step.mystuff <- future.events(mystuff,4000)
## replot the results
plot.ewing( step.mystuff )
## reprint timing of most recent future.events run
print.timing()
## or of the one you want
print.timing( step.mystuff$timing )
## show temperature information used in simulation
showTemp( step.mystuff$temperature )
## continue on with more future events
step.further <- future.events( step.mystuff,4000 )
} # }

```

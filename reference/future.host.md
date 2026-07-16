# future events table for species simulation

Contains life history events for QPE simulation. These are normally
internalized via `init.simulation`.

## Format

Data frame with one row per stage, except in the case of competing
risks. Time is in appropriate units for species as set in
`organism.features`.

- current:

  current stage

- future:

  potential future event

- fid:

  future event ID, corresponds to row when this is current stage

- time:

  mean time to future event

- pch:

  plot character

- color:

  plot color

- ageclass:

  class of stage (see `organism.features`

- event:

  event class

- init:

  current stage weighting for initialization

## Source

Lisa D. Forster and Robert F. Luck, Entomology, UC Riverside.

## Details

This sets out how an individual in a species will progress from
\`current\` to \`future\` stage when its event time is at the top of the
event leftist tree queue. The \`fid\` points to the row in this table
corresponding to the \`future\` stage, which would then become the
\`current\` stage. The code uses numeric \`fid\` because it ends up in a
vector of other numeric values.

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

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md),[`future.events`](https://byandell.github.io/ewing/reference/future.events.md)

## Examples

``` r

data(future.host)
data(future.parasite)
```

# process immediate, pending and future events in quantitative population ethology simulation

Process an event for next individual in a species. Events may be monadic
(one individual) or dyadic (involving two individuals).

## Usage

``` r
event.future(community, species)
```

## Arguments

- community:

  list with initial population data by species

- species:

  name of species with current event

## Value

List containing the updated community of species data structures.

## Details

These are the main event processors for Ewing's Quantitative Population
Ethology. They all take the same arguments, but handle events in quite
different ways. `event.future` is the generic routine for monadic future
events, such as progressing through life stages. `event.death` handles
all pending events of death of individuals.

`event.birth` handles immediate events of new births. It assumes at
present one birth at a time, leading ultimately to a starved state. The
`future.host` data has the gravid adult returning to the fertile state
until it depletes its resources. The pending event "starved" then
triggers the pending event "death".

`event.attack` is an example of a dyadic event processor, in this case
host-parasite interaction. The parasite locates a host based on life
stages and relative position in the simulation space. Ectoparasites kill
their hosts, hence realizing a pending death event (to be processed
immediately by `event.death` in the next step). Endoparasites merely
incapacitate their hosts.

At present, the search strategies of parasites and health consequences
of hosts are crude. However, search is over substrates and depends on
the stage of hosts. A crude sex determination strategy is in place, with
smaller (younger) hosts more likely to get male parasite eggs.

Simulation counts are stored internally unless the argument \`file\` is
set to the name of an external file. Following each event, counts are
recorded of changes to the number or stage of individuals.

The routine `future.events` builds the name of the event processor
function based on event type, which is established in the `future.host`
or `future.parasite` data structure. The `event.attack` uses further
information from `organism.features` about the type of parasite (`endo`
or `ecto`) in conjunction with the current event (`feed` or `ovip`) to
determine the nature of attack.

User-supplied `event.blah` functions can be specified in user-supplied
libraries. Note that user code needs to comply with the arguments and
values, and needs to process future events for individuals as they
progress through life. This is at present an advanced topic, but can be
figured out by examining the above routines.

## References

See
[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing).

## See also

[`future.events`](https://byandell.github.io/ewing/reference/future.events.md),
[`organism.features`](https://byandell.github.io/ewing/reference/organism.features.md),
[`future.host`](https://byandell.github.io/ewing/reference/future.host.md)

## Author

Brian S. Yandell

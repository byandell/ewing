# substrate data for host-parasite movement

The substrate.species tables (`substrate.host`, etc.) describe movement
rates on the substrate. The substrate.substrate object
(`substrate.substrate`) describes connectivity of substrate units. Names
of species and substrate(s) are established in `organism.features` and
internalized via `init.simulation`.

## Format

The substrate.species objects have the following columns:

- substrate:

  class of substrate (fruit, twig or leaf)

- side:

  side of substrate

- init:

  weights for initialization of simulation

- find:

  risk species finds this new position on substrate

- move:

  risk species moves from this position on substrate

- fruit:

  first substrate class (substrate fruit)

- twig:

  second substrate class (twig connecting fruit to branch)

- leaf:

  third substrate class (leaf on twig)

The row names of substrate.species objects coincide with the row and
column names of substrate.substrate. Notice that for this simulation,
there are four sides to each fruit (1,2,3,4) and two sides to each leaf
(top,bottom), but only one side to a twig.

- fr1:

  fruit side 1

- fr2:

  fruit side 2

- fr3:

  fruit side 3

- fr4:

  fruit side 4

- twig:

  twig

- lftop:

  leaf top

- lfbot:

  leaf bottom

## Details

The substrate classes are defined implicitly in the substrate.species
interaction tables, and could be different for different species on the
same substrate. These substrate classes are associated with the possible
substrate positions (7 in this simulation, 4 on fruit, 2 on leaf and 1
on twig).

Movement of individuals is rather primitive in this invocation, and is
not fully implemented. The `find` and `move` are placeholders for future
improvements. Basically the intent is that teh risk for moving from a
position on the substrate is proportional to `move`, while the risk of
moving to a new position is proportional to `find`. As set, fruit is
more preferable than leaf, which is preferred to twig, for all species.
\[These columns are not in fact used for current simulations.\]
Connectivity of positions is determined by substrate.substrate, which
contains 0s and 1s, with positions directly connected (1) or not (0).

## References

[www.stat.wisc.edu/~yandell/ewing](https://byandell.github.io/ewing/reference/www.stat.wisc.edu/~yandell/ewing)

## See also

[`init.simulation`](https://byandell.github.io/ewing/reference/init.simulation.md),[`organism.features`](https://byandell.github.io/ewing/reference/organism.features.md)

## Examples

``` r

data(substrate.host)
data(substrate.parasite)
data(substrate.substrate)
```

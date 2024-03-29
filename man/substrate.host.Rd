% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/substrate.host.R
\name{substrate.host}
\alias{substrate.host}
\alias{substrate.parasite}
\alias{substrate.substrate}
\title{substrate data for host-parasite movement}
\format{
The substrate.species objects have the following columns:
\describe{
 \item{substrate}{class of substrate (fruit, twig or leaf)}
 \item{side}{side of substrate}
 \item{init}{weights for initialization of simulation}
 \item{find}{risk species finds this new position on substrate}
 \item{move}{risk species moves from this position on substrate}
 \item{fruit}{first substrate class (substrate fruit)}
 \item{twig}{second substrate class (twig connecting fruit to branch)}
 \item{leaf}{third substrate class (leaf on twig)}
}

The row names of substrate.species objects coincide with the row and column
names of substrate.substrate. Notice that for this simulation, there are
four sides to each fruit (1,2,3,4) and two sides to each leaf (top,bottom),
but only one side to a twig.
\describe{
 \item{fr1}{fruit side 1}
 \item{fr2}{fruit side 2}
 \item{fr3}{fruit side 3}
 \item{fr4}{fruit side 4}
 \item{twig}{twig}
 \item{lftop}{leaf top}
 \item{lfbot}{leaf bottom}
}
}
\description{
The substrate.species tables (\code{substrate.host}, etc.) describe movement
rates on the substrate. The substrate.substrate object
(\code{substrate.substrate}) describes connectivity of substrate units.
Names of species and substrate(s) are established in
\code{organism.features} and internalized via \code{init.simulation}.
}
\details{
The substrate classes are defined implicitly in the substrate.species
interaction tables, and could be different for different species on the same
substrate. These substrate classes are associated with the possible
substrate positions (7 in this simulation, 4 on fruit, 2 on leaf and 1 on
twig).

Movement of individuals is rather primitive in this invocation, and is not
fully implemented. The \code{find} and \code{move} are placeholders for
future improvements. Basically the intent is that teh risk for moving from a
position on the substrate is proportional to \code{move}, while the risk of
moving to a new position is proportional to \code{find}. As set, fruit is
more preferable than leaf, which is preferred to twig, for all species.
[These columns are not in fact used for current simulations.] Connectivity
of positions is determined by substrate.substrate, which contains 0s and 1s,
with positions directly connected (1) or not (0).
}
\examples{

data(substrate.host)
data(substrate.parasite)
data(substrate.substrate)

}
\references{
\url{www.stat.wisc.edu/~yandell/ewing}
}
\seealso{
\code{\link{init.simulation}},\code{\link{organism.features}}
}
\keyword{datasets}

\name{init.simulation}
\alias{init.simulation}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Initialize simulation run}
\description{
Initializes simulation using organizm features.
}
\usage{
init.simulation(package="ewing", species, hosts, count=200 )
}
\arguments{
 \item{package}{package where community data features can be found}
 \item{species}{list of species to simulate}
 \item{hosts}{list of hosts for species}
 \item{count}{number of individuals per species}
}
\details{
}
\value{
  Object with elements for each species that are created by init.population.
}
\references{\url{www.stat.wisc.edu/~yandell/ewing}}
\author{Brian S. Yandell}
\seealso{\code{\link{init.population}}}

\examples{
\dontrun{init.simulation(myrun)}
}
\keyword{ }%-- one or more ...

\name{future.meanvalue}
\alias{future.meanvalue}
\title{design mean value curves for future events in Ewing simulation models}
\description{
Uses interactive spline tool to design curves.
}
\usage{
future.meanvalue( community, species, event, data )
}
\arguments{
  \item{community}{object with population data by species}
  \item{species}{name of species}
  \item{event}{name of future event}
  \item{data}{data if available}
}
\references{ \url{www.stat.wisc.edu/~yandell/ewing} }
\author{ Brian S. Yandell, \email{yandell@stat.wisc.edu} }

\seealso{\code{\link{init.simulation}},
  \code{\link{future.events}}}

\examples{
\dontrun{
future.meanvalue( community, "host", "first.instar" )
}
}
\keyword{utilities}

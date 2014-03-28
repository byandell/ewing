\name{summary.ewing}
\alias{summary.ewing}
\title{summary of Ewing simulation models}
\description{
Summary of various aspects of simulation.
}
\usage{
\method{summary}{ewing}( object, ... )
}
\arguments{
  \item{object}{object of class \code{ewing} with population data by species}
  \item{...}{other summary parameters}
}
\references{ \url{www.stat.wisc.edu/~yandell/ewing} }
\author{ Brian S. Yandell, \email{yandell@stat.wisc.edu} }

\seealso{\code{\link{init.simulation}},
  \code{\link{future.events}}, \code{\link{plot.ewing}}}

\examples{
\dontrun{
summary.ewing( community )
}
}
\keyword{utilities}

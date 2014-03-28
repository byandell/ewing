\name{plot.ewing}
\alias{plot.ewing}
\title{plot of Ewing simulation models}
\description{
Plot of various aspects of simulation.
}
\usage{
\method{plot}{ewing}( x, substrate, mfcol, ... )
}
\arguments{
  \item{x}{object of class \code{ewing} with population data by species}
  \item{substrate}{include substrate plot if TRUE}
  \item{mfcol}{par parameter reset by default}
  \item{...}{other plot parameters}
}
\references{ \url{www.stat.wisc.edu/~yandell/ewing} }
\author{ Brian S. Yandell, \email{yandell@stat.wisc.edu} }

\seealso{\code{\link{init.simulation}},
  \code{\link{future.events}}, \code{\link{summary.ewing}}}

\examples{
\dontrun{
plot.ewing( community )
}
}
\keyword{utilities}

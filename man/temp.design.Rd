\name{temp.design}
\alias{temp.design}
\alias{temp.plot}
\title{graphical adjustment of daily high and low temperature}
\description{
Graphical interface updates daily high and low temperatures, along with
rescaling the days and temperature range.
}
\usage{
temp.design(community,nspline=8, n=1, horizontal=TRUE,
  col=c(low="blue",high="red"))
temp.plot(community,lo.hour = s$knots[1], hi.hour = max( s$knots ), length = 201,
  col = NULL, derivative = FALSE, ..., printit = FALSE )
}
\arguments{
  \item{community}{object with population data by species}
 \item{nspline}{number of spline nodes}
 \item{n}{number of steps to perform}
 \item{horizontal}{use horizontal orientation if TRUE}
 \item{col}{colors of \code{low} and \code{high} temperature ranges}
 \item{lo.hour}{lower limit of hours to plot}
 \item{hi.hour}{upper limit of hours to plot}
 \item{length}{number of interpolation points for plot}
 \item{derivative}{show derivative of temperature}
 \item{...}{other arguments to pass to plot}
 \item{printit}{print out hours and temperatures if \code{TRUE}}
}
\details{
}
\references{
See \url{www.stat.wisc.edu/~yandell/ewing}.
}
\author{Brian S. Yandell}
\seealso{\code{\link{TemperatureBase}}}

\examples{
\dontrun{
temp.design()
temp.plot()
}
}
\keyword{}%-- one or more ...

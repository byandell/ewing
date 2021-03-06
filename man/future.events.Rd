\name{future.events}
\alias{future.events}
\title{realize future events in quantitative population ethology simulation}
\description{
Steps through future events for community with one or more
species. Keeps track of counts by age classes and substrates
in external file.
}
\usage{
future.events(community, file, nstep=4000, species,
  refresh=nstep/20, cex=0.5, append=FALSE, plotit=TRUE, substrate.plot=TRUE,
  extinct=TRUE, timeit=TRUE, debugit=FALSE)
}
\arguments{
 \item{community}{object with population data by species}
 \item{file}{file name for simulation output}
 \item{nstep}{number of steps to perform}
 \item{species}{list of species to simulate}
 \item{refresh}{plot refresh rate}
 \item{cex}{character expansion}
 \item{append}{append simulation output to file}
 \item{plotit}{new plot at each refresh}
 \item{substrate.plot}{show plots of substrate use}
 \item{extinct}{stop when first specied becomes extinct}
 \item{timeit}{record timing by event}
 \item{debugit}{detailed debug for advanced users}
}
\details{
This is the main routine for Ewing's Quantitative Population
Ethology. It steps through future events for individuals starting with
the next minimum future event time.
  
  A plot is created periodically unless \code{plotit=FALSE}.
  An external \code{file} is written with simulation counts
  for re-plotting.
}
\value{
  List containing the following items:
  \item{pop}{updated community of species}
  \item{org}{organism information (from \code{init.simulation}}
  \item{temp}{temperature and time curve structure}
  \item{count}{simulation counts}
  \item{cpu}{CPU use summary}
}
\references{
See \url{www.stat.wisc.edu/~yandell/ewing}.
}
\author{Brian S. Yandell}
\seealso{\code{\link{init.simulation}},
  \code{\link{event.future}} }
\examples{
\dontrun{
  init.simulation(mystuff)
## step through 4000 future events
step.mystuff <- future.events(mystuff,4000)
## replot the results
plot.totals( step.mystuff )
## reprint timing of most recent future.events run
print.timing()
## or of the one you want
print.timing( step.mystuff$timing )
## show temperature information used in simulation
showTemp( step.mystuff$temperature )
## continue on with more future events
step.further <- future.events( step.mystuff,4000 )
}
}
\keyword{}%-- one or more ...

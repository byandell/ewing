\name{organism.features}
\alias{organism.features}
\non_function{}
\title{organism features master table}
\description{
Identifies organisms and their key features for Ewing's Quantitative
Population Ethology simulation.
}
\usage{data(organism.features)}
\format{
  Data frame with one row per organism potentially in the study. By
  default, the first two organisms are selected for simulation
  initialization in \code{init.simulation}. Missing values indicate this
  feature is not relevant for this species.
  \describe{
    \item{units}{time units (DD=degree days, hr=hours)}
    \item{offspring}{Poisson mean number of offspring if numeric, or
      reference to host if character; see \code{host.parasite} object.}
    \item{attack}{character name of host for attack or feeding}
    \item{birth}{stage for birth of offspring if host; missing if parasite}
    \item{substrate}{substrate on which organism lives}
    \item{deplete}{energy depletion rate over time of offspring load in
      units of number of offspring}
    \item{subclass}{class of stages to use for plotting substrate
      movements in \code{future.events}}
    \item{parasite}{type of parasite or predator (ecto=ectoparasitoid, endo=endoparasitoid)}
    \item{move}{class of stages when species can move around substrate}
  }
}
\seealso{\code{\link{init.simulation}},\code{\link{future.events}}}
\source{
Lisa D. Forster and Robert F. Luck, Entomology, UC Riverside.
}
\references{
\url{www.stat.wisc.edu/~yandell/ewing}
}
\examples{
data(organism.features)
}
\keyword{datasets}

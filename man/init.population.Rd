\name{init.population}
\alias{init.population}
\title{Initialize Organism Population}
\description{
Create data structure with n organisms distributed across life stages.
Includes leftist tree structure
}
\usage{
init.population(community, species, n=200, width=100, units,
  timeit=FALSE, reject=Inf, position, colnames, init.stage, init.weight)
}
\arguments{
  \item{community}{community structure}
  \item{species}{species name(s) as character string vector}
 \item{n}{number of organisms in population}
 \item{width}{maximum width of dispersal on tridiagonal coordinate system}
 \item{units}{time units (default is
   \code{getOrgFeature(community,species,"units")})} 
 \item{timeit}{print timing information if \code{TRUE}}
 \item{reject}{rejection time (default is \code{Inf})}
 \item{position}{initial positions in triangular coordinate system
   (generated randomly)}
 \item{colnames}{names of columns in data structure (do not change)}
 \item{init.stage}{initial stage of all organisms (default values
   generated randomly)}
 \item{init.weight}{initial weights for life stages (default taken for
   organism features)}
}
\details{
Does various initializations based in information in \code{future.xxx},
where \code{xxx} is replaced by the name of the dataset. Normally,
\code{init.population} is called by \code{init.simulation}.
}
\value{
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
  ...
}
\author{Brian S. Yandell}
\seealso{\code{\link{init.simulation}}}

\examples{
\dontrun{init.population( redscale )}
}
\keyword{ }%-- one or more ...

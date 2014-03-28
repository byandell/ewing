\name{A starting point}
\alias{A starting point}
\title{Introductory comments}
\description{
  A brief introduction to the R/ewing package, with a walk-through of an
  simulation.
}

\section{New to R and/or R/ewing?}{
  \itemize{
    \item In order to use the R/ewing package, you must type (within R)
    \code{library(ewing)}.  You may wish to include this in a
    \code{\link[base:Startup]{.First}} function or
    \code{\link[base:Startup]{.Rprofile}} file. 

    \item Documention and several tutorials are available
    at the R archive (\url{http://cran.r-project.org}).

    \item Use the \code{\link[base]{help.start}} function to start the
    html version of the R help.  In Windows, you may wish to use
    \code{options(htmlhelp=TRUE)} to get easy access to the html version
    of the help files; this could be included in a
    \code{\link[base:Startup]{.First}} function or
    \code{\link[base:Startup]{.Rprofile}} file.

    \item Type \code{library(help=ewing)} to get a list of the functions
    in R/ewing.

    \item Use the \code{\link[base]{example}} function to run examples
    of the various functions in R/ewing.

    \item A tutorial on the use of R/ewing is distributed with the
    package, \file{build.pdf}.

    \item Download the latest version of R/ewing from
    \url{http://www.stat.wisc.edu/~yandell/ewing}.
    \item Download as well the R/redscale example library from the same
    location. 
  }
}

\section{Walk-through of a simulation}{
Here we briefly describe the use of R/ewing to simulate a host-parasite
system.  A more extensive tutorial on its use is distributed with the 
package, \file{build.pdf}, with figures in \file{buildfigs.pdf}. [The
current version of this document has some features out of date, but the
basic ideas are there.]

A simulation starts by initializing the simulation:

\code{myrun <- init.simulation()}

This will ask various questions abouting setting the temperature and the
number of each species (default species are \code{host} and
\code{parasite}). It creates a data structure containing population
information by species as well as background information on organisms
and temperature over time. Once completed, the simulation proceeds with

\code{saverun <- future.events(myrun,"myrun.out")}

Default runs for 4000 future events, with roughly 20 preview plots and a
final plot showing cumulative counts by age classes and proportions of a
selected age class by substrate. The \code{future.events} returns an
updated data structure from the last future event processed. The plot
can be recreated with

\code{plot( saverun )}

Simulation summary is available as:

\code{summary( saverun )}

Mean value curves for future events can be designed using
\code{future.meanvalue}. Temperature curves can be redesigned using
\code{temp.design} and examined using \code{temp.plot}.
}

\references{
  B Ewing, BS Yandell, JF Barbieri, and RF Luck (2002) "Event-driven
competing risks," \emph{Ecological Modelling 158}: 35--50.
}

\note{
  Also see the package BQTL (Bayesian QTL mapping toolkit) by
  Charles C. Berry and R/qtl by Karl W. Broman.  Their packages gave me
  the idea to have this "A starting point" help page.
}

\author{Brian S Yandell, \email{yandell@stat.wisc.edu}}

\keyword{models}

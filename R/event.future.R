#' process immediate, pending and future events in quantitative population
#' ethology simulation
#' 
#' Process an event for next individual in a species. Events may be monadic
#' (one individual) or dyadic (involving two individuals).
#' 
#' These are the main event processors for Ewing's Quantitative Population
#' Ethology. They all take the same arguments, but handle events in quite
#' different ways. \code{event.future} is the generic routine for monadic
#' future events, such as progressing through life stages. \code{event.death}
#' handles all pending events of death of individuals.
#' 
#' \code{event.birth} handles immediate events of new births. It assumes at
#' present one birth at a time, leading ultimately to a starved state. The
#' \code{future.host} data has the gravid adult returning to the fertile state
#' until it depletes its resources. The pending event "starved" then triggers
#' the pending event "death".
#' 
#' \code{event.attack} is an example of a dyadic event processor, in this case
#' host-parasite interaction. The parasite locates a host based on life stages
#' and relative position in the simulation space. Ectoparasites kill their
#' hosts, hence realizing a pending death event (to be processed immediately by
#' \code{event.death} in the next step). Endoparasites merely incapacitate
#' their hosts.
#' 
#' At present, the search strategies of parasites and health consequences of
#' hosts are crude. However, search is over substrates and depends on the stage
#' of hosts. A crude sex determination strategy is in place, with smaller
#' (younger) hosts more likely to get male parasite eggs.
#' 
#' Simulation counts are stored internally unless the argument `file` is set to
#' the name of an external file. Following each event, counts are recorded of
#' changes to the number or stage of individuals.
#' 
#' The routine \code{future.events} builds the name of the event processor
#' function based on event type, which is established in the \code{future.host}
#' or \code{future.parasite} data structure. The \code{event.attack} uses
#' further information from \code{organism.features} about the type of parasite
#' (\code{endo} or \code{ecto}) in conjunction with the current event
#' (\code{feed} or \code{ovip}) to determine the nature of attack.
#' 
#' User-supplied \code{event.blah} functions can be specified in user-supplied
#' libraries. Note that user code needs to comply with the arguments and
#' values, and needs to process future events for individuals as they progress
#' through life. This is at present an advanced topic, but can be figured out
#' by examining the above routines.
#' 
#' @aliases event.future event.birth event.death event.attack
#' @param community list with initial population data by species
#' @param species name of species with current event
#' @return List containing the updated community of species data structures.
#' @author Brian S. Yandell
#' @seealso \code{\link{future.events}}, \code{\link{organism.features}},
#' \code{\link{future.host}}
#' @references See \url{www.stat.wisc.edu/~yandell/ewing}.
#' @keywords utilities
#' @export event.future
event.future <- function( community, species )
{
  ## schedule future event based on current stage
  individual <- get.future( community, species )
  ## move if appropriate
  individual <- event.move( community, species, individual )
  ## update time translation if needed
  community <- checkTime( community, individual["time"],
                          getCount( community, species, "mintime"),
                          getOrgFeature( community, species, "units" ))
  ## put updated individual back in community
  community <- put.individual( community, species, individual )
  
  ## reprioritize the leftist tree if time has changed
  if( individual["time"] > individual["location"] ) {
    ## and time is longer than next scheduled time
    if( individual["time"] >
        min( get.species.element( community, species, "time",
                                  individual[c("left","right")] ))) {
      ## remove individual from leftist tree
      community <- put.species( community, species,
                                leftist.update( get.species( community, species )))
    }
    community <- update_mintime( community, species )
  }
  community
}

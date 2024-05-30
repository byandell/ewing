#' realize future events in quantitative population ethology simulation
#' 
#' Steps through future events for community with one or more species. Keeps
#' track of counts by age classes and substrates.
#' 
#' This is the main routine for Ewing's Quantitative Population Ethology. It
#' steps through future events for individuals starting with the next minimum
#' future event time.
#' 
#' All individuals have a `current` stage and are organized into an event
#' queue, which is a triply-linked leftist tree, based on the scheduled time
#' for their next `future` event. Each species has its own leftist tree, with
#' the tops of those trees identifying the individuals with the closest (in
#' time) next `future` event. Internal routine `put.species`, in conjuction
#' with `leftist.update`, `leftist.remove` or `leftist.birth`, modify the
#' leftist trees when there is an individual event update, death (remove) or
#' birth(s), respectfully.
#' 
#' An individual in a species will progress from `current` to `future` stage
#' when its event time is at the top of the event queue. The `fid` points to
#' the row in this table corresponding to the `future` stage, which would then
#' become the `current` stage. The code uses numeric `fid` because it ends up
#' in a vector of other numeric values.
#' 
#' Note that sometimes there are multiple rows with the same `current` value,
#' which are competing risks. For instance `future.host` has competing risks
#' from the `current` stage `second.3` of becoming `female` or `male`, while
#' `future.parasite` has competing risk from the current stage `adult` to
#' `feed` or `ovip`osit, with return lines from `feed` and `ovip` to `adult`.
#' That is, an adult parasite might feed or oviposit, which have different
#' health and population consequences: feeding prolongs life while ovipositing
#' produces new offspring and depletes life.
#' 
#' The `time` entry is used to schedule the time of the `future` event. That
#' is, when and individual appears at the top of the event queue.
#' 
#' A plot is created periodically unless \code{plotit=FALSE}. If argument
#' `file` is set to a file name, an external file is written with simulation
#' counts for re-plotting.
#' 
#' @param community object with population data by species
#' @param ... additional arguments passed to `initCount`
#' @param nstep number of steps to perform
#' @param species list of species to simulate
#' @param refresh plot refresh rate
#' @param cex character expansion
#' @param substrate.plot show plots of substrate use
#' @param extinct stop when first specied becomes extinct
#' @param timeit record timing by event
#' @param debugit detailed debug for advanced users
#' @param plotit new plot at each refresh
#' @param ggplots use ggplot if `TRUE` and `plotit` is `TRUE`
#' @return List containing the following items: \item{pop}{updated community of
#' species} \item{org}{organism information (from \code{init.simulation}}
#' \item{temp}{temperature and time curve structure} \item{count}{simulation
#' counts} \item{cpu}{CPU use summary}
#' @author Brian S. Yandell
#' @seealso \code{\link{init.simulation}}, \code{\link{event.future}}
#' @references See \url{www.stat.wisc.edu/~yandell/ewing}.
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#'   init.simulation(mystuff)
#' ## step through 4000 future events
#' step.mystuff <- future.events(mystuff,4000)
#' ## replot the results
#' plot.ewing( step.mystuff )
#' ## reprint timing of most recent future.events run
#' print.timing()
#' ## or of the one you want
#' print.timing( step.mystuff$timing )
#' ## show temperature information used in simulation
#' showTemp( step.mystuff$temperature )
#' ## continue on with more future events
#' step.further <- future.events( step.mystuff,4000 )
#' }
#' 
#' 
#' @export future.events
future.events <- function( community, ...,
                           nstep = 4000,
                           species = get.species( community ),
                           
                           refresh = nstep / 20, cex = 0.5,
                           substrate.plot = TRUE, extinct = TRUE,
                           timeit = TRUE, debugit = FALSE,
                           messages = TRUE )
  
{
  ## Integrity check of dataset, and initialization of tallies.
  if( missing( community ))
    stop( "Must specify a community." )
  
  if( debugit ) cat( "initialization\n" )
  community <- initCount( community, species, debugit, messages = messages, ... )
  if( timeit )
    community <- init.timing( community )
  
  mintime <- getCount( community, , "mintime" )
  species.now <- species[ mintime == min( mintime ) ][1]
  future <- getOrgFuture( community, species.now )
  
  # Set up list for plot information.
  p <- list()
  pstep <- 0
  
  ## for nstep steps schedule future events and process immediate events
  for( istep in seq( nstep )) {
    ## stop if any extinct and extinct flag on, or all extinct
    omintime <- mintime
    mintime <- getCount( community, , "mintime" )
    if( debugit ) print( mintime )
    if( min( mintime ) < min( omintime )) {
      cat( "time reversal!\n" ) # should not happen
      browser()
    }
    tmp <- mintime == Inf
    if( any( tmp )) {
      if( extinct | all( tmp )) {
        for( i in names( mintime )[tmp] )
          cat( "***", i, "is extinct ***\n" )
        if( plotit )
          plot.ewing( community, substrate = substrate.plot, cex = cex, ...)
        break
      }
    }
    ## each species is always sorted so 1st element is next future event
    species.prev <- species.now
    species.now <- species[ mintime == min( mintime ) ][1]
    individual <- get.individual( community, species.now )
    if( is.na( individual["time"] ) | individual["time"] == Inf ) {
      cat( "No more finite future events. End of simulation.\n" )
      break
    }
    if( all( individual[c("dist","left","right","up")] == 1 )) {
      cat( individual["time"], ": last", species.now, "alive",
           getCount( community, species.now, "base" ), "\n" )
    }
    if( species.now != species.prev )
      future <- getOrgFuture( community, species.now )
    ## make future event the current stage
    current <- individual["stage"]
    stage <- as.character( future$current[current] )
    if(!length(stage))
      stop(paste("no stage", current))
    community <- updateCount( community, species.now, individual, stage == "death",
                              istep )
    individual["stage"] <- current
    individual["sub.stage"] <- individual["sub.future"]
    community <- put.individual( community, species.now, individual )
    
    if( debugit ) {
      cat( species.now, istep, "base",
           getCount( community, species.now, "base" ), "\n" )
      print( c( step = istep, 
                countage = sum( getCount( community, species.now, "countage" )),
                countsub = sum( getCount( community, species.now, "countsub" )),
                round( individual["time"], 2 ), current, stage ))
    }
    
    ## processing of immediate, pending and future events
    ## this is the main show--all the rest is setup
    event.type <- as.character( future[ current, "event" ] )
    if( debugit ) cat( "do", event.type, istep, stage,
                       as.character( future$current[ individual["future"] ] ),
                       individual["time"], "\n" )
    community <- set.timing( community, event.type )
    if( event.type == "death" )
      community <- event.death( community, species.now )
    else {
      if( event.type != "future" ) {
        ## this routine could be user supplied
        ## generic routines are event.birth, event.attack
        event.parsed <- get( paste( "event", event.type, sep = "." ))
        community <- event.parsed( community, species.now )
      }
      community <- event.future( community, species.now )
    }
    community <- set.timing( community, event.type, 1 )
    
    ## refresh plot
    if( refresh & ! ( istep %% refresh )) {
      community <- set.timing( community, "refresh" )
      # Save the ewing_ageclass and ewing_substrate objects.
      pstep <- pstep + 1
      p[[pstep]] <- ewing_snapshot(community, istep, ...)
      
      if(messages) {
        cat( "refresh", istep )
        for( j in get.species( community ))
          cat( ":", j, sum( getCount( community, j, "countage" ), na.rm = TRUE ))
        cat( "\n" )
      }
      community <- set.timing( community, "refresh", 1 )
    }
    ## periodic browser if in debug mode
    if( debugit ) {
      cat( "done", istep, "\n" )    
      if( refresh & !( istep %% refresh )) {
        cat( "Type \"c\" to continue or \"Q\" to quit.\n" )
        browser()
      }
    }
  }
  community <- set.timing( community, "refresh" )
  ## end of main loop on future events
  if( debugit ) cat("done\n")
  
  if( sum( getOrgAlive( community, species.now )) > 1 ) {
    ## tally events at end of simulation
    community <- setEvents( community, "final" )
  }
  community <- set.timing( community, "refresh", 1 )
  
  community <- fini.timing( community )
  attr(community, "nstep") <- nstep
  
  if( !refresh | (nstep%%refresh)) {
    pstep <- pstep + 1
    p[[pstep]] <- ewing_snapshot(community, istep, ...)
  }
  community$plot <- p
  
  community
}

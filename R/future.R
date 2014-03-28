## $Id: future.R,v 1.0 2002/12/11 yandell@stat.wisc.edu Exp $
##
## Functions for Bland Ewing's modeling.
##
##     Copyright (C) 2000,2001,2002 Brian S. Yandell.
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
###############################################################################
##
## future.events( community )
##
###############################################################################
future.events <- function( community,
                          file,
                          nstep = 4000,
                          species = get.species( community ),

                          refresh = nstep / 20, cex = 0.5, append = FALSE,
                          plotit = TRUE, substrate.plot = TRUE, extinct = TRUE,
                          timeit = TRUE, debugit = FALSE)
  
{
  ## Integrity check of dataset, and initialization of tallies.
  if( missing( community ))
    stop( "Must specify a community." )
  if( missing( file ))
    stop( "Must specify a simulation file." )

  if( debugit ) cat( "initialization\n" )
  community <- initCount( community, species, file, append, debugit )
  if( timeit )
    community <- init.timing( community )

  mintime <- getCount( community, , "mintime" )
  species.now <- species[ mintime == min( mintime ) ][1]
  future <- getOrgFuture( community, species.now )

  ## for nstep steps schedule future events and process immediate events
  for( istep in seq( nstep )) {
    ## stop if any extinct and extinct flag on, or all extinct
    omintime <- mintime
    mintime <- getCount( community, , "mintime" )
    if( debugit ) print( mintime )
    if( min( mintime ) < min( omintime )) {
      cat( "time reversal!\n" )
      browser()
    }
    tmp <- mintime == Inf
    if( any( tmp )) {
      if( extinct | all( tmp )) {
        for( i in names( mintime )[tmp] )
          cat( "***", i, "is extinct ***\n" )
        if( plotit )
          plot.ewing( community, substrate = substrate.plot, cex = cex )
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
    current <- individual["future"]
    stage <- as.character( future$current[current] )
    print(stage)
    if(!length(stage)) browser()
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
      if( plotit ) {
        plot.ewing( community, substrate = substrate.plot, cex = cex )
      }
      cat( "refresh", istep )
      for( j in get.species( community ))
        cat( ":", j, sum( getCount( community, j, "countage" ), na.rm = TRUE ))
      cat( "\n" )
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
  if( plotit ) {
    if( !refresh | (nstep%%refresh))
      plot.ewing( community, substrate = substrate.plot, cex = cex )
  }
  community <- set.timing( community, "refresh", 1 )

  community <- fini.timing( community )
  community
}
###############################################################################
### Get birth and future event
###############################################################################
get.future <- function (community, species,
                        individuals = get.individual(community, species, id),
                        id = get.base(community, species))
{
  ## NOTE: This is the slow routine. For every event, it has to check if there
  ## is a mean value function and then call rspline.

  ## the structure species.future
  ## is set up right to handle competing risks!
  future <- getOrgFuture(community, species, c("current", "fid", "time"))
  individuals <- as.matrix(individuals)

  rownames <- dimnames(individuals)[[1]]
  for (i in seq(ncol(individuals))) {
    individual <- individuals[,i]
    current <- individual["stage"]
    individual["location"] <- individual["time"]

    ## competing risks based on potential future event times
    futures <- future$current == future$current[current]
    times <- rep( individual["time"], sum( futures ))
    cur <- seq(nrow(future))[futures]
    for( i in seq( sum( futures ))) {
      meantime <- future[cur[i], "time"]
      if( meantime > 0 ) {
        for.stage <- as.character(future$current[future$fid[ cur[i] ]])
        times[i] <- rspline( meantime, individual,
                              getOrgMeanValue(community, species)[[for.stage]])
      }
    }
    individual["time"] <- min( times )
    current <- cur[ times == individual["time"] ][1]
    individual["future"] <- future$fid[current]
    if (individual["time"] == individual["rejection"])
        individual["future"] <- future$fid[future$current == "death"]
    individuals[,i] <- individual
  }
  individuals
}
###############################################################################
event.death <- function( community, species,
                        id = get.base( community, species ))
{
  ## remove dead individual from leftist tree
  community <- put.species( community, species,
                           leftist.remove( get.species( community, species ), id ))
  ## free up individual for reuse
  community <- put.base( community, species, id )
  community
}
###############################################################################
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
    community <- update.mintime( community, species )
  }
  community
}
###############################################################################
update.mintime <- function( object, species, ... )
{
  base <- get.base( object, species )
  mintime <- max( getCount( object, species, "mintime" ),
                 getTime( object, species,
                         get.species.element( object, species, "time", base )))
  setCount( object, species, list( base = base, mintime = mintime ))
}
###############################################################################
set.birth <- function( community, species, neworg )
{
  ## merge immediate new births (if any)
  newbirths <- ncol( neworg )
  if( newbirths > 0 ) {
    neworg[c("dist","left","right","up"),] <- 1
    community <- checkTime( community, neworg["time",],
              getCount( community, species, "mintime" ),
              getOrgFeature( community, species, "units" ))
    oldbase <- getCount( community, species, "base" )
    tmp <- leftist.birth( get.species( community, species ), neworg,
                         getCount( community, species, "free" ))
    
    community <- put.species( community, species, tmp$tree )
    community <- put.base( community, species, free = tmp$free )
    community <- updateCounts( community, species, newbirths )
  }
  community
}



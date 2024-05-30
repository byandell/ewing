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




###############################################################################
### Get birth and future event
###############################################################################
get.future <- function (community, species,
                        individuals = get.individual(community, species, id),
                        id = get.base(community, species))
{
  ## NOTE: This is the slow routine. For every event, it has to check if there
  ## is a mean value function and then call rspline.

  ## the structure future.species is set up to handle competing risks!
  future <- getOrgFuture(community, species, c("current", "future", "time"))
  future$fid <- match(future$future, future$current)
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
    for( j in seq( sum( futures ))) {
      meantime <- future[cur[j], "time"]
      if( meantime > 0 ) {
        for.stage <- as.character(future$current[future$fid[ cur[j] ]])
        times[j] <- rspline( meantime, individual,
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
update_mintime <- function( object, species, ... )
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



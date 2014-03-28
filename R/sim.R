## $Id: sim.R,v 0.9 2002/12/09 yandell@stat.wisc.edu Exp $
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
### migrate count$count* to writeCount and then retrieve with readCount
###########################################################################################
updateCount <- function( community, species, individual, is.death = FALSE, step )
{
  community <- updateEvents( community, species, individual["future"] )
  if( !missing( step ))
    community <- setCount( community,, list( step = step ))

  ## move individual through age classes or drop if it dies
  countage <- getCount( community, species, "countage" )
  ageclass <- getOrgAgeClass( community, species, individual["stage"] )
  if( !is.na( ageclass )) {
    ageclass <- as.character( ageclass )
    countage[ageclass] <- countage[ageclass] - 1
  }
  ageclass <- getOrgAgeClass( community, species, individual["future"] )
  if( !is.na( ageclass )) {
    ageclass <- as.character( ageclass )
    countage[ageclass] <- countage[ageclass] + 1
  }
  ## move individual across substrate elements or drop if it dies
  countsub <- getCount( community, species, "countsub" )
  substrate <- getOrgFeature( community, species, "substrate" )
  elements <- getOrgInteract( community, substrate, species, "substrate" )
  element <- elements[ individual["sub.stage"] ]
  subclass <- getOrgFeature( community, species, "subclass" )
  include <- subclass == getOrgAgeClass( community, species, individual[c("stage","future")] )
  ## leave old substrate
  if( !is.na( element ) & include[1] ) {
    element <- as.character( element )
    countsub[element] <- countsub[element] - 1
  }
  if( !is.death & include[2] ) {
    ## move to new substrate
    newsub <- individual["sub.future"]
    if( !is.na( newsub )) {
      element <- elements[newsub]
      if( !is.na( element )) {
        element <- as.character( element )
        countsub[element] <- countsub[element] + 1
      }
    }
  }
  ## record counts
  community <- setCount( community, species,
                        list( countage = countage, countsub = countsub ))
  writeCount( community, species, individual["time"], individual["future"],
               countage, countsub )
  community
}
###########################################################################################
updateCounts <- function( community, species, newbirths )
{
  community <- updateEvents( community, species, 1, newbirths )
  
  countage <- getCount( community, species, "countage" )
  countsub <- getCount( community, species, "countsub" )
  subclass <- getOrgFeature( community, species, "subclass" )
  ageclass <- getOrgAgeClass( community, species, 1 )
  individual <- get.individual( community, species )
  if( !is.na( ageclass[1] )) {
    ageclass <- as.character( ageclass )
    countage[ageclass] <- newbirths + countage[ageclass]
    if( subclass == ageclass ) {
      substrate <- getOrgSubstrate( community, species, individual["sub.stage"] )
      if( !is.na( substrate[1] )) {
        substrate <- as.character( substrate )
        countsub[substrate] <- newbirths + countsub[substrate]
      }
    }
  }
  community <- setCount( community, species,
                        list( countage = countage, countsub = countsub ))
    writeCount( community, species, individual["time"], 1, countage, countsub )
  community
}
###########################################################################################
put.base <- function( community, species, id,
                     free = getCount( community, species, "free" ))
{
  base <- get.base( community, species )
  if( !missing( id ))
    free <- leftist.free( free, id )
  mintime <- max( getCount( community, species, "mintime" ),
                 getTime( community, species,
                         get.species.element( community, species, "time", base )))
  setCount( community, species, list( base = base, free = free, mintime = mintime ))
}
###########################################################################################
writeCount <- function( community, species, time, future, countage, countsub )
{
  file <- getCount( community,, "file" )
  nstep <- getCount( community,, "step" )
  cat( species, nstep, time, future, countage, countsub, "\n", file = file, append = TRUE )
}
###########################################################################################
readCount <- function( community, species = levels( counts$species ))
{
  file <- getCount( community,, "file" )
  counts <- read.table( file, header = TRUE, fill = TRUE )
  count <- list()
  for( i in species ) {
    colnames <- c( levels( getOrgFuture( community, i, "ageclass" )),
                  levels( getOrgInteract( community,, i, "substrate" )))
    count[[i]] <- as.matrix( counts[ counts$species == i, seq( 2, 4 + length( colnames )) ] )
    dimnames( count[[i]] ) <- list( count[[i]]$step, c( "step", "time", "future", colnames ))
  }
  count
}


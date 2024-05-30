## $Id: init.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
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
###########################################################################################
##
## init.simulation( community )
##
## also includes routines to get objects from Organism files
###########################################################################################




##########################################################################################




##########################################################################################
## Offspring Information
##########################################################################################
getOffspring <- function( community, species,
                         offspring = getOrgFeature( community, species, "offspring" ))
{
  if( is.na( offspring ))
    return( 0 )
  if( is.numeric( offspring ))
    return( offspring )
  getOrgInteract( community, offspring, species, "offspring" )
}
##########################################################################################
initOffspring <- function( community, species )
{
  hostname <- getOrgFeature( community, species, "offspring" )

  ## find if there is offspring load based on host
  orgoffspring <- getOffspring( community, species, hostname )

  norganism <- sum( getOrgAlive( community, species ))

  if( length( orgoffspring ) == 1 ) {
    ## mean offspring does not depend on any host
    offspring <- stats::rpois( norganism, orgoffspring )
  }
  else {
    orgoffspring <- orgoffspring[ orgoffspring > 0 ]
    
    if(!length(orgoffspring))
      return(community)
  
    ## figure out initial offspring load based on host distribution

    ## mean offspring depends on host stages and events
    host <- get.species( community, hostname )
    if( is.null( host ))
      stop( paste( "Host", hostname, "not initiated yet" ))

    ## get weights of host stages in terms of future event times
    host <- host[ , getOrgAlive( community, hostname ) ]

    ## find host stages that are preferred by parasite
    ## need to take subset of current that are actually in host
    hoststages <- match( names( orgoffspring ), getOrgFuture( community, hostname )$current,
                        nomatch = 0 )
    host <- as.matrix( host[ , !is.na( match( host["stage",], hoststages )) ] )
    if( ncol( host ) == 0 )
      return( community )

    tmp <- !is.na( match( hoststages, host["stage",] ))
    hoststages <- hoststages[tmp]
    orgoffspring <- orgoffspring[tmp]

    if(!length(orgoffspring))
      return(community)
    
    dd <- tapply( host["time",], host["stage",], sum )
    dd[ as.character( hoststages[
      is.na( match( hoststages, names( dd ))) ] ) ] <- 0
    dd[ is.na( dd ) ] <- 0
    sdd <- sum( dd )
    if( length( dd ) > 1 & sdd > 0)
      offspring <- as.vector( sample( orgoffspring, norganism, replace = TRUE,
        prob = dd / sdd ))
    else
      offspring <- rep( ( sdd > 0 ) * orgoffspring[1], norganism )
    offspring[ is.na( offspring ) ] <- 0
  }
  organism <- get.species( community, species )
  organism["offspring",-1] <- offspring
  put.species( community, species, organism )
}
###############################################################################
get.offspring <- function( community, species )
{
  individual <- get.individual( community, species )
  if( individual["offspring"] > 0 )
    1
  else
    0
}
###########################################################################################
set.offspring <- function( community, species, host, dead )
{
  stage <- get.species.element( community, host, "stage", dead )
  current <- getOrgFuture( community, host, "current", stage )
  offspring <- getOrgInteract( community, host, species, "offspring")
  offspring <- as.vector( offspring[ as.character( current ) ] )
  offspring[ is.na( offspring ) ] <- 0
  offspring
}

## $Id: move.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
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
event.move <- function( community, species, individual )
{
  individual <- as.matrix( individual )
  ## move only if individual is in stage that moves
  if( all( is.move( community, species, individual ))) {
    ## move among substrates?
    individual["sub.future",] <-
      sampleOrgSubstrate( community, species, individual["sub.stage",] )
    ## move to new position in substrate
    position <- paste( "pos", letters[1:3], sep = "." )
    individual[position,] <- rtri( ncol( individual ), 10, individual[position,] )
  }
  if( ncol( individual ) == 1 )
    individual <- individual[,1]
  individual
}
###############################################################################
is.move <- function( community, species, individual )
{
  !is.na( match( getOrgFeature( community, species, "move" ), 
                getOrgFuture( community, species, "current" )[ individual["stage",] ] ))
}
###############################################################################
event.find <- function( community, species, host, event )
{
  individual <- get.individual( community, species )
  substrate <- individual["sub.stage"]
  ## pending event: need to find available hosts on substrate
  avail <- get.alive( community, host, substrate )
  navail <- length( avail )
  if( !navail )
    return( avail )
  
  ## preferences based on schedule
  interact <- get.interact( community, species, host, avail, event )
  if( length( event ) > 1 )
    interact <- apply( interact, 1, sum )
  sinteract <- sum( interact )
  if( sinteract ) {
    if( navail > 1 ) {
      found <- sample( avail, 1, prob = interact / sinteract )
    }
    else
      found <- avail
    found
  }
  else
    numeric(0)
}

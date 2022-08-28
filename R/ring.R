## $Id: ring.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
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
## Doubly linked rings
###########################################################################################
ring.add <- function( ring = data.frame( root = c( key = NA, left = 1 + nx, right = 2 )),
  x )
{
  nx <- length( x )
  n1 <- ring["left","root"]
  ring[[ as.character( n1 + 1 ) ]] <- c( key = x[1],
    left = suppressWarnings(as.numeric( ring["left","root"] ), right = 1 ))
  
  if( nx > 1 ) for( i in seq( 2, nx ))
  {
    ring[[ as.character( n1 + i ) ]] <- c( key = x[i], left = n1+i-1, right = 1 )
    ring[ "right", as.character( n1 + i - 1 ) ] <- n1+i
  }
  ring["left","root"] <- n1 + nx
  ring
}
###########################################################################################
ring.remove <- function( ring, P )
{
  aP <- as.character( P )
  if( is.na( match( aP, names( ring ))))
  {
    cat( paste( "Warning:", aP, "not found in ring\n" ))
    return( ring )
  }
  left <- ring["left",aP]
  right <- ring["right",aP]
  ring[ "right", as.character( left ) ] <- right
  ring[ "left", as.character( right ) ] <- left
  ring[[aP]] <- NULL
  ring
}

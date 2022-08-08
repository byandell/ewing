## $Id: plot.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
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
### Plot and Print Routines
###########################################################################################
count.join <- function( ... )
{
  x <- list( ... )
  numnum <- list()
  for( i in seq( length( x ))) {
    for( j in names( x[[i]] )) {
      if( is.null( numnum[[j]] ))
        numnum[[j]] <- x[[i]][[j]]
      else
        numnum[[j]] <- cbind( numnum[[j]], x[[i]][[j]] )
    }
  }
  numnum
}
###########################################################################################


#' plot of Ewing simulation models
#' 
#' Plot of various aspects of simulation.
#' 
#' 
#' @param x object of class \code{ewing} with population data by species
#' @param substrate include substrate plot if TRUE
#' @param mfcol par parameter reset by default
#' @param ... other plot parameters
#' @author Brian S. Yandell, \email{yandell@@stat.wisc.edu}
#' @seealso \code{\link{init.simulation}}, \code{\link{future.events}},
#' \code{\link{summary.ewing}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' plot.ewing( community )
#' }
#' 
#' @importFrom graphics axis lines mtext par
#' @importFrom stats runif
#' 
plot.ewing <- function( x, substrate = TRUE, mfcol = mfcols, ... )
{
  count <- readCount( x )
  mfcols <- if( substrate )
    c( length( count ), 2 )
  else
    c( 1, length( count ))
  ageclass.plot( x, count, mfcol, "age classes", ... )

  if( substrate )
    substrate.plot( x, count, NULL, "substrates", ... )
  invisible()
}
###########################################################################################
substrate.plot <- function( community, count,
  mfcol = c( nplot, 1 ),
  main = "",
  col = 1:8, cex = 0.65, logaxis= "y",
  debugit = FALSE)
{
  species <- names( count )
  nplot <- length( count )
  extra <- 2 * (1 + is.null( mfcol ))
  substrate <- list()
  maxpaths <- 0
  for( i in species ) {
    substrate[[i]] <- levels( getOrgInteract( community,, i, "substrate" ))
    maxpaths <- max( maxpaths, length( substrate[[i]] ))
  }
  maxpaths <- maxpaths + 1

  if( extra == 2 )
    par( mfcol = mfcol, omi = c(0.5,0.5,0.5,0.5) )
  opwarn <- options( warn = -1 ) ## turn off warning of log(0)
  maxtime <- NULL
  unitss <- NULL
  ncounts <- 0
  for( i in species ) {
    ## set up total count
    mycount <- count[[i]][ , substrate[[i]] ]
    if( !is.matrix( mycount ))
      mycount <- t( mycount )
    mytotal <- pmax( 1, apply( mycount, 1, sum ))
    ncount <- nrow( mycount )
    ncounts <- ncounts + ncount
    paths <- substrate[[i]]
    npaths <- length( paths )
    for( j in seq( npaths ))
      mycount[,paths[j]] <- mycount[,paths[j]] / mytotal
    ry <- range( mycount, na.rm = TRUE )

    mar <- c( 6, 0.5, 1, 0.5 )
    mar[extra] <- 1 + 1.5 * maxpaths
    graphics::par( mar = .1 + mar )
    if( debugit )
      browser()

    units <- getOrgFeature( community, i, "units" )
    unitss <- c( unitss, units )
    mytime <- count[[i]][,"time"]
    rx <- range( mytime, na.rm = TRUE )
    maxtime <- c( maxtime, rx[2] )

    opar <- graphics::par( cex = cex, yaxt = "n" )
    if( any( is.na( rx )) | max( rx ) == 0 ) {
      cat( "Warning: no times for species", i, "\n" )
      plot( c(1,1),c(1,1), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "" )
      graphics::mtext( paste( i, 0, sep = ": " ), 1, 2.5, cex = cex )
      break
    }
    plot( rx, ry, type = "n", xlab = "", ylab = "", col = col[1], cex = cex )
    usr <- par("usr")
    graphics::mtext( paste( getOrgFeature( community, i, "subclass" ), mytotal[ncount], sep = ": " ),
          1, 2.5, cex = cex )
    if( i == species[1] )
      graphics::mtext( main, 1, 4.5 )
    graphics::par( opar )
    opar <- graphics::par( cex = cex )
    graphics::axis( extra, pretty( c(0,1) ))
    graphics::par( opar )

    for( j in seq( npaths )) {
      y <- mycount[,paths[j]]
      ## add extra colored axes
      graphics::mtext( paths[j], extra, 1.5 * j + 1, col = col[j+1], cex = cex, at = y[ncount],
            adj = 0.5 )
      graphics::lines( mytime, y, col = col[j+1] )
    }
  }
  options( opwarn )
  if( extra == 2 )
    graphics::mtext( paste( paste( round( maxtime ), unitss, collapse = ", " ),
      ", ", ncounts, " events", sep = "" ), 3, outer = TRUE )
  invisible()
}
###########################################################################################
ageclass.plot <- function( community, count,
  mfcol = c( nplot, 1 ),
  main = "",
  col = 1:8, cex = 0.65,
  debugit = FALSE)
{
  species <- names( count )
  nplot <- length( count )
  extra <- 2 * (1 + is.null( mfcol ))
  ageclass <- list()
  maxpaths <- 0
  for( i in species ) {
    ageclass[[i]] <- levels( getOrgFuture( community, i, "ageclass" ))
    maxpaths <- max( maxpaths, length( ageclass[[i]] ))
  }
  maxpaths <- maxpaths + 1

  if( extra == 2 )
    graphics::par( mfcol = mfcol, omi = c(0.5,0.5,0.5,0.5) )
  opwarn <- options( warn = -1 ) ## turn off warning of log(0)
  maxtime <- NULL
  unitss <- NULL
  ncounts <- 0
  for( i in species ) {
    ## set up total count
    mycount <- count[[i]][ , ageclass[[i]] ]
    if( !is.matrix( mycount ))
      mycount <- t( mycount )
    mycount <- cbind( total = apply( mycount, 1, sum ), mycount )
    ncount <- nrow( mycount )
    ncounts <- ncounts + ncount
    paths <- c( "total", ageclass[[i]] )
    npaths <- length( paths )
    
    mar <- c( 6, 0.5, 1, 0.5 )
    mar[extra] <- 1 + 1.5 * maxpaths
    graphics::par( mar = .1 + mar )
    if( debugit )
      browser()

    ry <- range( mycount[,"total"], na.rm = TRUE )

    units <- getOrgFeature( community, i, "units" )
    unitss <- c( unitss, units )
    mytime <- count[[i]][,"time"]
    rx <- range( mytime, na.rm = TRUE )
    maxtime <- c( maxtime, rx[2] )

    opar <- graphics::par( cex = cex, yaxt = "n" )
    if( any( is.na( rx )) | max( rx ) == 0 ) {
      cat( "Warning: no times for species", i, "\n" )
      plot( c(1,1),c(1,1), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "" )
      graphics::mtext( paste( i, 0, sep = ": " ), 1, 2.5, cex = cex )
      break
    }
    plot( rx, 1 + ry, log = "y", type = "n", xlab = "", ylab = "", col = col[1], cex = cex )
    usr <- graphics::par("usr")
    graphics::mtext( paste( paths[1], " " ), extra, 1, cex = cex, at = 10^usr[3], adj = 1 )
    graphics::mtext( paste( i, mycount[ncount,"total"], sep = ": " ), 1, 2.5, cex = cex )
    if( i == species[1] )
      graphics::mtext( main, 1, 4.5 )
    graphics::par( opar )

    if( !diff( ry ))
      ry <- 10 ^ ( usr[3:4] * c(1.02,.98) )
    for( j in seq( npaths )) {
      y <- mycount[ , paths[j] ]
      p1 <- logpretty( y, 4 )
      if( j == 1 ) {
        opar <- graphics::par( cex = cex )
        if( diff( range( y, na.rm = TRUE )))
          graphics::axis( extra, 1 + p1, as.character( p1 ))
        else
          graphics::axis( extra, 1 + y[1], as.character( y[1] ))
        graphics::par( opar )
      }
      else {
        ## rescale values and add extra colored axes
        ryj <- range( y, na.rm = TRUE )
        difr <- diff( ryj )
        if( is.na( difr ))
          difr <- 0
        if( difr > 0 ) {
          y <- ry[1] + ( y - ryj[1] ) * diff( ry ) / difr
          p2 <- ry[1] + ( p1 - ryj[1] ) * diff( ry ) / difr
        }
        else
        {
          p1 <- y[1]
          tmp <- diff( usr[3:4] )
          p2 <- 10 ^ ( mean( usr[3:4] ) + .04 * stats::runif( 1, -tmp, tmp ))
          y <- p2 + y - p1
        }
	  tmp <- log10( p2 ) > usr[3] & log10( p2 ) < usr[4]
        tmp[ is.na( tmp ) ] <- FALSE
        graphics::mtext( as.character( p1[tmp] ), extra, 1.5 * j - .5, at = 1 + p2[tmp],
          col = col[j], cex = cex )
        graphics::mtext( paste( paths[j], " " ), extra, 1.5 * j - .5, col = col[j], cex = cex,
          at = 10^usr[3], adj = 1 )
      }
      graphics::lines( mytime, 1 + y, col = col[j] )
    }
  }
  options( opwarn )
  if( extra == 2 )
    graphics::mtext( paste( paste( round( maxtime ), unitss, collapse = ", " ),
      ", ", ncounts, " events", sep = "" ), 3, outer = TRUE )
  invisible()
}
###########################################################################################
logpretty <- function( x, n = 5, roundit = TRUE )
{
  gx <- range( x[x>0], na.rm = TRUE )
  if( any( is.na( gx )))
    return( numeric( 0 ))
  rx <- log10( gx )
  px <- pretty( rx, n )
  wx <- px == round( px )
  if( sum( wx ) < 3 )
    px <- pretty( gx, n )
  else {
    px <- 10^px[wx]
    if( length( px ) < n ) {
      px5 <- sort( c( .5*px[1], px, 5*px ))
      px5 <- px5[ px5 >= gx[1] & px5 <= gx[2] ]
      if( length( px5 ) >= n )
        px <- px5
      else {
        px <- sort( c( .2*px[1], px5, 2*px ))
        px <- px[ px >= gx[1] & px <= gx[2] ]
      }
    }
  }
  if( roundit )
    px[ px == round( px ) ]
  else
    px
}

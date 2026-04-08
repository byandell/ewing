## $Id: triangle.R,v 0.9 2002/12/09 yandell@stat.wisc.edu Exp $
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
## rtri( n, width )
##
## plot.current( x, species )
## text.current( x, species )
##
###########################################################################################
###########################################################################################
### Tridiagonal Coordinate System S3 Classes & Algebra
###########################################################################################
#' @export
tricoord <- function(a, b = NULL, c = NULL) {
  if (is.data.frame(a) && all(c("a", "b", "c") %in% names(a))) {
    res <- a
  } else if (is.matrix(a) && ncol(a) == 3) {
    res <- as.data.frame(a)
    names(res) <- c("a", "b", "c")
  } else if (!is.null(b) && !is.null(c)) {
    res <- data.frame(a = a, b = b, c = c)
  } else if (is.numeric(a) && length(a) == 3) {
    res <- data.frame(a = a[1], b = a[2], c = a[3])
  } else {
    stop("Invalid tricoord input format")
  }
  class(res) <- c("tricoord", "data.frame")
  res
}

#' @export
`+.tricoord` <- function(e1, e2) {
  # Handle vector offsets cleanly
  if (is.numeric(e2) && length(e2) == 3) {
    tricoord(e1$a + e2[1], e1$b + e2[2], e1$c + e2[3])
  } else if (inherits(e2, "tricoord")) {
    tricoord(e1$a + e2$a, e1$b + e2$b, e1$c + e2$c)
  } else {
    stop("Invalid right hand operand for tricoord addition")
  }
}

#' @export
`-.tricoord` <- function(e1, e2) {
  if (is.numeric(e2) && length(e2) == 3) {
    tricoord(e1$a - e2[1], e1$b - e2[2], e1$c - e2[3])
  } else if (inherits(e2, "tricoord")) {
    tricoord(e1$a - e2$a, e1$b - e2$b, e1$c - e2$c)
  } else {
    stop("Invalid right hand operand for tricoord subtraction")
  }
}

###########################################################################################
#' @importFrom stats runif
rtri <- function( n, width, tri = matrix(0,3,n), roundoff = TRUE )
{
  tri <- as.matrix( tri )
  if( n == 1 ) {
    xy <- stats::runif( 2, 0, width )
    if( roundoff )
      xy <- round( xy )

    i <- sample( 3, 1  )
    i1 <- 1 + i%%3
    i2 <- 1 + (i+1)%%3
    tri[i1,] <- tri[i1,] + xy[1]
    tri[i2,] <- tri[i2,] - xy[2]
    tri[i,] <- - ( tri[i1,] + tri[i2,] )
    return( tri )
  }
  else {
    xy <- data.frame( x = stats::runif( n, 0, width ),
      y = - stats::runif( n, 0, width ))
    if( roundoff )
      xy <- round( xy )

    out <- sample( 3, n, replace = TRUE )
    for( i in 1:3 ) {
      outi <- out == i
      if( any( outi )) {
        i1 <- 1 + i%%3
        i2 <- 1 + (i+1)%%3
        tri[i1,outi] <- tri[i1,outi] + xy$x[outi]
        tri[i2,outi] <- tri[i2,outi] + xy$y[outi]
        tri[i,outi] <- - ( tri[i1,outi] + tri[i2,outi] )
      }
    }
  }
  tri
}
###########################################################################################
car2tri.default <- function(x,y)
  car2tri( cbind( x, y ))
car2tri <- function( xy, xmult = ( 2 + sq3 ) / 4, ymult = ( 3 + 2 * sq3 ) / 12,
  sq3 = sqrt( 3 ))
{
#  if( !is.matrix( xy ))
#    xy <- t( as.matrix( xy ))
  aa <- xmult * xy[,1] - ymult * xy[,2]
  bb <- - xmult * xy[,1] - ymult * xy[,2]
  cc <- -( aa + bb )
  rbind( a = aa, b = bb, c = cc )
}
###########################################################################################
tri2car.default <- function(aa,bb,cc=-(aa+bb))
  tri2car( rbind( aa, bb, cc ))
tri2car <- function(tri, xmult = 2 / ( 2 + sq3 ), ymult = 6 / ( 3 + 2 * sq3 ),
  sq3 = sqrt( 3 ))
{
  if( inherits(tri, "tricoord") ) {
    # If the user passes our S3 tricoord dataframe, map it correctly natively.
    x <- ( tri$a - tri$b ) * xmult
    y <- - ( tri$a + tri$b ) * ymult
  } else {
    if( !is.matrix( tri ))
      tri <- as.matrix( tri )
    x <- ( tri[1,] - tri[2,] ) * xmult
    y <- - ( tri[1,] + tri[2,] ) * ymult
  }
  data.frame( x = x, y = y )
}
###########################################################################################
cardist <- function( xy )
  sqrt( xy[,1]^2 + xy[,2]^2 )
###########################################################################################
tridist <- function( tri )
  apply( tri, 1, max )
###########################################################################################
gasket <- function( aa, bb )
{
  n <- length( aa )
  pp <- c(-1,1,0,1)
  dda <- diff( aa )
  ddb <- diff( bb )
  ss <- sign( sign( dda ) - sign( ddb ))
  dda <- 2 - abs( dda )
  ddb <- 2 - abs( ddb )
  aa <- 2 * aa
  aa <- c( aa[1], rbind( aa[-n] + pp[dda+1+ss], aa[-1] + pp[dda+1-ss], aa[-1] ))
  bb <- 2 * bb
  bb <- c( bb[1], rbind( bb[-n] + pp[ddb+1-ss], bb[-1] + pp[ddb+1+ss], bb[-1] ))
  data.frame( aa = aa, bb = bb )
}

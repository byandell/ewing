#' @export
#' @importFrom graphics axis lines mtext par text
#' @importFrom stats runif
sierpinski <- function( stage = 5, reset = TRUE )
{
  if( reset )
    tmpar <- graphics::par( pty = "s", bty = "n", xaxt = "n", yaxt = "n", omi = rep(0,4),
                            mar = rep(0,4) )
  aa <- 0:1
  bb <- - aa
  for( i in seq( stage )) {
    tmp <- gasket( aa, bb )
    aa <- tmp$aa
    bb <- tmp$bb
    tri <- tri2car.default( aa, bb )
    r <- range( unlist( tri ))
    plot( r, r, type = "n", xlab = "", ylab = "" )
    graphics::lines(tri )
    graphics::mtext( paste( "(", letters[1+i], ") Gasket of Order ", i, sep = "" ), 3, -2 )
    #    graphics::mtext( paste( "Gasket of order", i ), 3, -2 )
  }
  if( reset )
    graphics::par( tmpar )
  invisible( tri )
}

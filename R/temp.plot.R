#' @export
#' @importFrom stats predict
temp.plot <- function( community, lo.hour = s$knots[1], hi.hour = max( s$knots ),
                       length = 201,
                       col = NULL, derivative = FALSE, ..., printit = FALSE )
{
  s <- getTemp( community, "DegreeDay" )
  x <- seq( lo.hour, hi.hour, length = length )
  ## make sure to pick up knots to plot in this region
  x <- unique( sort( c( x, s$knots[ s$knots >= lo.hour & s$knots <= hi.hour ] )))
  ylab <- "degree-days"
  if( derivative ) {
    ylab <- "degrees above min"
    s$coefficients <- s$coefficients[,-1]
    for( i in seq( 2, ncol( s$coefficients )))
      s$coefficients[,i] <- s$coefficients[,i] * i
  }
  y <- stats::predict( s, x )$y
  plot( x / getTemp( community, "Unit" ), y, type = "l", xlab = "day",
        ylab = ylab, ... )
  if( printit )
    print( cbind( hi.hour, stats::predict( s, hi.hour )$y ))
  if( !is.null( col ))
    points( s$knots, coef(s)[,1], col = col )
  if( !derivative ) {
    s <- getTemp( community, "Hour" )
    x <- seq( min( y ), max( y ), length = length )
    tmp <- stats::predict( s, x )
    lines( tmp$y / getTemp( community, "Unit" ), tmp$x, col = "blue" )
  }
}
###########################################################################################
temp.lines <- function( s, mult = 24, col = "red" )
{
  x <- seq( s$knots[1], max( s$knots ), length = 51 )
  x <- unique( sort( c( x, s$knots )))
  p <- stats::predict( s, x )
  lines( p$x, p$y, col = col )
  if( !is.null( col ))
    points( s$knots / mult, coef(s)[,1], col = col )
}

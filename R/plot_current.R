#' @export
#' @importFrom graphics lines mtext par text
plot_current <- function( x,
                          species,
                          col = as.character( future$color[stage] ),
                          headstuff = c( 0, "start", sum( to.plot )),
                          units = getOrgFeature( x, species, "units" ),
                          right = species, adj = c(0,.5,1),
                          position = paste( "pos", letters[1:3], sep = "." ),
                          pch = as.character( future$pch[stage] ), cex = 0.5,
                          stage = organism["stage",],
                          xlab = "horizontal", ylab = "vertical",
                          future = getOrgFuture( x, species, c("color","pch") ),...)
{
  ## plot current stages for species (except random parasites)
  organism <- get.species( x, species )[,-1]
  if(is.null(organism))
    return(NULL)
  
  tmp <- tri2car( organism[position,] )
  xlim <- range( c( 0, 1.1 * tmp$x ))
  ylim <- range( c( 0, 1.1 * tmp$y ))
  
  plot( tmp$x, tmp$y, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab )
  for( i in levels( future$color )) {
    u <- i == col
    if( any( u ))
      graphics::text( tmp$x[u], tmp$y[u], pch[u], col = i, cex = cex )  
  }
  usr <- graphics::par( "usr" )[1:2]
  usr <- c(usr[1],mean(usr),usr[2])
  graphics::mtext( c( units, "future event", right ), 3, 1.5, at = usr, adj = adj )
  to.plot <- 0 # not sure what this is
  graphics::mtext( headstuff, 3, 0.5, at = usr, adj = adj )
  invisible( usr )
}

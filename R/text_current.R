#' @export
#' @importFrom graphics text
text_current <- function( x, species,
                          col = as.character( future$color[stage] ),
                          pch = as.character( future$pch[stage] ), cex = 0.5,
                          position = paste( "pos", letters[1:3], sep = "." ),
                          stage = organism["stage",],
                          future = getOrgFuture( x, species, c("color","pch") ),...)
{
  organism <- as.matrix( get.species( x, species ))[,-1]
  tmp <- tri2car( organism[position,] )
  graphics::text( tmp$x, tmp$y, pch, col = col, cex = cex )
}

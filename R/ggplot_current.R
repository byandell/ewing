#' @export
#' @importFrom graphics lines mtext par text
#' @importFrom ggplot2 aes facet_wrap geom_text ggplot ggtitle
#'             scale_color_manual theme xlab ylab
ggplot_current <- function( x,
                          species,
                          col = as.character( future$color[stage] ),
                          headstuff = c( 0, "start"),
                          units = getOrgFeature( x, species, "units" ),
                          right = species, adj = c(0,.5,1),
                          position = paste( "pos", letters[1:3], sep = "." ),
                          pch = as.character( future$pch[stage] ), cex = 0.5,
                          stage = organism["stage",],
                          xlab = "horizontal", ylab = "vertical",
                          future = getOrgFuture( x, species, c("color","pch") ),
                          facet = TRUE, ...)
{
  ## plot current stages for species (except random parasites)
  organism <- get.species( x, species )[,-1]
  if(is.null(organism))
    return(NULL)
  
  tri_coord <- tri2car( organism[position,] )
  tri_coord$col <- NA
  tri_coord$label <- pch
  values <- future$color
  names(values) <- future$pch
  values <- unique(values)

  # Facet by Substrate.
  # This needs to use generic function to get substrate names.
  tmp <- names(x$org$Interact$substrate$substrate)
  tri_coord$substrate <- tmp[organism["sub.stage",]]
  
  p <- ggplot2::ggplot(tri_coord) +
    ggplot2::aes(x, y, col = label, label = label) +
    ggplot2::geom_text(size=3) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(paste(units, "future event", right),
      subtitle = paste(headstuff, collapse = " ")) +
    ggplot2::scale_color_manual(values = values)
  if(facet) {
    p <- p + ggplot2::facet_wrap(~substrate)
  }
  p
}

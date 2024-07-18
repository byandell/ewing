#' Ewing Substrate by Species
#' 
#' Ewing Substrate by Species
#' 
#' 
#' @aliases ewing_substrate ggplot_ewing_substrate autoplot.ewing_substrate
#' @export ewing_substrate
#' @importFrom dplyr distinct filter mutate
#' @importFrom tibble tibble
#' @importFrom ggplot2 aes facet_grid geom_text ggplot ggtitle
#'             scale_color_manual xlab ylab
#' @importFrom rlang .data
ewing_substrate <- function( community,
                             species,
                             headstuff = c( 0, "start", sum( to.plot )),
                             units = getOrgFeature( community, species, "units" ),
                             right = species, adj = c(0,.5,1),
                             show_sub = substrates,
                             step = 0,
                             ...)
{
  ## plot current stages for species (except random parasites)
  organism <- get.species( community, species )[,-1]
  if(is.null(organism)) # species is not in community
    return(NULL)
  
  future = getOrgFuture( community, species, c("color","pch") )
  
  # Substrate names
  substrate <- getOrgFeature( community, species, "substrate")
  substrates <- names(getOrgInteract(community, substrate, substrate))
  
  # Convert triangular coordinates into Cartesian (xy) coordinates
  position = paste( "pos", letters[1:3], sep = "." )
  xy <- tri2car( organism[position,] )
  
  dat <- dplyr::filter(
    dplyr::mutate(
      tibble::tibble(xy), 
      stage = organism["stage",],
      substrate = substrates[organism["sub.stage",]],
      pchar = as.character( future$pch[.data$stage] ),
      color = as.character( future$color[.data$stage] )),
    .data$substrate %in% show_sub)
  
  attr(dat, "species") <- species
  attr(dat, "step") <- step
  class(dat) <- c("ewing_substrate", class(dat))
  dat
}

#' @importFrom ggplot2 aes facet_grid geom_text ggplot scale_color_manual xlab ylab
#' @importFrom rlang .data
#' @importFrom dplyr distinct
#' @rdname ewing_substrate
#' @export
ggplot_ewing_substrate <- function(object,
                                   xlab = "horizontal", ylab = "vertical",
                                   ...)
{     
  # Allows same color for different pchar, but only one color per pchar.
  tmp <- dplyr::distinct(
    dplyr::distinct(object, .data$pchar, .data$color),
    .data$pchar, .keep_all = TRUE)
  col.palate <- tmp$color
  names(col.palate) <- tmp$pchar
  
  species <- attr(object, "species")
  step <- attr(object, "step")
  
  ggplot2::ggplot(object) +
    ggplot2::aes(.data$x, .data$y, label = .data$pchar, col = .data$pchar) +
    ggplot2::geom_text() +
    ggplot2::facet_grid(. ~ substrate) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_color_manual(name = "Stage", values = col.palate) + 
    ggplot2::ggtitle(paste(species, "on substrate at", step, "steps"))
}
#' @export
#' @rdname ewing_substrate
#' @method autoplot ewing_substrate
autoplot.ewing_substrate <- function(object, ...)
  ggplot_ewing_substrate(object, ...)

#' ggplot of Ewing simulation model age classes
#' 
#' GGPlot of various aspects of simulation.
#' 
#' 
#' @aliases ggplot_ewing autoplot.ewing ggplot_ewing_ageclass
#' autoplot.ewing_ageclass
#' @param ... other plot parameters
#' @param x object of class \code{ewing} with population data by species
#' @author Brian S. Yandell, \email{yandell@@stat.wisc.edu}
#' @seealso \code{\link{init.simulation}}, \code{\link{future.events}},
#' \code{\link{summary.ewing}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' ggplot_ewing( community )
#' }
#' 
#' 
#' @export
#' @importFrom ggplot2 autoplot
ggplot_ewing <- function(object, step = 0, ageclass = TRUE, substrate = TRUE, ...)
{
  if(!inherits(object, "ewing_snapshot")) {
    object <- ewing_snapshot(object, step, ...)
  }
  step <- object$step
  
  p <- list()
  i <- 0
  if(ageclass) {
    i <- i + 1
    p[[i]] <- ggplot2::autoplot(object$ageclass, ...)
  }
  if(substrate) {
    species <- get.species(object)
    for(j in species) {
      i <- i + 1
      p[[i]] <- ggplot2::autoplot(object$substrate[[j]], ...)
    }
  }
  p
}
#' @export
#' @rdname ggplot_ewing
#' @method autoplot ewing
autoplot.ewing <- function(object, ...) {
  ggplot_ewing(object, ...)
}

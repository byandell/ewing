#' summary of Ewing simulation models
#' 
#' Summary of various aspects of simulation.
#' 
#' 
#' @param object object of class \code{ewing} with population data by species
#' @param ... other summary parameters
#' @author Brian S. Yandell, \email{yandell@@stat.wisc.edu}
#' @seealso \code{\link{init.simulation}}, \code{\link{future.events}},
#' \code{\link{plot.ewing}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' summary.ewing( community )
#' }
#' 
#' 
#' @export summary.ewing
#' @method summary ewing
summary.ewing <- function(object, ...) {
  out <- list()
  out$package <- object$org$package
  out$species <- names(object$org$Future)
  out$interact <- names(object$org$Interact)
  out$meanvalue <- list()
  for(i in out$species) {
    out$meanvalue <- names(object$org$MeanValue[[i]])
  }
  if(length(object$pop)) {
    out$stage <- list()
    for(i in out$species)
      out$stage[[i]] <- 
        table(object$org$Future[[i]]$current[getOrgAlive(object, i, "stage")])
  }
  if(!is.null(object$count)) {
    out$events <- object$count$events
    
    for( i in seq( length(out$events)))
      out$events[[i]] <- apply(out$events[[i]], 2, function(x) {
        tmp <- sum(x, na.rm = TRUE)
        if(tmp > 0)
          c(round(100 * x / tmp, 1), total = tmp)
        else
          c(x, total = 0)
      })
  }
  out$cpu <- signif(object$cpu, 4)
  class(out) <- c("summary.ewing", class(out))
  out
}
#' @export print.summary.ewing
#' @method print summary.ewing
print.summary.ewing <- function(x, ...) {
  cat("Data initialization package:", x$package, "\n")
  cat("Community species:", paste(x$species, collapse = ", " ), "\n")
  cat("Community hosts:", paste(x$interact, collapse = ", "), "\n")
  cat("Mean Value curves by species:")
  mv <- FALSE
  for(i in x$species) {
    meanvalue <- x$meanvalue[[i]]
    mv <- mv | !is.null(meanvalue)
    if( !is.null(meanvalue)) {
      cat("\n  ", i, ":", paste( meanvalue, collapse = ", " ), "\n")
    }
  }
  if(!mv)
    cat(" none\n")
  
  if(!is.null(x$stage)) {
    cat("\nSimulation community has following counts:\n",
        paste(x$species, lapply(x$stage, sum), sep = "=", collapse = ", "),
        "\n")
    print(x$stage)
  }
  if( !is.null( x$temp )) {
    # ** later
  }
  if(!is.null(x$events)) {
    print(x$events)
  }
  if(!is.null(x$cpu)) {
    cat( "CPU timing by event in simulation\n" )
    print(x$cpu)
  }
}

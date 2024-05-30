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
summary.ewing <- function( object, ... )
{
  cat( "Data initialization package:", object$org$package, "\n" )
  species <- names( object$org$Future )
  cat( "Community species:", paste( species, collapse = ", " ), "\n" )
  cat( "Community hosts:", paste( names( object$org$Interact ), collapse = ", " ), "\n" )
  cat( "Mean Value curves by species:" )
  mv <- FALSE
  for( i in species ) {
    meanvalue <- names( object$org$MeanValue[[i]] )
    mv <- mv | !is.null( meanvalue )
    if( !is.null( meanvalue )) {
      cat( "\n  ", species, ":", paste( meanvalue, collapse = ", " ), "\n" )
    }
  }
  if( !mv )
    cat( " none\n" )
  
  if( length( object$pop )) {
    species <- get.species( object )
    stage <- list()
    for( i in species )
      stage[[i]] <- table( object$org$Future[[i]]$current[
        getOrgAlive( object, i, "stage" ) ] )
    cat( "\nSimulation community has following counts:\n",
         paste( species, lapply( stage, sum ), sep = "=", collapse = ", " ), "\n" )
    print( stage )
  }
  if( !is.null( object$temp )) {
  }
  if( !is.null( object$count )) {
    events <- object$count$events
    
    for( i in seq( length( events )))
      events[[i]] <- apply( events[[i]], 2,
                            function( x ) {
                              tmp <- sum( x, na.rm = TRUE )
                              if( tmp > 0 )
                                c( round( 100 * x / tmp, 1 ), total = tmp )
                              else
                                c( x, total = 0 )
                            })
    print( events )
  }
  if( !is.null( object$cpu )) {
    cat( "CPU timing by event in simulation\n" )
    print( object$cpu )
  }
}

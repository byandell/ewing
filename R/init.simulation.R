#' Initialize simulation run
#' 
#' Initializes simulation using organism features. Default data are used unless
#' user provides global data or uses the hidden argument `datafile` to specify
#' a folder with user data.
#' 
#' 
#' @param package package where community data features can be found
#' @param count number of individuals per species (single number or count in
#' order of `species`)
#' @param interact ask for species count if `TRUE` and interactive
#' @param messages show messages if `TRUE` (default)
#' @param ... additional arguments for `init.population`
#' @return Object with elements for each species that are created by
#' init.population.
#' @author Brian S. Yandell
#' @seealso \code{\link{init.population}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{init.simulation(myrun)}
#' 
#' 
#' @export init.simulation
init.simulation <- function( package = "ewing", 
                             count = 200,
                             interact = FALSE,
                             messages = TRUE,
                             ...)
{
  community <- initOrgInfo( package, messages = messages, ... )
  community <- initTemp( community, messages = messages, ... )
  
  species <- getOrgFeature( community )[1:2]
  hosts <- getOrgHosts( community, species )
  
  if(messages) {
    cat( "Creating simulation organism set using species:\n",
         paste( species, collapse = ", " ), "\n\n" )
  }
  
  community <- setOrgInfo( community, species, hosts, package,
                           messages = messages, ... )
  
  if(messages) {
    cat( "\n" )
  }
  species <- unique( species )
  num <- numeric( length( species ))
  names( num ) <- species
  
  count <- rep_len(count, length(species))
  names(count) <- species
  
  for( i in species ) {
    num[i] <- reuse <- count[i]
    
    if(messages | (interact & interactive())) {
      cat( paste( "Initialize ", i, " at size ", reuse, sep = "" ))
    }
    if(interact & interactive()) {
      cat(" :")
      r <- readline( )
      if( r != "" & is.na( pmatch( substring( r, 1, 1 ), c("y","Y") )))
        reuse <- suppressWarnings(as.numeric( r ))
      if( is.na( reuse ))
        reuse <- count[i]
    }
    if( reuse ) {
      if(messages) {
        cat( "...\n" )
      }
      community <- init.population( community, i, n = reuse, messages = messages, ... )
      num[i] <- reuse
    }
  }
  class( community ) <- c("ewing", "list")
  attr(community, "count") <- count
  community
}

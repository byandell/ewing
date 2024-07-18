#' Initialize Organism Population
#' 
#' Create data structure with n organisms distributed across life stages.
#' Includes leftist tree structure
#' 
#' Does various initializations based in information in \code{future.xxx},
#' where \code{xxx} is replaced by the name of the dataset. Normally,
#' \code{init.population} is called by \code{init.simulation}.
#' 
#' @param community community structure
#' @param species species name(s) as character string vector
#' @param n number of organisms in population
#' @param width maximum width of dispersal on tridiagonal coordinate system
#' @param units time units (default is
#' \code{getOrgFeature(community,species,"units")})
#' @param timeit print timing information if \code{TRUE}
#' @param reject rejection time (default is \code{Inf})
#' @param position initial positions in triangular coordinate system (generated
#' randomly)
#' @param colnames names of columns in data structure (do not change)
#' @param init.stage initial stage of all organisms (default values generated
#' randomly)
#' @param init.weight initial weights for life stages (default taken for
#' organism features)
#' @param ... not used
#' @return \item{comp1 }{Description of `comp1'} \item{comp2 }{Description of
#' `comp2'} ...
#' @author Brian S. Yandell
#' @seealso \code{\link{init.simulation}}
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{init.population( redscale )}
#' 
#' 
#' @export
#' @importFrom stats rexp
init.population <- function( community, species, n = 200, width = 100,
                             units = getOrgFeature( community, species, "units" ),
                             timeit = FALSE,
                             reject = Inf,
                             position = rtri( n, width ),
                             colnames = c(leftistnames,paramnames,posnames,eventnames),
                             init.stage = istage,
                             init.weight = getOrgFuture( community, species, "init" ),
                             messages = TRUE,
                             ...)
{
  leftistnames <- c("dist","left","right","up")
  paramnames <- c("dispersion","location","intensity","truncation","rejection")
  posnames <- paste("pos",letters[1:3], sep = ".")
  eventnames <- c("time","stage","future","offspring","sex","sub.stage","sub.future")
  
  organism <- matrix( 0, length( colnames ), n+1,
                      dimnames = list( colnames, NULL ))
  organism["time",1] <- Inf
  
  ## 5-parameter initialization
  organism[c("dispersion","intensity"),-1] <- 1
  organism["rejection",-1] <- if( reject == Inf )
    rep( Inf, n )
  else
    reject * stats::rexp( n )
  
  ## triangular coordinates
  organism[posnames,-1] <- position
  
  ## substrate
  substrate.name <- getOrgFeature( community, species, "substrate" )
  if( !is.na( substrate.name )) {
    substrate <- getOrgInteract( community, substrate.name, species, "init" )
    organism["sub.stage",-1] <- organism["sub.future",-1] <- sample( length( substrate ),
                                                                     n, replace = TRUE, prob = substrate / sum( substrate ))
  }
  ## randomly generate events proportional to future time units
  nstage <- length( init.weight )
  istage <- sample( nstage, n, replace = TRUE, prob = init.weight / sum( init.weight ))
  init.stage <- array( init.stage, n )
  organism["stage",-1] <- init.stage
  
  ## schedule future events
  if( timeit )
    proctime <- proc.time()
  organism[,-1] <- get.future( community, species, organism[,-1] )
  if( timeit ) {
    tmp <- proc.time() - proctime
    cat( "future time: user=", tmp[1], "system=", tmp[2], "total=", tmp[3], "\n" )
  }
  ## create leftist tree
  if( timeit )
    proctime <- proc.time()
  community <- put.species( community, species, leftist.create( organism ))
  if( timeit ) {
    tmp <- proc.time() - proctime
    cat( "leftist time: user=", tmp[1], "system=", tmp[2], "total=", tmp[3], "\n" )
  }
  ## mean number of offspring
  if( timeit )
    proctime <- proc.time()
  #***This is where parasite is crashing--no offspring?**
  community <- initOffspring( community, species )
  if( timeit ) {
    tmp <- proc.time() - proctime
    cat( "offspring time: user=", tmp[1], "system=", tmp[2], "total=", tmp[3], "\n" )
  }
  
  if(messages) {
    cat( "Initializing events for", species, "with", ncol( organism ) - 1, "individuals\n" )
  }
  community
}

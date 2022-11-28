## $Id: init.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
##
## Functions for Bland Ewing's modeling.
##
##     Copyright (C) 2000,2001,2002 Brian S. Yandell.
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
###########################################################################################
##
## init.simulation( community )
##
## also includes routines to get objects from Organism files
###########################################################################################


#' Initialize simulation run
#' 
#' Initializes simulation using organism features.
#' Default data are used unless user provides global data
#' or uses the hidden argument `datafile` to specify a folder with user data.
#' 
#' @param package package where community data features can be found
#' @param count number of individuals per species (single number or count in order of `species`)
#' @param interact ask for species count if `TRUE` and interactive
#' @param messages show messages if `TRUE` (default)
#' @param ... additional arguments for `init.population`
#' 
#' @return Object with elements for each species that are created by init.population.
#' 
#' @author Brian S. Yandell
#' @seealso \code{\link{init.population}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{init.simulation(myrun)}
#' 
#' @export
#' @importFrom stats rexp rpois
#' @importFrom utils data
#' @importFrom tools file_ext
#' @importFrom readxl read_xls read_xlsx
#' 
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
  class( community ) <- "ewing"
  attr(community, "count") <- count
  community
}
##########################################################################################


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
#'
#' @return \item{comp1 }{Description of `comp1'} \item{comp2 }{Description of
#' `comp2'} ...
#' @author Brian S. Yandell
#' @seealso \code{\link{init.simulation}}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{init.population( redscale )}
#' 
#' @export
#' 
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
##########################################################################################
## Offspring Information
##########################################################################################
getOffspring <- function( community, species,
                         offspring = getOrgFeature( community, species, "offspring" ))
{
  if( is.na( offspring ))
    return( 0 )
  if( is.numeric( offspring ))
    return( offspring )
  getOrgInteract( community, offspring, species, "offspring" )
}
##########################################################################################
initOffspring <- function( community, species )
{
  hostname <- getOrgFeature( community, species, "offspring" )

  ## find if there is offspring load based on host
  orgoffspring <- getOffspring( community, species, hostname )

  norganism <- sum( getOrgAlive( community, species ))

  if( length( orgoffspring ) == 1 ) {
    ## mean offspring does not depend on any host
    offspring <- stats::rpois( norganism, orgoffspring )
  }
  else {
    orgoffspring <- orgoffspring[ orgoffspring > 0 ]
    
    if(!length(orgoffspring))
      return(community)
  
    ## figure out initial offspring load based on host distribution

    ## mean offspring depends on host stages and events
    host <- get.species( community, hostname )
    if( is.null( host ))
      stop( paste( "Host", hostname, "not initiated yet" ))

    ## get weights of host stages in terms of future event times
    host <- host[ , getOrgAlive( community, hostname ) ]

    ## find host stages that are preferred by parasite
    ## need to take subset of current that are actually in host
    hoststages <- match( names( orgoffspring ), getOrgFuture( community, hostname )$current,
                        nomatch = 0 )
    host <- as.matrix( host[ , !is.na( match( host["stage",], hoststages )) ] )
    if( ncol( host ) == 0 )
      return( community )

    tmp <- !is.na( match( hoststages, host["stage",] ))
    hoststages <- hoststages[tmp]
    orgoffspring <- orgoffspring[tmp]

    if(!length(orgoffspring))
      return(community)
    
    dd <- tapply( host["time",], host["stage",], sum )
    dd[ as.character( hoststages[
      is.na( match( hoststages, names( dd ))) ] ) ] <- 0
    dd[ is.na( dd ) ] <- 0
    sdd <- sum( dd )
    if( length( dd ) > 1 & sdd > 0)
      offspring <- as.vector( sample( orgoffspring, norganism, replace = TRUE,
        prob = dd / sdd ))
    else
      offspring <- rep( ( sdd > 0 ) * orgoffspring[1], norganism )
    offspring[ is.na( offspring ) ] <- 0
  }
  organism <- get.species( community, species )
  organism["offspring",-1] <- offspring
  put.species( community, species, organism )
}
###############################################################################
get.offspring <- function( community, species )
{
  individual <- get.individual( community, species )
  if( individual["offspring"] > 0 )
    1
  else
    0
}
###########################################################################################
set.offspring <- function( community, species, host, dead )
{
  stage <- get.species.element( community, host, "stage", dead )
  current <- getOrgFuture( community, host, "current", stage )
  offspring <- getOrgInteract( community, host, species, "offspring")
  offspring <- as.vector( offspring[ as.character( current ) ] )
  offspring[ is.na( offspring ) ] <- 0
  offspring
}
###########################################################################################
### Organism Features
###########################################################################################
getOrgFeature <- function( community, species, feature = names( OrgFeature ))
{
  OrgFeature <- getOrgInfo( community, "Feature" )
  ## The global Organism$Feature contains static features of all organisms.
  ## Return the feature as numeric if possible, or as character.
  ## Missing value (NA) indicates no such feature is possible for that organism.
  if( missing( species ))
    return( row.names( OrgFeature ))
  f <- OrgFeature[ species, feature ]
  if( length( feature ) == 1 ) {
    if( any( is.na( f )))
      return( NA )
    f <- as.character( f )
  }
  else {
    f <- apply( f, 2, as.character )
  }
  f <- c( unlist( f ))
  ## try to interpret f as numeric
  opwarn <- options( warn = -1 )
  numf <- suppressWarnings(as.numeric( f ))
  options( opwarn )
  if( all( !is.na( numf )))
    f <- numf
  f
}
##########################################################################################
getOrgHosts <- function( community, species,
                        feature = c("offspring","attack","substrate") ###HOST SPECIFIC###
                        )
{
  f <- unique( getOrgFeature( community, species, feature ))
  o <- getOrgFeature( community )
  o[ match( f, o, nomatch = 0 ) ]
}
###########################################################################################
getOrgFuture <- function( community, species, feature, current,
  future = OrgFuture[[species]] )
{
  OrgFuture <- getOrgInfo( community, "Future" )
  ## The global Organism$Future[[species]] contains future event information.
  ## Return the information as numeric if possible, or as character.
  ## Missing value (NA) indicates no such feature is possible for that organism.
  if( missing( current )) {
    if( missing( feature ))
      return( future )
    future <- future[,feature]
  }
  else {
    if( !is.numeric( current ))
      current <- match( current, future$current, nomatch = 0 )
    if( missing( feature ))
      future <- future[ current, ]
    else
      future <- future[ current, feature ]
  }
  if( is.null( future ))
    return( NA )
  if(is.character( future ))
    future <- as.factor(future)
  future
}
###########################################################################################
get.interact <- function( community, species, host, avail, event )
{
  id <- get.species.element( community, host, "stage", avail )
  interact <- getOrgInteract( community, host, species, event )[id]
  interact[ is.na( interact ) ] <- 0
  interact
}
###########################################################################################
getOrgInteract <- function( community,
                           org1name = getOrgFeature( community, org2name, "substrate" ),
                           org2name, event = NULL )
{
  OrgInteract <- getOrgInfo( community, "Interact" )
  ## The global org$Interact[[org1name]][[org2name]] contains interaction information.
  tmp <- OrgInteract[[org1name]][[org2name]]
  if( is.null( event ))
    return( tmp )
  event <- as.character( event )
  inter <- tmp[,event]
  if( length( event ) == 1 )
    names( inter ) <- row.names( tmp )
  if(is.character(inter))
    inter <- factor(inter)
  inter
}
###########################################################################################
getOrgMeanValue <- function( community, species )
{
  OrgMeanValue <- getOrgInfo( community, "MeanValue" )
  ## The global org$MeanValue[[species]] contains mean value information.
  OrgMeanValue[[species]]
}
###########################################################################################
copyOrgInfo <- function( fromname, toname )
{
  for( i in c("sim")) {
    from <- paste( fromname, i, sep = "." )
    if( exists( from )) {
      toto <- paste( toname, i, sep = "." )
      assign( toto, get( from ), ".GlobalEnv" )
      cat( "copied", from, "to", toto, "\n" )
    }
  }
  invisible()
}
###########################################################################################
get.alive <- function( community, species, substrate )
{
  alive <- getOrgAlive( community, species )
  alive <- seq( length( alive ))[alive]
  alive[ substrate == get.species.element( community, species, "sub.stage", alive ) ]
}
###########################################################################################
getOrgAlive <- function( community, species, element )
{
  organism <- get.species( community, species )
  ## identify dead organisms (free nodes for leftist tree)
  tmp <- c( FALSE, apply( organism[c("dist","up","left","right"),-1], 2,
    function( x ) any( x > 1 )))
  if( !any( tmp )) {
    tmpp <- organism["up",1]
    if( tmpp > 1 )
      tmp[tmpp] <- TRUE
  }
  if( !missing( element ))
    tmp <- organism[ element, tmp ]
  tmp
}
###########################################################################################
getOrgAgeClass <- function( community, species, stage = seq( nrow( future )),
  future = getOrgFuture( community, species ))
{
  ageclass <- future$ageclass[stage]
  tmp <- !is.na( ageclass )
  if( any( tmp ))
    ageclass[ !is.na( ageclass ) ]
  else
    NA
}
###########################################################################################
getOrgSubstrate <- function( community, species, elements = seq( nrow( inter )),
  substrate = getOrgFeature( community, species, "substrate" ),
  inter = getOrgInteract( community, substrate, species ))
{
  sites <- inter$substrate[elements]
  tmp <- !is.na( sites )
  if( any( tmp ))
    sites[ !is.na( sites ) ]
  else
    NA
}
###########################################################################################
sampleOrgSubstrate <- function( community, species, elements = seq( nrow( inter )),
  substrate.name = getOrgFeature( community, species, "substrate" ),
  inter = getOrgInteract( community, substrate.name, species ))
{
  if( is.na( substrate.name ))
    return( elements )
  newsub <- as.matrix( cbind( elements, inter[ elements, levels( factor(inter$substrate) ) ] ))
  apply( newsub, 1, function( x, is ) {
    ns <- sample( levels( factor(is) ), 1, prob = x[-1] / sum( x[-1] ))
    sub <- seq( nrow( inter ))[ ns == is ]
    if( length( sub ) > 1 ) {
      newsub <- getOrgInteract( community, substrate.name, substrate.name )[x[1],sub]
      sample( sub, 1, prob = newsub / sum( newsub ))
    }
    else
      sub
  }, inter$substrate )
} 
###########################################################################################
## System files
###########################################################################################
my.read <- function(dataname, stringsAsFactors = TRUE)
{
  switch(tools::file_ext(dataname),
         "txt" =, "tsv" = read.table(dataname, stringsAsFactors = stringsAsFactors),
         "csv" = read.csv(dataname, stringsAsFactors = stringsAsFactors),
         "xls" = readxl::read_xls(dataname),
         "xlsx" = readxl::read_xlsx(dataname))
}
###########################################################################################
my.eval <- function(species, extension, element, checkdata = FALSE )
{
  if( !missing( extension ))
    species <- paste( species, extension, sep = ".")
  if( exists( species )) 
    organism <- get( species )
  else {
    if( checkdata ) {
      organism <- utils::data( list = species )
      if( organism == species )
        organism <- NULL
    }
    else
      organism <- NULL
  }
  if(!( missing(element) | is.null( organism )))
    organism <- organism[[element]]
  organism
}
###########################################################################################
mydata <- function( dataname, package, restart = FALSE, messages = TRUE )
{
  edata <- exists( dataname )
  if( restart & edata ) {
    remove( list = dataname, pos = 1 )
    edata <- !edata
  }
  if( !edata ) {
    utils::data( list = dataname, package = eval( package ))
    if(messages) {
      cat( "Data", dataname, "loaded\n" )
    }
  }
  else
    if(messages) {
      cat( "Data", dataname, "already loaded\n" )
    }
} 

###########################################################################################
### Simulation count object administration
###########################################################################################
initCount <- function( community, species, debugit = FALSE, file = NULL, append = FALSE,
                       messages = TRUE, ... )
{
  if(messages) {
    cat( "initial" )
    for( i in species)
      cat( ":", i, sum( apply( get.species( community, i ), 2,
                               function(x) !all(x[c("dist","left","right","up")]==1))) - 1 )
    cat( "\n" )
  }
  
  count <- list()
  ## leftist tree counters
  count$mintime <- numeric( length( species ))
  count$base <- numeric( length( species ))
  names( count$base ) <- names( count$mintime ) <- species
  count$free <- list()
  
  ## initialize lists to keep track of events
  count$events <- count$countage <- count$countsub <- count$nameage <- count$namesub <- list()
  
  ## Set up hour to degreeday spline based on range of hours if any
  simmin <- c(hr=Inf,DD=Inf)
  for( i in species ) {
    if( is.null( get.species( community, i )))
      stop( paste( "Missing species", i ))
    count$free[[i]] <- 1
    count$base[i] <- get.base( community, i )
    units <- getOrgFeature( community, i, "units" )
    tmp <- get.individual( community, i )["time"]
    if( tmp < simmin[units] )
      simmin[units]<- tmp
  }
  if( max( simmin ) < Inf ) {
    community <- activeTemp( community, simmin["hr"], , simmin["DD"], messages = messages )
  }
  esums <- c("initial","during","final")
  tmpfn <- function( counter )
  {
    rownames <- levels( counter )
    array( 0, length( rownames ), dimnames = list( rownames ))
  }
  subclass <- getOrgFeature( community, species, "subclass" )
  names( subclass ) <- species
  for( i in species ) {
    species.time <- get.individual( community, i )["time"]
    count$mintime[i] <- getTime( community, i, species.time )
    future <- getOrgFuture( community, i )
    ## possible future events
    count$events[[i]] <- matrix( 0, nrow( future ), length( esums ),
                                 dimnames = list( as.character( future$current ), esums ))
    ## current record of future events
    count$countage[[i]] <- tmpfn( getOrgFuture( community, i, "ageclass" ))
    count$countsub[[i]] <- tmpfn( getOrgInteract( community,, i, "substrate" ))
    
    if( species.time < Inf ) {
      ## count by age groups
      stage <- getOrgAlive( community, i, "stage" )
      if( length( stage )) {
        classes <- getOrgAgeClass( community, i, stage )
        tmp <- table( classes )
        count$countage[[i]][ names( tmp ) ] <- tmp
      }
      ## count by substrate
      substage <- getOrgAlive( community, i, "sub.stage" )
      ## only for individuals of class = subclass[i]
      substage <- substage[ subclass[i] == getOrgAgeClass( community, i, stage ) ]
      if( length( substage )) {
        classes <- getOrgInteract( community,, i, "substrate" )
        tmp <- table( classes[substage] )
        count$countsub[[i]][ names( tmp ) ] <- tmp
      }
    }
  }
  count$debug <- debugit
  
  # If file is NULL, then don't write to file; keep counts internal
  count$file <- file
  
  community$count <- count
  
  ## Put counts in file
  community <- putCount( community, append )
  
  ## tally events at start of simulation
  setEvents( community, "initial" )
}
###########################################################################################
getCount <- function( community, species, element )
{
  count <- community$count[[element]]
  if( !missing( species ))
    count <- count[[species]]
  count
}
###########################################################################################
set.step <- function( community, step )
  setCount( community,, list( step = step ))
###########################################################################################
setCount <- function( community, species, elements )
{
  count <- community$count
  for( i in names( elements )) {
    if( missing( species ))
      count[[i]] <- elements[[i]]
    else
      count[[i]][[species]] <- elements[[i]]
  }
  community$count <- count
  community
}

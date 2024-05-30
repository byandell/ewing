###########################################################################################
updateEvents <- function( community, species, event, increment = 1 )
{
  community$count$events[[species]][event,"during"] <-
    community$count$events[[species]][event,"during"] + increment
  community
}
###########################################################################################
setEvents <- function( community, period )
{
  count <- community$count
  for( species in get.species( community )) {
    current <- getOrgFuture( community, species, "current" )
    events <- rep( 0, length( current ))
    names( events ) <- as.character( current )
    
    stage <- getOrgAlive( community, species, "stage" )
    if( !length( stage ))
      return( events )
    
    tmp <- tapply( stage, current[stage], length )
    tmp[ is.na( tmp ) ] <- 0
    events[ names( tmp ) ] <- tmp
    
    count$events[[species]][,period] <- events
  }
  community$count <- count
  community
}

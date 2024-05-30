###########################################################################################
### Timing of simulation run
###########################################################################################
init.timing <- function( community )
{
  ## initialize timing
  events <- NULL
  for( species in get.species( community ))
    events <- c( events, levels( getOrgFuture( community, species, "event" )))
  events <- sort( unique( events ))
  tmp <- c("total",events,"refresh","other")
  cpu <- matrix( 0, 3, length( tmp ),
                 dimnames = list( c("user","system","total"), tmp ))
  community$cpu <- cpu
  community <- set.timing( community, "total" )
  community
}
###########################################################################################
set.timing <- function( community, string, flag = -1 ) {
  if( !is.null( community$cpu ))
    community$cpu[,string] <- community$cpu[,string] + flag * proc.time()[1:3]
  community
}
###########################################################################################
fini.timing <- function( community )
{
  if( !is.null( community$cpu )) {
    community <- set.timing( community, "total", 1 )
    community$cpu[,"other"] <- community$cpu[,"total"] - apply( community$cpu[,-1], 1, sum )
  }
  community
}

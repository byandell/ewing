## $Id: community.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
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
get.species <- function( community, species ) {
  if( missing( species ))
    names( community$pop )
  else
    community$pop[[species]]
}
###########################################################################################
get.species.element <- function( community, species, rows, cols )
  community$pop[[species]][rows,cols]
###########################################################################################
put.species <- function( community, species, value )
{
  community$pop[[species]] <- value
  community
}
###############################################################################
put.individual <- function( community, species, individual,
                           id = get.base( community, species ))
{
  community$pop[[species]][,id] <- individual
  community
}  
###############################################################################
get.individual <- function( community, species,
                           id = get.base( community, species ))
  community$pop[[species]][,id]
###############################################################################
get.base <- function( community, species )
  community$pop[[species]]["up",1]
##########################################################################################
### simulation organism administration
##########################################################################################
initOrgInfo <- function( package )
{
  community <- list( pop = list( ))
  community$org <- list( )
  community$org$package <- package
  ## Get data
  mydata( "organism.features", package, restart = TRUE )
  community$org$Feature <- organism.features
  rm( list = "organism.features", pos = ".GlobalEnv" )
  community$pop <- list()
  community
}
##########################################################################################
setOrgInfo <- function( community, species, hosts, package )
{
  Organism <- community$org

  if( is.null( Organism$Future ))
    Organism$Future <- list( )
  if( is.null( Organism$Interact ))
    Organism$Interact <- list( )

  for( j in hosts )
    if( is.null( Organism$Interact[[j]] ))
      Organism$Interact[[j]] <- list( )
  ## Do not reset MeanValue as it may have important spline fits!
  if( is.null( Organism$MeanValue ))
    Organism$MeanValue <- list( )

  for( i in species ) {
    tmp <- paste( "future", i, sep = "." )
    mydata( tmp, getOrgInfo( community, "package" ), restart = TRUE )
    future <- my.eval( tmp )
    rm( list = tmp, pos = ".GlobalEnv" )
    level.ageclass <- unique( future$ageclass )
    level.ageclass <- as.character( level.ageclass[ !is.na( level.ageclass ) ] )
    future$ageclass <- ordered( future$ageclass, level.ageclass )
    Organism$Future[[i]] <- future
    for( j in hosts )
      if( i != j ) {
        tmp <- paste( j, i, sep = "." )
        mydata( tmp, getOrgInfo( community, "package" ), restart = TRUE )
        Organism$Interact[[j]][[i]] <- my.eval( tmp )
        rm( list = tmp, pos = ".GlobalEnv" )
      }
    if( is.null( Organism$MeanValue[[i]] ))
      Organism$MeanValue[[i]] <- list( )
    else
      cat( "Keeping Mean Value information for", i, "if any\n" )
  }
  for( i in unique( getOrgFeature( community, species, "substrate" ))) {
    substrate <- paste( i, i, sep = "." )
    mydata( substrate, getOrgInfo( community, "package" ), restart = TRUE )
    Organism$Interact[[i]][[i]] <- my.eval( substrate )
    rm( list = substrate, pos = ".GlobalEnv" )
  }
  community$org <- Organism
  community
}
###########################################################################################
getOrgInfo <- function( community, element )
{
  community$org[[element]]
}
###########################################################################################
setOrgMeanValue <- function( community, species, stage, mvalue )
{
  ## The global Organism$MeanValue[[species]] contains mean value information.
  community$org$MeanValue[[species]][[stage]] <- mvalue
  community
}
###########################################################################################
### Simulation count object administration
###########################################################################################
initCount <- function( community, species, file, append = FALSE, debugit = FALSE )
{
  cat( "initial" )
  for( i in species)
    cat( ":", i, sum( apply( get.species( community, i ), 2,
                            function(x) !all(x[c("dist","left","right","up")]==1))) - 1 )
  cat( "\n" )

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
    require( splines )
    community <- activeTemp( community, simmin["hr"], , simmin["DD"] )
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
  count$file <- file
  if( !( file.exists( count$file ) & append ))
    cat( "species step time future",
        paste( "count", seq( max( unlist( lapply( count$countage, length )) +
                                 unlist( lapply( count$countsub, length )))),
              sep = "" ), "\n", file = count$file )
  for( i in species ) {
    cat( i, 0, get.species.element( community, i, c("time","stage"), count$base[i] ),
        count$countage[[i]], count$countsub[[i]], "\n",
        file = count$file, append = TRUE )
  }
  community$count <- count
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
##########################################################################################
### simulation temperature administration
##########################################################################################
initTemp <- function( community, lo.hour = 0, hi.hour = getTemp( community, "Unit" ),
                     days = TemperaturePar["Days"] )
{
  cat( "Initializing Temperature Profile ...\n" )
  require( splines )
  Temperature <- list()
  
  data( TemperaturePar )
  TemperaturePar <- array( TemperaturePar[,"value"],
                          dimnames = list( row.names( TemperaturePar )))
  Temperature$Unit <- TemperaturePar["Unit"]
  Temperature$Min <- TemperaturePar["Min"]

  ## set up daily temperature base
  data( TemperatureBase )
  Temperature$Time <- split( TemperatureBase$Time, TemperatureBase$Day )
  Temperature$Base <- split( TemperatureBase$Base, TemperatureBase$Day )
  remove( TemperatureBase, pos = 1 )

  community$temp <- Temperature
    
  tmp <- seq( lo.hour / Temperature$Unit,
             days + 1 + ( hi.hour / Temperature$Unit ),
             length = TemperaturePar["Length"] )
  tmp0 <- seq( 0, 1, length = TemperaturePar["Length"] )
  tmp1 <-  0.25 * ( TemperaturePar["HighBeg"] - TemperaturePar["LowBeg"] )
  Temperature$Low <- interpSpline( tmp, TemperaturePar["LowBeg"] * ( 1 - tmp0 ) +
                                  TemperaturePar["LowEnd"] * tmp0 +
                                  sin( pi * 4 * tmp0 ) * tmp1 )
  Temperature$High <- interpSpline( tmp, TemperaturePar["HighBeg"] * ( 1 - tmp0 ) +
                                   TemperaturePar["HighEnd"] * tmp0 +
                                   sin( pi * ( 0.125 + 4 * tmp0 )) * tmp1 )
  remove( TemperaturePar, pos = 1 )

  Temperature$DegreeDay <- NULL

  cat( "Base daily temperature fluctuation:\n" )
  for( i in names( Temperature$time )) {
    cat( "From day", i, ":\n" )
    tmp <- Temperature$Base[[i]]
    names( tmp ) <- Temperature$Time[[i]]
    print( tmp )
  }
  community$temp <- Temperature
  
  showTemp( community )
  cat( "Initial active temperature:\n" )
  activeTemp( community, lo.hour, hi.hour, getTemp( community, "Time", 1 )[1] )
}
###########################################################################################
getTemp <- function( community, element, sub )
{
  tempelem <- community$temp[[element]]
  if( !missing( sub ))
    tempelem <- tempelem[[sub]]
  tempelem
}
###########################################################################################
setTemp <- function( community, element, value )
{
  community$temp[[element]] <- value
  community
}
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
###########################################################################################
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

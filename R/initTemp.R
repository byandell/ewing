##########################################################################################
### simulation temperature administration
##########################################################################################
#' temperature data tables for simulation
#' 
#' Used internally by \code{init.simulation} to initialize temperature regime
#' for simulation.
#' 
#' 
#' @aliases TemperatureBase TemperaturePar
#' @format \code{TemperatureBase} is a data frame with one or more daily
#' baseline temperatures. \describe{ \item{Day}{day on which baseline begins}
#' \item{Time}{time in hours from midnight} \item{Base}{temperature offset in
#' percent between daily lows and highs} \code{TemperaturePar} is a data frame
#' whow rows are: \item{value}{value of parameter}
#' \item{description}{description of parameter (same as column descriptions
#' below)} } \describe{ The columns of \code{TemperaturePar} are:
#' \item{Unit}{hours in day} \item{Days}{number of days} \item{Min}{minimum
#' temperature for degree-day calculation (Fahrenheit)} \item{LowBeg}{begining
#' daily low temperature} \item{LowEnd}{ending daily low temperature}
#' \item{HighBeg}{begining daily high temperature} \item{HighEnd}{ending daily
#' high temperature} \item{Length}{number of nodes for interpolating spline} }
#' @seealso \code{\link{init.simulation}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords datasets
#' @examples
#' 
#' 
#' data(TemperatureBase)
#' data(TemperaturePar)
#' 
#' @importFrom splines interpSpline
initTemp <- function( community, lo.hour = 0, hi.hour = getTemp( community, "Unit" ),
                      days = TemperaturePar["Days"], 
                      messages = TRUE, datafile = "", ... )
{
  if(messages) {
    cat( "Initializing Temperature Profile ...\n" )
  }
  Temperature <- list()
  
  TemperaturePar <- getOrgData(community, "temperature", "par", messages, datafile)
  #  mydata( "TemperaturePar", getOrgInfo( community, "package" ), messages = messages)
  TemperaturePar <- array( TemperaturePar[,"value"],
                           dimnames = list( row.names( TemperaturePar )))
  Temperature$Unit <- TemperaturePar["Unit"]
  Temperature$Min <- TemperaturePar["Min"]
  
  ## set up daily temperature base
  
  TemperatureBase <- getOrgData(community, "temperature", "base", messages, datafile)
  #  mydata( "TemperatureBase", getOrgInfo( community, "package" ), messages = messages)
  Temperature$Time <- split( TemperatureBase$Time, TemperatureBase$Day )
  Temperature$Base <- split( TemperatureBase$Base, TemperatureBase$Day )
  
  community$temp <- Temperature
  
  tmp <- seq( lo.hour / Temperature$Unit,
              days + 1 + ( hi.hour / Temperature$Unit ),
              length = TemperaturePar["Length"] )
  tmp0 <- seq( 0, 1, length = TemperaturePar["Length"] )
  tmp1 <-  0.25 * ( TemperaturePar["HighBeg"] - TemperaturePar["LowBeg"] )
  Temperature$Low <- splines::interpSpline( tmp, TemperaturePar["LowBeg"] * ( 1 - tmp0 ) +
                                              TemperaturePar["LowEnd"] * tmp0 +
                                              sin( pi * 4 * tmp0 ) * tmp1 )
  Temperature$High <- splines::interpSpline( tmp, TemperaturePar["HighBeg"] * ( 1 - tmp0 ) +
                                               TemperaturePar["HighEnd"] * tmp0 +
                                               sin( pi * ( 0.125 + 4 * tmp0 )) * tmp1 )
  
  Temperature$DegreeDay <- NULL
  
  if(messages) {
    cat( "Base daily temperature fluctuation:\n" )
  }
  for( i in names( Temperature$time )) {
    cat( "From day", i, ":\n" )
    tmp <- Temperature$Base[[i]]
    names( tmp ) <- Temperature$Time[[i]]
    print( tmp )
  }
  community$temp <- Temperature
  
  if(messages) {
    showTemp( community )
    cat( "Initial active temperature:\n" )
  }
  activeTemp( community, lo.hour, hi.hour, getTemp( community, "Time", 1 )[1],
              messages = messages)
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

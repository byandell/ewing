#' temperature data tables for simulation
#' 
#' Used internally by \code{init.simulation} to initialize temperature regime
#' for simulation.
#' 
#' @name TemperatureBase
#' @aliases TemperatureBase TemperaturePar
#' @format \code{TemperatureBase} is a data frame with one or more daily
#' baseline temperatures.
#' \describe{
#'  \item{Day}{day on which baseline begins}
#'  \item{Time}{time in hours from midnight}
#'  \item{Base}{temperature offset in percent between daily lows and highs} \code{TemperaturePar} is a data frame whow rows are: \item{value}{value of parameter}
#'  \item{description}{description of parameter (same as column descriptions below)}
#' }
#' \describe{ The columns of \code{TemperaturePar} are:
#'  \item{Unit}{hours in day}
#'  \item{Days}{number of days}
#'  \item{Min}{minimum temperature for degree-day calculation (Fahrenheit)}
#'  \item{LowBeg}{begining daily low temperature}
#'  \item{LowEnd}{ending daily low temperature}
#'  \item{HighBeg}{begining daily high temperature}
#'  \item{HighEnd}{ending daily high temperature}
#'  \item{Length}{number of nodes for interpolating spline}
#' }
#' @seealso \code{\link{init.simulation}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords datasets
#' @examples
#' 
#' data(TemperatureBase)
#' data(TemperaturePar)
#' 
NULL

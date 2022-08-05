#' future events table for species simulation
#' 
#' Contains life history events for QPE simulation. These are normally
#' internalized via \code{init.simulation}.
#' 
#' 
#' @name future.host
#' @aliases future.host future.parasite
#' @format Data frame with one row per stage, except in the case of competing
#' risks. Time is in appropriate units for species as set in
#' \code{organism.features}.
#' \describe{
#'  \item{current}{current stage}
#'  \item{future}{potential future event}
#'  \item{fid}{future event ID, corresponds to row when this is current stage}
#'  \item{time}{mean time to future event}
#'  \item{pch}{plot character}
#'  \item{color}{plot color}
#'  \item{ageclass}{class of stage (see \code{organism.features}}
#'  \item{event}{event class}
#'  \item{init}{current stage weighting for initialization}
#' }
#' @seealso \code{\link{init.simulation}},\code{\link{future.events}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @source Lisa D. Forster and Robert F. Luck, Entomology, UC Riverside.
#' @keywords datasets
#' @examples
#' 
#' data(future.host)
#' data(future.parasite)
#' 
NULL

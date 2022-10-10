#' Simulated Data with Multiple Runs
#' 
#' Result of `ewing_discrete` with 100 runs of 
#' 
#' @name simdata
#' @format Object of class `ewing_discrete`.
#' Contains `nsim` runs each of `nstep` steps.
#' Attributes include the following
#' \describe{
#'  \item{nsim}{number of simulations}
#'  \item{nstep}{number of steps per simulation}
#'  \item{count}{vector with initial population count by species}
#'  \item{species}{bector of species names}
#'  \item{items}{list by species of age classes (used in summary and plot methods)}
#'  \item{ordinate}{ordinate for simulation sequesnce (usually `time`)}
#' }
#' @seealso \code{\link{ewing_discrete}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords datasets
#' @examples
#' 
#' data(simdata)
#' 
NULL

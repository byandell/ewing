#' host-parasite interaction table for simulation
#' 
#' Parasites select hosts for ovipositing (egg-laying) or feeding based on
#' their stage of development. The risk of male offspring depends on stage, as
#' does the initial offspring load for the successfully reared parasite. These
#' tables are internalized via \code{init.simulation}.
#' 
#' @name host.parasite
#' @aliases host.parasite host.comperiella host.encarsia
#' @format \describe{
#'  \item{ovip}{risk of life stage being selected for oviposition (egg-laying)}
#'  \item{feed}{risk of life stage being selected for feeding}
#'  \item{offspring}{Poisson mean number of offspring load for new parasite reared in host of this stage}
#' \item{male}{risk of parasite laying a male egg in this host stage}
#' }
#' @seealso \code{\link{init.simulation}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords datasets
#' @examples
#' 
#' data(host.parasite)
#' data(host.comperiella)
#' data(host.encarsia)
#' 
NULL

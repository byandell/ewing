#' organism features master table
#' 
#' Identifies organisms and their key features for Ewing's Quantitative
#' Population Ethology simulation. 
#' Each row corresponds to a `species` to be used in the simulation.
#' Note that `subclass` names must correspond to possible `current` stage
#' for that `species`.
#' 
#' @name organism.features
#' @format Data frame with one row per organism potentially in the study. By
#' default, the first two organisms are selected for simulation initialization
#' in \code{init.simulation}. Missing values indicate this feature is not
#' relevant for this species.
#' 
#' \describe{
#'  \item{units}{time units (DD=degree days, hr=hours)}
#'  \item{offspring}{Poisson mean number of offspring if numeric, or reference to host if character;
#'        see \code{host.parasite} object.}
#'  \item{attack}{character name of host for attack or feeding}
#'  \item{birth}{stage for birth of offspring if host; missing if parasite}
#'  \item{substrate}{substrate on which organism lives}
#'  \item{deplete}{energy depletion rate over time of offspring load in units of number of offspring}
#'  \item{subclass}{ageclass of species to use for substrate movements in \code{future.events}}
#'  \item{parasite}{type of parasite or predator (ecto=ectoparasitoid, endo=endoparasitoid)}
#'  \item{move}{stage when species can move around substrate}
#' }
#' @seealso \code{\link{init.simulation}},\code{\link{future.events}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @source Lisa D. Forster and Robert F. Luck, Entomology, UC Riverside.
#' @keywords datasets
#' @examples
#' 
#' data(organism.features)
#' 
NULL

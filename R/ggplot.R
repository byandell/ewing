#' ggplot of Ewing simulation models
#' 
#' GGPlot of various aspects of simulation.
#' 
#' @param x object of class \code{ewing} with population data by species
#' @param ... other plot parameters
#' @author Brian S. Yandell, \email{yandell@@stat.wisc.edu}
#' @seealso \code{\link{init.simulation}}, \code{\link{future.events}},
#' \code{\link{summary.ewing}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' ggplot_ewing( community )
#' }
#' 
#' @export
#' @importFrom graphics axis lines mtext par
#' @importFrom stats runif
#' @importFrom ggplot2 aes facet_wrap geom_path ggplot
#' @importFrom rlang .data
#' 
ggplot_ewing <- function( x, ... )
{
  ages <- ravel.count( x, ... )
  ggplot2::ggplot(ages) +
    ggplot2::aes(.data$time, .data$Count, col = .data$State) +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(.data$Type ~ .data$Species, scales = "free")
}
###########################################################################################
#' Ravel count from Ewing simulation models
#' 
#' Ravel counts to produce one comprehensive data frame across species
#' 
#' @param community object of class \code{ewing} with population data by species
#' @param substrate include substrate if \code{TRUE}
#' @param total add total column if \code{TRUE}
#' @param normalize normalize counts to 1 if \code{TRUE}
#' @param ... extra parameters not used
#'
#' @author Brian S. Yandell, \email{yandell@@stat.wisc.edu}
#' @seealso \code{\link{init.simulation}}, \code{\link{future.events}},
#' \code{\link{summary.ewing}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' ravel.count( community )
#' }
#' 
#' @export
#' @importFrom dplyr bind_rows filter mutate tibble
#' @importFrom tidyr pivot_longer 
#' 
ravel.count <- function(community, substrate = TRUE, total = TRUE, normalize = TRUE, ...) {
  count <- readCount(community)
  species <- names( count )
  ageclass <- list()
  for( i in species ) {
    ageclass[[i]] <- levels( getOrgFuture( community, i, "ageclass" ))
  }
  substrates <- list()
  for( i in species ) {
    substrates[[i]] <- levels( getOrgInteract( community,, i, "substrate" ))
  }
  out <- list()
  for(i in species) {
    out[[i]] <- mutate(
      tidyr::pivot_longer(
        tibble::tibble(
          as.data.frame(count[[i]])),
        dplyr::any_of(c(ageclass[[i]], substrates[[i]])), 
        names_to = "State",
        values_to = "Count"),
      Type = ifelse(.data$State %in% substrates[[i]], "substrate", "ageclass"))
  }
  out <- dplyr::bind_rows(out, .id = "Species")
  if(!substrate) {
    out <- dplyr::filter(out, .data$Type != "substrate")
  }
  if(total) {
    tot <- dplyr::mutate(
      dplyr::ungroup(
        dplyr::summarize(
          dplyr::group_by(
            out,
            .data$Species, .data$step, .data$time, .data$future, .data$Type),
          Count = sum(.data$Count),
          .groups = "drop")
      ),
      State = "total")
    out <- dplyr::bind_rows(out, tot)      
  }
  if(normalize) {
    out <- dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(
          out,
          .data$Species, .data$State, .data$Type),
        Count = .data$Count / max(.data$Count)))
  }
  out
}

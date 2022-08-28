#' ggplot of Ewing simulation models
#' 
#' GGPlot of various aspects of simulation.
#' 
#' @param x object of class \code{ewing} with population data by species
#' @param normalize normalize counts to 1 if TRUE
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
#' 
ggplot_ewing <- function( x, ... )
{
  ages <- ravel.count( x, ... )
  ggplot(ages) +
    aes(time, Count, col = State) +
    geom_path() +
    facet_wrap(Type ~ Species, scales = "free")
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
#' @importFrom dplyr mutate tibble
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
    out[[i]] <- count[[i]] %>% 
      as.data.frame() %>%
      tibble() %>%
      pivot_longer(any_of(c(ageclass[[i]], substrates[[i]])), names_to = "State", values_to = "Count") %>%
      mutate(Type = ifelse(State %in% substrates[[i]], "substrate", "ageclass"))
  }
  out <- bind_rows(out, .id = "Species")
  if(!substrate) {
    out <- out %>%
      filter(Type != "substrate")
  }
  if(total) {
    tot <- out %>%
      group_by(Species, step, time, future, Type) %>%
      summarize(Count = sum(Count), .groups = "drop") %>%
      ungroup() %>%
      mutate(State = "total")
    out <- bind_rows(out, tot)      
  }
  if(normalize) {
    out <- out %>%
      group_by(Species, State, Type) %>%
      mutate(Count = Count / max(Count)) %>%
      ungroup
  }
  out
}

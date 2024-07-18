#' @method c ewing
c.ewing <- function(...) {
  communities <- list(...)
  if(length(communities) < 2) {
    if(length(communities) == 1)
      return(communities[[1]])
    return(NULL)
  }

  community <- communities[[1]]
  
  # *** need to verify that all communities have save structure
  # Elements org, temp should be identical
  
  for(comi in seq(2, length(communities))) {
    # Element pop
    for(species in names(community$pop)) {
      community$pop[[species]] <-
        cbind(community$pop[[species]], communities[[comi]]$pop[[species]])
    }
    # Element cpu
    community$cpu <- community$cpu + communities[[comi]]$cpu
  }
  # Element plot is more complicated as it contains items for plots
  # Probably want some form of appendX functions
}
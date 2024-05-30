#' Title
#'
#' @param object 
#' @param step 
#' @param ... 
#'
#' @return object of class ewing_snapshot
#' @export
ewing_snapshot <- function(object, step = 0, ...)
{
  out <- list(step = step,
              ageclass = ewing_ageclass(object, ...))

  species <- get.species(object)
  subs <- list()
  for(j in species) {
    subs[[j]] <- ewing_substrate(object, j, step = step, ...)
  }
  out$substrate <- subs
  
  class(out) <- c("ewing_snapshot", "ewing", "list")
  out
}

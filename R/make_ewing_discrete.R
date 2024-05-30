#' Make envelope for Ewing simulations
#' 
#' Wrapper to set up class and attributes of `ewing_envelope` object. 
#' 
#' @param object list of objects from `ewing_discrete`
#' 
#' @export
#' @rdname ewing_discrete
#' 
make_ewing_discrete <- function(object) {  
  nsim <- length(object)
  class(object) <- c("ewing_discrete", class(object))
  
  attr(object, "species") <- species <- names(object[[1]])
  attr(object, "ordinate") <- "time"
  attr(object, "count") <- attr(object[[1]], "count")
  attr(object, "nstep") <- attr(object[[1]], "nstep")
  attr(object, "items") <- attr(object[[1]], "items")
  attr(object, "nsim") <- nsim
  object
}

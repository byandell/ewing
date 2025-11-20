#' Envelope for Ewing simulations
#' 
#' @param nsim number of simulations to run
#' @param verbose show `.` for each simulation if `TRUE`
#' @param ... any additional arguments
#' 
#' @export
#' @importFrom dplyr distinct mutate
#' @importFrom purrr map map_df
ewing_discrete <- function(nsim, verbose = FALSE, ...) {
  sims <- seq_len(nsim)
  
  object <- as.list(sims)
  names(object) <- sims
  
  for(i in sims) {
    if(verbose) cat(".")
    object[[i]] <- ewing_discrete1(...)
  }
  make_ewing_discrete(object)
}
#' Discrete time simulation for Ewing Envelope
#' 
#' Do one simulation and save only by discrete `time` and `increment`
#' 
#' Wrapper to set up class and attributes of `ewing_envelope` object.
#' 
#' 
#' @aliases ewing_discrete1 ewing_discrete make_ewing_discrete
#' summary.ewing_discrete
#' 
#' @param siminit initialize simulation
#' @param increment increment for discrete simulation time
#' @param ... additional parameters
#' @param nsim number of simulations to run
#' @param verbose show `.` for each simulation if `TRUE`
#' @param object object of class `ewing_envelope` or `ewing_envelopes`
#' @export ewing_discrete1
ewing_discrete1 <- function(siminit = init.simulation(interact = FALSE,
                                                    messages = FALSE, ...),
                            increment = 0.5, ...)
{
  # Make sure increment is 1,2,5 x power of 10
  incr <- pretty(increment)
  increment <- incr[which.min(abs(incr - increment))[1]]
  
  out <- future.events(siminit, refresh = 1000,
                       plotit = FALSE, messages = FALSE, ...)
  attrs <- attributes(out)
  
  # Get age classes used later for summaries and plots
  items <- purrr::map(out$org$Future, function(x) levels(factor(x$ageclass)))
  
  out <- readCount(out)
  out <- purrr::map(
    out,
    function(x) {
      purrr::map_df(
        dplyr::distinct(
          purrr::map_df(
            dplyr::mutate(
              as.data.frame(x),
              time = ifelse(.data$step == 0, 0,
                            increment * ceiling(.data$time / increment))),
            rev),
          .data$time, .keep_all = TRUE),
        rev)
    })
  attr(out, "count") <- attrs$count
  attr(out, "nstep") <- attrs$nstep
  attr(out, "items") <- items
  out
}
#' Summary of Ewing Envelope
#' 
#' @param object object of class `ewing_envelope` or `ewing_envelopes`
#' @param ... additional parameters
#' 
#' @export
#' @method summary ewing_discrete
#' @rdname ewing_discrete
summary.ewing_discrete <- function(object, ...) {
  summary(ewing_envelopes(object), ...)
}

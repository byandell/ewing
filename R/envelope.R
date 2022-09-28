#' Discrete time simulation for Ewing Envelope
#' 
#' Do one simulation and save only by discrete `time` and `increment`
#'  
#' @param increment increment for discrete simulation time
#' @param ... any additional arguments
#' 
#' @export
#' @rdname ewing_discrete
#' 
#' @importFrom purrr map map_df
#' @importFrom dplyr distinct mutate
#' @importFrom rlang .data
#' 
ewing_discrete1 <- function(increment = 0.5, ...)
{
  # Make sure increment is 1,2,5 x power of 10
  incr <- pretty(increment)
  increment <- incr[which.min(abs(incr - increment))[1]]
  
  mysim <- init.simulation(interact = FALSE, messages = FALSE)
  out <- readCount(
    future.events(
    mysim, nstep = 1000, refresh = 1000, plotit = FALSE, messages = FALSE))
  purrr::map(
    out,
    function(x) {
      purrr::map_df(
        dplyr::distinct(
          purrr::map_df(
            dplyr::mutate(
              as.data.frame(x),
              time = ifelse(.data$step == 0, 0, increment * ceiling(.data$time / increment))),
            rev),
          .data$time, .keep_all = TRUE),
        rev)
    })
}
#' Envelope for Ewing simulations
#' 
#' @param nsim number of simulations to run
#' @param ... any additional arguments
#' 
#' @export
#' 
ewing_discrete <- function(nsim, ...) {
  sims <- seq_len(nsim)
  
  object <- as.list(sims)
  names(object) <- sims
  
  for(i in sims) {
    object[[i]] <- ewing_discrete1(...)
  }
  make_ewing_discrete(object)
}
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
  ## Hardwired for now!
  class(object) <- c("ewing_envelope", class(object))
  attr(object, "species") <- species <- c("host", "parasite")
  attr(object, "items") <- list(
    host = c("crawler", "host", "gravid"),
    parasite = c("young", "adult"))
  attr(object, "ordinate") <- "time"
  attr(object, "nsim") <- nsim
  object
}

#' Create Envelope of Ewing Simulations
#' 
#' Create envelope object for plotting from multiple runs of Ewing simulation.
#' 
#' @param object multiple simulation object from `envelope_sim`
#' @param species name of species in `object` to build envelope
#' @param item name of item in `species` to build envelope
#' @param ordinate name of ordinate (X axis) to build envelope
#' @param increment increament for discretizing
#' 
#' @export
#' @importFrom tidyr fill pivot_wider
#' @importFrom dplyr arrange bind_rows distinct matches
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom GET create_curve_set
#' 
ewing_envelope <- function(object, species, item, ordinate = "time", increment = 0.5) {
  # Pull out `ordinate` and `item` for each run 
  pulled <-  
    tidyr::fill(
      dplyr::arrange(
        tidyr::pivot_wider(
          dplyr::bind_rows(
            purrr::map(
              object,
              function(x) {
                dplyr::distinct(
                  as.data.frame(x[[species]][,c(ordinate, item)]),
                  .data[[ordinate]],
                  .keep_all = TRUE)
              }),
            .id = "run"),
          names_from = "run",
          values_from = item),
        .data[[ordinate]]),
      -dplyr::matches(ordinate))
  
  object <- GET::create_curve_set(list(r = as.matrix(pulled)[,1], 
                                      obs = as.matrix(pulled[,-1])))
  class(object) <- c("ewing_envelope", class(object))
  attr(object, "species") <- species
  attr(object, "item") <- item
  attr(object, "ordinate") <- ordinate
  object
}

#' GGplot of Ewing multiple envelopes
#' 
#' @param object object of class `ewing_envelope`
#' @param confidence plot confidence bands if `TRUE`
#' @param ... additional parameters
#' 
#' @rdname ggplot_ewing_envelope
#' @export
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom GET fBoxplot
#' @importFrom ggplot2 labs
#' 
ggplot_ewing_envelopes <- function(object, confidence = FALSE, ...) {
  items <- attr(object, "items")
  species <- attr(object, "species")
  ordinate <- attr(object, "ordinate")
  nsim <- attr(object, "nsim")
  
  patch <- list()
  for(specy in species) {
    p <- list()
    for(item in items[[specy]]) {
      env <- ewing_envelope(object, specy, item, ordinate)
      if(confidence) {
        res <- GET::fBoxplot(env, type = 'area')
        p[[item]] <- plot(res) + 
          ggplot2::labs(x = "time", y = item)
        
      } else {
        p[[item]] <- ggplot_ewing_envelope(env)
      }
    }
    ### USE patchwork::wrap_plots here
    patch[[specy]] <- patchwork::wrap_plots(p) + 
      patchwork::plot_layout(nrow = length(p))
  }
  patchwork::wrap_plots(patch)
}

#' GGplot of Ewing envelope
#' 
#' @param object object of class `ewing_envelope`
#' @param cols colors for top simulations
#' @param ... additional parameters
#' 
#' @export
#' @importFrom ggplot2 ggtitle labs
#' @importFrom GET forder
#' 
ggplot_ewing_envelope <- function(object, cols = c("#21908CFF", "#440154FF", "#5DC863FF"), ...) {
  # GET::forder is brittle on class and size of object
  class(object) <- "curve_set"
  
  # Kludge. GET::forder needs at least 3 points; cols can be at most length(object).
  lcols <- length(cols)
  nsim <- ncol(object$funcs)
  if(nsim >= min(3, lcols)) {
    A <- GET::forder(object, measure = 'area')
    lcols <- min(lcols, length(object))
    idx <- order(A)[seq_len(lcols)]
    cols <- cols[seq_len(lcols)]
  } else {
    lcols <- nsim
    idx <- seq_len(lcols)
    cols <- cols[seq_len(lcols)]
  }

  item <- attr(object, "item")
  species <- attr(object, "species")
  ordinate <- attr(object, "ordinate")
  
  if(length(object) >= 50) {
    p <- plot(object, idx = idx, col_idx = cols)
  } else {
    p <- plot(object)
  }
  p + 
    ggplot2::labs(x = ordinate, y = item) +
    ggplot2::ggtitle(paste(species, item))
}
#' GGplot of Ewing Envelope
#' 
#' 
#' @param object object of class `ewing_envelope`
#' @param ... additional arguments 
#' 
#' @export
#' @rdname ggplot_ewing_envelope
#' @importFrom ggplot2 autoplot
#' @method autoplot ewing_envelope
autoplot.ewing_envelope <- function(object, ...) {
  ggplot_ewing_envelope(object, ...)
}

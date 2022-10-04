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
  
  mysim <- init.simulation(interact = FALSE, messages = FALSE, ...)
  out <- future.events(mysim, refresh = 1000, plotit = FALSE, messages = FALSE, ...)
  attrs <- attributes(out)
  
  out <- readCount(out)
  out <- purrr::map(
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
  attr(out, "count") <- attrs$count
  attr(out, "nstep") <- attrs$nstep
  out
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
  out <- make_ewing_discrete(object)
  
  attr(out, "count") <- attr(object[[1]], "count")
  attr(out, "nstep") <- attr(object[[1]], "nstep")
  attr(out, "nsim") <- nsim
  out
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
  nsim <- length(object)
  class(object) <- c("ewing_discrete", class(object))
  
  ## Hardwired for now!
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
#' Ewing Multiple Envelopes
#' 
#' @param object object of class `ewing_discrete`
#' 
#' @rdname ewing_envelope
#' @export
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom GET fBoxplot
#' @importFrom ggplot2 labs
#' 
ewing_envelopes <- function(object) {
  species <- attr(object, "species")
  items <- attr(object, "items")
  ordinate <- attr(object, "ordinate")
  nsim <- attr(object, "nsim")
  confidence <- (nsim > 2)
  
  envs <- as.list(species)
  names(envs) <- species
  if(confidence) {
    confs <- envs
  } else {
    confs <- NULL
  }
  for(specy in species) {
    env1 <- as.list(items[[specy]])
    if(confidence) {
      conf1 <- env1
    }
    for(item in items[[specy]]) {
      env1[[item]] <- ewing_envelope(object, specy, item, ordinate)
      if(confidence) {
        conf1[[item]] <- GET::fBoxplot(env1[[item]], type = 'area')
      }
    }
    envs[[specy]] <- env1
    if(confidence) {
      confs[[specy]] <- conf1
    }
  }
  
  object <- list(env = envs, conf = confs)
  class(object) <- c("ewing_envelopes", class(object))
  attr(object, "species") <- species
  attr(object, "items") <- items
  attr(object, "ordinate") <- ordinate
  attr(object, "nsim") <- nsim
  attr(object, "confidence") <- confidence
  object
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
#' Summary of Ewing Envelopes
#' 
#' @param object object of class `ewing_envelope` or `ewing_envelopes`
#' @param species subset on `species` if not `NULL`
#' @param ... additional parameters
#' 
#' @export
#' @method summary ewing_envelopes
#' @rdname ewing_envelope
summary.ewing_envelopes <- function(object, species = NULL, ...) {
  # object$conf[[specy]][[item]] is time by 6-num boxplot summary
  if(is.null(object$conf)) {
    return(NULL)
  }
  out <- dplyr::bind_rows(
    purrr::map(
      object$conf,
      function(x) {
        # somehow get summary across species and items using as.data.frame
        x <- x[names(x) != ""]
        dplyr::bind_rows(
          purrr::map(
            x,
            as.data.frame),
          .id = "item")
      }),
    .id = "species")
  if(!is.null(species)) {
    if(species %in% unique(out$species)) {
      sp <- species
      out <- dplyr::filter(out, species == sp)
    }
  }
  out
}
#' GGplot of Ewing multiple envelopes
#' 
#' @param object object of class `ewing_envelope` or `ewing_envelopes`
#' @param confidence plot confidence bands if `TRUE`
#' @param main title for plot
#' @param ... additional parameters
#' 
#' @rdname ggplot_ewing_envelope
#' @export
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom GET fBoxplot
#' @importFrom ggplot2 labs
#' 
ggplot_ewing_envelopes <- function(object, confidence = FALSE, main = "", ...) {
  if(inherits(object, "ewing_discrete")) {
    object <- ewing_envelopes(object)
  }
  species <- attr(object, "species")
  items <- attr(object, "items")
  ordinate <- attr(object, "ordinate")
  nsim <- attr(object, "nsim")
  confidence <- confidence & attr(object, "confidence")
  
  patch <- list()
  for(specy in species) {
    p <- list()
    for(item in items[[specy]]) {
      if(confidence) {
        p[[item]] <- plot_plot(object$conf[[specy]][[item]], main = main) + 
          ggplot2::labs(x = "time", y = item)
        
      } else {
        p[[item]] <- ggplot_ewing_envelope(object$env[[specy]][[item]])
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
#' @param main title for plot
#' @param ... additional parameters
#' 
#' @export
#' @importFrom ggplot2 ggtitle labs
#' @importFrom GET forder
#' 
ggplot_ewing_envelope <- function(object, cols = c("#21908CFF", "#440154FF", "#5DC863FF"), 
                                  main = "", ...) {
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
    p <- plot(object, idx = idx, col_idx = cols, main = main)
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

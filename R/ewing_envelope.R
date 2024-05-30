#' Create Envelope of Ewing Simulations
#' 
#' Create envelope object for plotting from multiple runs of Ewing simulation.
#' 
#' 
#' @aliases ewing_envelope ewing_envelopes summary.ewing_envelopes
#' print.ewing_envelopes
#' @param object object of class `ewing_envelope` or `ewing_envelopes`
#' @param species subset on `species` if not `NULL`
#' @param item name of item in `species` to build envelope
#' @param ordinate name of ordinate (X axis) to build envelope
#' @param increment increament for discretizing
#' @param verbose print settings if `TRUE`
#' @param ... additional parameters
#' @param x object of class `ewing_envelope` or `ewing_envelopes`
#' @export ewing_envelope
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
  
  out <- GET::create_curve_set(list(r = as.matrix(pulled)[,1], 
                                    obs = as.matrix(pulled[,-1])))
  class(out) <- c("ewing_envelope", class(out))
  attr(out, "count") <- attr(object, "count")
  attr(out, "nstep") <- attr(object, "nstep")
  attr(out, "nsim") <- attr(object, "nsim")
  
  attr(out, "species") <- species
  attr(out, "item") <- item
  attr(out, "ordinate") <- ordinate
  out
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
  nstep <- attr(object, "nstep")
  count <- attr(object, "count")
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
  attr(object, "nstep") <- nstep
  attr(object, "count") <- count
  attr(object, "nsim") <- nsim
  attr(object, "confidence") <- confidence
  object
}
#' Summary of Ewing Envelopes
#' 
#' @param object object of class `ewing_envelope` or `ewing_envelopes`
#' @param species subset on `species` if not `NULL`
#' @param verbose print settings if `TRUE`
#' @param ... additional parameters
#' 
#' @export
#' @importFrom dplyr across filter group_by mutate ungroup
#' @importFrom rlang .data
#' @method summary ewing_envelopes
#' @rdname ewing_envelope
summary.ewing_envelopes <- function(object, species = NULL, verbose = TRUE, ...) {
  # object$conf[[specy]][[item]] is time by 6-num boxplot summary
  if(verbose) {
    nstep <- attr(object, "nstep")
    count <- attr(object, "count")
    nsim <- attr(object, "nsim")
    cat(nsim, "Runs of ",
        nstep, "Steps for", 
        paste(names(count), count, sep = "=", collapse = ", "), "\n")
  }
  out <- print(object, species, ...)
  if(!is.null(out)) {
    out <- dplyr::ungroup(
      dplyr::filter(
        dplyr::group_by(
          out,
          .data$species, .data$item),
        (.data$r == 0) | (.data$r == max(.data$r))))
  }
  out
}
#' Print of Ewing Envelopes
#' 
#' @param x object of class `ewing_envelope` or `ewing_envelopes`
#' @param species subset on `species` if not `NULL`
#' @param ... additional parameters
#' 
#' @export
#' @method print ewing_envelopes
#' @rdname ewing_envelope
print.ewing_envelopes <- function(x, species = NULL, ...) {
  # x$conf[[specy]][[item]] is time by 6-num boxplot summary
  if(is.null(x$conf)) {
    return(NULL)
  }
  out <- dplyr::bind_rows(
    purrr::map(
      x$conf,
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
  dplyr::mutate(out, dplyr::across(where(is.numeric), function(x) pmax(x,0)))
  out
}



#' GGplot of Ewing multiple envelopes
#' 
#' GGplot of Ewing multiple envelopes
#' 
#' GGplot of Ewing envelope
#' 
#' GGplot of Ewing Envelope
#' 
#' 
#' @aliases ggplot_ewing_envelopes ggplot_ewing_envelope
#' autoplot.ewing_envelope
#' @param object object of class `ewing_envelope`
#' @param confidence plot confidence bands if `TRUE`
#' @param main title for plot
#' @param ... additional arguments
#' @param cols colors for top simulations
#' @export ggplot_ewing_envelopes
ggplot_ewing_envelopes <- function(object, confidence = FALSE, main = "", ...) {
  if(inherits(object, "ewing_discrete")) {
    object <- ewing_envelopes(object)
  }
  species <- attr(object, "species")
  items <- attr(object, "items")
  ordinate <- attr(object, "ordinate")
  nstep <- attr(object, "nstep")
  count <- attr(object, "count")
  nsim <- attr(object, "nsim")
  confidence <- confidence & attr(object, "confidence")
  
  patch <- list()
  for(specy in species) {
    p <- list()
    for(item in items[[specy]]) {
      if(confidence) {
        p[[item]] <- plot(object$conf[[specy]][[item]], main = main) + 
          ggplot2::labs(x = "time", y = item) +
          ggplot2::ggtitle(main) +
          ggplot2::ylim(0, NA)
        
      } else {
        p[[item]] <- ggplot_ewing_envelope(object$env[[specy]][[item]])
      }
    }
    ### USE patchwork::wrap_plots here
    patch[[specy]] <- patchwork::wrap_plots(p) + 
      patchwork::plot_layout(nrow = length(p))
  }
  # NEED TO get attribute count and nstep in here
  patchwork::wrap_plots(patch) +
    patchwork::plot_annotation(
      title = paste(nsim, "Runs of ",
                    nstep, "Steps for", 
                    paste(species, count, sep = "=", collapse = ", ")))
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

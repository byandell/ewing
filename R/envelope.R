#' Envelope for Ewing simulations
#' 
#' This currently relies on integer `step` value, but ultimately want to use the `time`,
#' which are not the same across simulations. That will require using spline interpolation
#' to a common grid. More work.
#'  
#' https://cran.r-project.org/web/packages/GET/GET.pdf
#' https://cran.r-project.org/web/packages/GET/vignettes/GET.pdf
#' https://medium.com/runic-software/simple-guide-to-plotting-monte-carlo-envelopes-for-spatial-statistics-in-r-295b04b64ec1
#' https://rss.onlinelibrary.wiley.com/doi/10.1111/j.1467-9876.2009.00701.x
#' https://stat.ethz.ch/R-manual/R-devel/library/boot/html/envelope.html
#' 
#' @export
#' @importFrom purrr map
#' 
envelope_sim <- function(nsim)
{
  sims <- seq_len(nsim)
  out <- purrr::map(sims, function(x) {
    mysim <- init.simulation(interact = FALSE, messages = FALSE)
    readCount(
      future.events(
        mysim, nstep = 1000, refresh = 1000, plotit = FALSE, messages = FALSE))
  })
}

#' @export
#' @importFrom tidyr fill pivot_wider
#' @importFrom dplyr arrange bind_rows distinct
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom GET create_curve_set
#' 
ewing_envelope <- function(msim, species, item) {
  # Pull out `step` and `element` for each run 
  pulled <-  
    tidyr::fill(
      dplyr::arrange(
        tidyr::pivot_wider(
          dplyr::bind_rows(
            purrr::map(
              msim,
              function(x) {
                dplyr::distinct(
                  as.data.frame(x[[species]][,c("step", item)]),
                  .data$step,
                  .keep_all = TRUE)
              }),
            .id = "run"),
          names_from = "run",
          values_from = item),
        .data$step),
      -1)
  crset <- GET::create_curve_set(list(r = as.matrix(pulled)[,1], 
                                      obs = as.matrix(pulled[,-1])))
  class(crset) <- c("ewing_envelope", class(crset))
  attr(crset, "species") <- species
  attr(crset, "item") <- item
  crset
}

#' @export
#' @importFrom ggplot2 ggtitle labs
#' @importFrom GET forder
#' 
ggplot_ewing_envelope <- function(crset, cols = c("#21908CFF", "#440154FF", "#5DC863FF"), ...) {
  A <- GET::forder(crset, measure = 'area')

  item <- attr(crset, "item")
  species <- attr(crset, "species")
  
  plot(crset, idx = order(A)[seq_along(cols)], col_idx = cols) + 
    ggplot2::labs(x = "Step", y = item) +
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

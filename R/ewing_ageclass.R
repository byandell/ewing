#' Ravel count from Ewing simulation models
#' 
#' Ravel counts to produce one comprehensive data frame across species
#' 
#' 
#' @param community object of class \code{ewing} with population data by
#' species
#' @param substrate include substrate if \code{TRUE}
#' @param total add total column if \code{TRUE}
#' @param normalize normalize counts to 1 if \code{TRUE}
#' @param object object of class \code{ewing_ageclass}
#' @param main plot main title
#' @param title plot title
#' @param ... extra parameters not used
#' @author Brian S. Yandell, \email{yandell@@stat.wisc.edu}
#' @seealso \code{\link{init.simulation}}, \code{\link{future.events}},
#' \code{\link{summary.ewing}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' 
#' \dontrun{
#' ewing_ageclass( community )
#' }
#' 
#' 
#' @export ewing_ageclass
#' @importFrom dplyr any_of bind_rows filter group_by mutate summarize ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @importFrom ggplot2 aes facet_wrap geom_point geom_step ggplot ggtitle labs scale_shape_manual
#' @importFrom rlang .data
ewing_ageclass <- function(community, substrate = TRUE, total = TRUE,
                           normalize = TRUE, ...) {
  nsim_val <- NULL
  nstep_val <- NULL
  if (inherits(community, "ewing_discrete")) {
    nsim_val <- attr(community, "nsim")
    nstep_val <- attr(community, "nstep")
    community <- community[[1]]
  }
  count <- readCount(community)
  if(!length(count)) return(NULL)
  species <- names(count)
  if(is.null(species)) return(NULL)
  
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
        Count = {
          m <- max(.data$Count, na.rm = TRUE)
          if (!is.na(m) && m > 0) .data$Count / m else 0
        }))
  }
  subs <- if (substrate) unlist(substrates) else NULL
  if (length(species) > 1) {
    ordered_levels <- unique(c(ageclass[[species[1]]], "total", unlist(ageclass[species[-1]]), subs))
  } else {
    ordered_levels <- unique(c(unlist(ageclass), "total", subs))
  }
  out$State <- factor(out$State, levels = ordered_levels)
  
  attr(out, "nstep") <- if (!is.null(nstep_val)) nstep_val else attr(community, "nstep")
  if (is.null(attr(out, "nstep")) && !is.null(out$step)) {
    attr(out, "nstep") <- max(out$step, na.rm = TRUE)
  }
  attr(out, "nsim") <- if (!is.null(nsim_val)) nsim_val else attr(community, "nsim")
  
  class(out) <- c("ewing_ageclass", class(out))
  out
}
#' @export
#' @rdname ewing_ageclass
ggplot_ewing_ageclass <- function(object, main = NULL, title = NULL, ... )
{
  if (is.null(title)) title <- main
  if (is.null(title)) {
    nstep <- attr(object, "nstep")
    if (is.null(nstep) && !is.null(object$step)) {
      nstep <- max(object$step, na.rm = TRUE)
    }
    nsim <- attr(object, "nsim")
    if (!is.null(nstep)) {
      if (!is.null(nsim) && nsim > 1) {
        title <- paste0("Age Distribution over Time (", nstep, " steps, nsim = ", nsim, ")")
      } else {
        title <- paste0("Age Distribution over Time (", nstep, " steps)")
      }
    } else if (!is.null(nsim) && nsim > 1) {
      title <- paste0("Age Distribution over Time (nsim = ", nsim, ")")
    } else {
      title <- "Age Distribution over Time"
    }
  }
  
  ggplot2::ggplot(object) +
    ggplot2::aes(.data$time, .data$Count, col = .data$State, group = .data$State, shape = .data$Species) +
    ggplot2::geom_step(na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::scale_shape_manual(name = "Species", values = c(1, 2, 0, 5, 6, 3, 4)) +
    ggplot2::labs(title = title, color = "State", shape = "Species") +
    ggplot2::facet_wrap(.data$Type ~ .data$Species, scales = "free")
}
#' @export
#' @rdname ewing_ageclass
#' @method autoplot ewing_ageclass
autoplot.ewing_ageclass <- function(object, ...)
  ggplot_ewing_ageclass(object, ...)


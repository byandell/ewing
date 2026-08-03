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
  
  if (inherits(community, "isle_royale_sim")) {
    if (is.null(community$history) || nrow(community$history) == 0) return(NULL)
    out <- community$history
    if (!substrate) {
      out <- dplyr::filter(out, .data$Type != "substrate")
    }
    if (total) {
      tot <- dplyr::mutate(
        dplyr::ungroup(
          dplyr::summarize(
            dplyr::group_by(out, .data$Species, .data$step, .data$time, .data$Type),
            Count = sum(.data$Count),
            .groups = "drop"
          )
        ),
        State = "total"
      )
      out <- dplyr::bind_rows(out, tot)
    }
    if (normalize) {
      out <- dplyr::ungroup(
        dplyr::mutate(
          dplyr::group_by(out, .data$Species, .data$State, .data$Type),
          Count = {
            m <- max(.data$Count, na.rm = TRUE)
            if (!is.na(m) && m > 0) .data$Count / m else 0
          }
        )
      )
    }
    ordered_levels <- unique(c("calf", "yearling", "adult", "senior", "pup", "subadult", "total"))
    out$State <- factor(out$State, levels = ordered_levels[ordered_levels %in% unique(out$State)])
    attr(out, "nstep") <- community$nstep
    attr(out, "units") <- "days"
    class(out) <- c("ewing_ageclass", class(out))
    return(out)
  }
  
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
    out[[i]] <- dplyr::mutate(
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
  attr(out, "units") <- if (inherits(community, "isle_royale_sim")) "days" else tryCatch(getOrgFeature(community, species[1], "units"), error = function(e) "time")
  
  class(out) <- c("ewing_ageclass", class(out))
  out
}
#' @export
#' @rdname ewing_ageclass
ggplot_ewing_ageclass <- function(object, main = NULL, title = NULL, x_var = c("step", "time"), time_unit = NULL, ... )
{
  x_var <- match.arg(x_var)
  
  if (is.null(time_unit)) {
    time_unit <- attr(object, "units")
  }
  if (is.null(time_unit) || is.na(time_unit) || time_unit == "NA") {
    time_unit <- "time"
  }
  
  if (is.null(title)) title <- main
  if (is.null(title)) {
    nstep <- attr(object, "nstep")
    if (is.null(nstep) && !is.null(object$step)) {
      nstep <- max(object$step, na.rm = TRUE)
    }
    nsim <- attr(object, "nsim")
    unit_str <- if (x_var == "step") "steps" else time_unit
    time_hdr <- if (x_var == "step") "Steps" else paste0(toupper(substring(time_unit, 1, 1)), substring(time_unit, 2))
    if (!is.null(nstep)) {
      if (!is.null(nsim) && nsim > 1) {
        title <- paste0("Age Classes over ", time_hdr, " (", nstep, " ", unit_str, ", nsim = ", nsim, ")")
      } else {
        title <- paste0("Age Classes over ", time_hdr, " (", nstep, " ", unit_str, ")")
      }
    } else {
      title <- paste0("Age Classes over ", time_hdr)
    }
  }
  
  species_vec <- unique(as.character(object$Species))
  if (length(species_vec) == 0) species_vec <- "Organism"
  
  p_list <- list()
  
  for (sp in species_vec) {
    df_sp <- object[object$Species == sp, , drop = FALSE]
    if (is.factor(df_sp$State)) {
      df_sp$State <- droplevels(df_sp$State)
    }
    
    x_col <- if (x_var == "step" && "step" %in% names(df_sp)) "step" else "time"
    x_lbl <- if (x_var == "step") "steps" else time_unit
    
    sp_title <- paste(toupper(substring(sp, 1, 1)), substring(sp, 2), " Age Classes", sep = "")
    
    p_sub <- ggplot2::ggplot(df_sp, ggplot2::aes(x = .data[[x_col]], y = .data$Count, col = .data$State, group = .data$State)) +
      ggplot2::geom_step(na.rm = TRUE, linewidth = 0.8) +
      ggplot2::geom_point(size = 2, na.rm = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = sp_title,
        x = x_lbl,
        y = "Count",
        color = "Age Class"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
        legend.position = "right"
      )
      
    p_list[[sp]] <- p_sub
  }
  
  if (length(p_list) == 1) {
    return(p_list[[1]] + ggplot2::ggtitle(title))
  }
  
  grid_plots <- cowplot::plot_grid(plotlist = p_list, ncol = length(p_list), align = "h")
  title_widget <- cowplot::ggdraw() + 
    cowplot::draw_label(title, fontface = 'bold', x = 0.5, hjust = 0.5, size = 13)
  
  cowplot::plot_grid(title_widget, grid_plots, ncol = 1, rel_heights = c(0.12, 1))
}
#' @export
#' @rdname ewing_ageclass
#' @method autoplot ewing_ageclass
autoplot.ewing_ageclass <- function(object, x_var = c("step", "time"), ...) {
  x_var <- match.arg(x_var)
  ggplot_ewing_ageclass(object, x_var = x_var, ...)
}


#' Ewing Substrate by Species
#' 
#' Ewing Substrate by Species with optional hexagonal topology layout, multi-species support, and substrate-level coordinate rescaling.
#' 
#' @aliases ewing_substrate ggplot_ewing_substrate autoplot.ewing_substrate
#' @param community Simulation community object (`ewing` S3 class)
#' @param species Species name or vector of species names (e.g. `"host"`, `"parasite"`, or `c("host", "parasite")`)
#' @param headstuff Title parameters
#' @param units Unit labels
#' @param right Right label
#' @param adj Adjustment
#' @param show_sub Substrate filter (defaults to all active substrates)
#' @param step Current step
#' @param layout Display layout: `"facet"` (default) for separated panels or `"hex"` for global hexagonal substrate grid overlay
#' @param width Substrate radius limit (default: 10)
#' @param step_density Grid density spacing interval (default: 1)
#' @param rescale Logical; if `TRUE` (default for `"hex"` layout), rescales organism local coordinates so they fit strictly within each substrate patch's surface area.
#' @param ... Additional arguments
#' @export ewing_substrate
#' @importFrom dplyr distinct filter mutate arrange
#' @importFrom tibble tibble
#' @importFrom ggplot2 aes facet_grid facet_wrap geom_text geom_polygon ggplot ggtitle scale_color_manual xlab ylab theme_void coord_fixed guides guide_legend
#' @importFrom rlang .data
ewing_substrate <- function( community,
                             species,
                             headstuff = c( 0, "start", sum( to.plot )),
                             units = getOrgFeature( community, species[1], "units" ),
                             right = species[1], adj = c(0,.5,1),
                             show_sub = NULL,
                             step = 0,
                             layout = c("facet", "hex"),
                             width = 10,
                             step_density = 1,
                             rescale = TRUE,
                             ...)
{
  layout <- match.arg(layout)
  
  if (length(species) > 1) {
    res_list <- lapply(species, function(sp) {
      df <- ewing_substrate(community = community, species = sp, headstuff = headstuff,
                            units = units, right = right, adj = adj, show_sub = show_sub,
                            step = step, layout = layout, width = width, step_density = step_density,
                            rescale = rescale, ...)
      if (!is.null(df) && nrow(df) > 0) {
        df$species <- sp
      }
      df
    })
    res_list <- res_list[!sapply(res_list, is.null)]
    if (length(res_list) == 0) return(NULL)
    combined <- do.call(rbind, res_list)
    attr(combined, "species") <- paste(species, collapse = " & ")
    step_val <- if (!is.null(community$step)) community$step else if (!is.null(community$count$step)) community$count$step else if (!is.null(attr(community, "nstep"))) attr(community, "nstep") else step
    attr(combined, "step") <- step_val
    attr(combined, "layout") <- layout
    attr(combined, "width") <- width
    attr(combined, "step_density") <- step_density
    class(combined) <- c("ewing_substrate", class(combined))
    return(combined)
  }
  
  ## plot current stages for species (except random parasites)
  organism <- get.species( community, species )[,-1]
  if(is.null(organism)) # species is not in community
    return(NULL)
  
  future <- getOrgFuture( community, species, c("color","pch") )
  
  # Substrate names mapping (e.g. fr1, fr2, fr3, fr4, twig, lftop, lfbot)
  substrate_feat <- getOrgFeature( community, species, "substrate")
  sub_interact <- getOrgInteract(community, substrate_feat, species)
  substrates <- rownames(sub_interact)
  if (is.null(substrates) || length(substrates) == 0) {
    substrates <- names(getOrgInteract(community, substrate_feat, substrate_feat))
  }
  if (is.null(show_sub)) show_sub <- substrates
  
  position <- paste( "pos", letters[1:3], sep = "." )
  
  if (layout == "hex") {
    topo <- substrate_topology(width = width, step = step_density)
    n_org <- ncol(organism)
    gx <- numeric(n_org)
    gy <- numeric(n_org)
    
    sub_indices <- organism["sub.stage", ]
    org_sub_names <- substrates[sub_indices]
    
    # Process organisms per substrate patch to rescale local coordinates into substrate surface triangle
    unique_subs <- unique(org_sub_names)
    
    for (sub in unique_subs) {
      idx <- which(org_sub_names == sub)
      
      target_name <- sub
      if (!target_name %in% names(topo)) {
        if (target_name == "twig") target_name <- "tw1"
        if (target_name == "tw1") target_name <- "twig"
      }
      
      cfg <- topo[[target_name]]
      
      pa <- organism["pos.a", idx]
      pb <- organism["pos.b", idx]
      pc <- organism["pos.c", idx]
      
      if (!is.null(cfg)) {
        # Determine substrate surface width (allows substrates of different sizes in future)
        W_sub <- if (!is.null(cfg$width)) (cfg$width - step_density) else (width - step_density)
        
        if (rescale) {
          amin <- min(pa); amax <- max(pa)
          bmin <- min(pb); bmax <- max(pb)
          
          u <- if (amax > amin) (pa - amin) / (amax - amin) else rep(0.5, length(idx))
          v <- if (bmax > bmin) (pb - bmin) / (bmax - bmin) else rep(0.5, length(idx))
          
          # 15% inner padding to ensure symbols sit comfortably inside substrate polygon borders
          u_m <- 0.15 + 0.70 * u
          v_m <- 0.15 + 0.70 * v
          
          a_p <- u_m * W_sub
          b_p <- v_m * (W_sub - a_p)
          c_p <- -(a_p + b_p)
        } else {
          a_p <- pa
          b_p <- pb
          c_p <- pc
        }
        
        off <- cfg$offset
        if (cfg$dir == "up") {
          ga <- a_p + off[1]
          gb <- b_p + off[2]
          gc <- c_p + off[3]
        } else {
          ga <- -a_p + off[1]
          gb <- -b_p + off[2]
          gc <- -c_p + off[3]
        }
        car <- tri2car(tricoord(ga, gb, gc))
        gx[idx] <- car$x
        gy[idx] <- car$y
      } else {
        car <- tri2car(organism[position, idx, drop = FALSE])
        gx[idx] <- car$x
        gy[idx] <- car$y
      }
    }
    
    xy <- data.frame(x = gx, y = gy)
  } else {
    xy <- tri2car( organism[position,] )
  }
  
  dat <- dplyr::filter(
    dplyr::mutate(
      tibble::tibble(xy), 
      stage = organism["stage",],
      substrate = substrates[organism["sub.stage",]],
      pchar = factor(as.character( future$pch[.data$stage] ), levels = unique(as.character(future$pch))),
      color = as.character( future$color[.data$stage] ),
      species = species),
    .data$substrate %in% show_sub)
  
  attr(dat, "species") <- species
  step_val <- if (!is.null(community$step)) community$step else if (!is.null(community$count$step)) community$count$step else if (!is.null(attr(community, "nstep"))) attr(community, "nstep") else step
  attr(dat, "step") <- step_val
  attr(dat, "layout") <- layout
  attr(dat, "width") <- width
  attr(dat, "step_density") <- step_density
  class(dat) <- c("ewing_substrate", class(dat))
  dat
}

#' @param object Object of class `ewing_substrate`
#' @param xlab Label for x axis (facet layout)
#' @param ylab Label for y axis (facet layout)
#' @param layout Display layout (`"facet"` or `"hex"`)
#' @param width Substrate radius limit (for hex layout)
#' @param step_density Grid density spacing interval (for hex layout)
#' @param layers Display layers for hex layout: vector containing any of `"poly"`, `"hex"`, `"organisms"`, `"centers"`, `"labels"`
#' @param ... Additional arguments
#' @importFrom ggplot2 aes facet_grid facet_wrap geom_text geom_polygon ggplot scale_color_manual xlab ylab theme_void coord_fixed guides guide_legend
#' @importFrom rlang .data
#' @importFrom dplyr distinct arrange
#' @rdname ewing_substrate
#' @export
ggplot_ewing_substrate <- function(object,
                                   xlab = "horizontal", ylab = "vertical",
                                   layout = attr(object, "layout"),
                                   width = attr(object, "width"),
                                   step_density = attr(object, "step_density"),
                                   layers = c("poly", "hex", "organisms", "centers", "labels"),
                                   ...)
{     
  if (is.null(layout)) layout <- "facet"
  if (is.null(width)) width <- 10
  if (is.null(step_density)) step_density <- 1
  
  species <- attr(object, "species")
  step <- attr(object, "step")
  
  # Allows same color for different pchar, but only one color per pchar.
  tmp <- dplyr::arrange(
    dplyr::distinct(
      dplyr::distinct(object, .data$pchar, .data$color),
      .data$pchar, .keep_all = TRUE),
    .data$pchar)
  col.palate <- tmp$color
  names(col.palate) <- as.character(tmp$pchar)
  
  if (layout == "hex") {
    topo <- substrate_topology(width = width, step = step_density)
    sub_obj <- create_substrate(topo, width = width, step = step_density)
    hex_overlay <- create_hex_overlay(sub_obj, step = step_density)
    
    p <- ggplot2::ggplot()
    if ("poly" %in% layers && nrow(sub_obj$poly) > 0) {
      p <- p + ggplot2::geom_polygon(data = sub_obj$poly, ggplot2::aes(x = x, y = y, group = substrate), 
                                     fill = NA, color = "black", linewidth = 0.7)
    }
    if ("hex" %in% layers && nrow(hex_overlay) > 0) {
      p <- p + ggplot2::geom_polygon(data = hex_overlay, ggplot2::aes(x = x, y = y, group = cell_id), 
                                     fill = NA, color = "gray75", linewidth = 0.3)
    }
    if ("organisms" %in% layers && nrow(object) > 0) {
      p <- p + ggplot2::geom_text(data = object, ggplot2::aes(x = x, y = y, label = pchar, color = pchar), 
                                  fontface = "bold", size = 4) +
        ggplot2::scale_color_manual(name = "Stage", values = col.palate) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(label = names(col.palate))))
    }
    if ("centers" %in% layers && nrow(sub_obj$centers) > 0) {
      p <- p + ggplot2::geom_text(data = sub_obj$centers, ggplot2::aes(x = x, y = y, label = substrate), 
                                  color = "black", fontface = "bold", size = 4.5)
    }
    if ("labels" %in% layers && nrow(sub_obj$labels) > 0) {
      p <- p + ggplot2::geom_text(data = sub_obj$labels, ggplot2::aes(x = x, y = y, label = label), 
                                  color = "darkred", fontface = "bold", size = 3.5)
    }
    
    return(p + ggplot2::theme_void() + 
             ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2, "pt")) +
             ggplot2::coord_fixed() + 
             ggplot2::ggtitle(paste(species, "on Hex Substrate Grid at", step, "steps")))
  }
  
  # Default Facet View by Substrate Component
  ggplot2::ggplot(object) +
    ggplot2::aes(.data$x, .data$y, label = .data$pchar, col = .data$pchar) +
    ggplot2::geom_text() +
    ggplot2::facet_wrap(~ substrate) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_color_manual(name = "Stage", values = col.palate) + 
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(label = names(col.palate)))) +
    ggplot2::ggtitle(paste(species, "on substrate at", step, "steps"))
}
#' @export
#' @rdname ewing_substrate
#' @method autoplot ewing_substrate
autoplot.ewing_substrate <- function(object, ...)
  ggplot_ewing_substrate(object, ...)

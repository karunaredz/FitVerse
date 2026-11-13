# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : plot_fitverse.R
# Description: Publication-quality diagnostic plots for fitted
#              distributions.
#
#              plot.fitverse() produces a 2-panel figure:
#                Panel 1 - Histogram + overlaid fitted density curve(s)
#                          with the PDF formula annotated below.
#                Panel 2 - Q-Q plot comparing empirical vs theoretical
#                          quantiles, with a pointwise 95% CI band.
#
#              If interactive = TRUE and plotly is installed, an
#              interactive HTML version is produced instead.
#
#              Design principles:
#               - Clean theme_bw base - white background, minimal grid
#               - Colorblind-safe palette (Wong 2011)
#               - Best-fit curve: thick solid line (1.55 pt)
#               - Alternate curves: thinner long-dash lines (0.85 pt)
#               - Legend inside the plot (upper-right) for space efficiency
#               - Q-Q confidence band from Beta-order-statistic percentiles
#               - 600 DPI default for journal submission
#               - Formula rendered using R's native plotmath engine
#
# Authors:
#   Karuna G. Reddy <karuna.reddy@auckland.ac.nz>
#   University of Auckland, New Zealand
#
#   M. G. M. Khan <mgm.khan@usp.ac.fj>
#   University of the South Pacific, Fiji
#
# Package : FitVerse
# Version : 1.0-1
# Date    : 2026-06-17
# License : GPL (>= 3)
# ============================================================


# -- Publication colour palette ------------------------------------------------
# Colorblind-safe (Wong 2011 + extensions).  Best fit always gets the first
# (deep midnight navy) colour; alternates cycle through the rest.
.pub_curve_colours <- c(
  "#0D3B66",  # midnight navy - best fit (dominant, strong)
  "#E63946",  # vivid crimson
  "#2A9D8F",  # teal-green
  "#8338EC",  # violet
  "#FB8500",  # warm orange
  "#118C8C",  # dark teal
  "#7B2D8B"   # deep plum
)


# -- S3 plot method ------------------------------------------------------------

#' Plot method for fitverse objects
#'
#' Produces a publication-quality two-panel diagnostic plot:
#' a density-overlaid histogram with the annotated PDF formula (left) and
#' a Q-Q plot with pointwise 95% confidence band (right).
#'
#' @param x           A \code{fitverse} object (output of \code{fitverse()}).
#' @param which       Character: \code{"density"} (histogram + density only),
#'                    \code{"qq"} (Q-Q plot only), or \code{"both"} (default).
#' @param top_n       Number of additional distribution curves to overlay.
#'                    Set to 1 for best fit only.  Default 3.
#' @param xlab        Character: x-axis label.  If \code{NULL} (default), the
#'                    label is taken from \code{x$xlab} if present, otherwise
#'                    \code{"x"}.
#' @param interactive Logical: produce an interactive plotly figure?
#'                    Requires the \pkg{plotly} package.  Default \code{FALSE}.
#' @param ...         Additional arguments (currently unused).
#' @return A ggplot2 object (or plotly object if \code{interactive = TRUE}),
#'         invisibly.
#' @export
#' @method plot fitverse
plot.fitverse <- function(x,
                          which       = "both",
                          top_n       = 3L,
                          xlab        = NULL,
                          interactive = FALSE,
                          draw        = TRUE,
                          ...) {

  fit    <- x$best_fit
  data   <- x$data
  entry  <- get_dist_entry(fit$dist_key)
  n      <- x$n

  # Resolve xlab: explicit arg > stored in object > fallback "x"
  xlab <- if (!is.null(xlab)) xlab else if (!is.null(x$xlab)) x$xlab else "x"

  # -- Build only the panels that are actually needed -----------------------
  # Avoid building both panels when only one is requested - each panel does
  # non-trivial ggplot2 work and, in the Shiny context, ggplotly() renders
  # the object immediately, so building the unused panel wastes time and can
  # trigger graphics-device side-effects on macOS.
  p_density <- if (which %in% c("density", "both")) .plot_density_panel(x, top_n = top_n, xlab = xlab) else NULL
  p_qq      <- if (which %in% c("qq",      "both")) .plot_qq_panel(x, xlab = xlab) else NULL

  # -- Interactive version via plotly ---------------------------------------
  if (interactive) {
    if (!has_pkg("plotly", "interactive plots")) {
      cat("  [note] plotly not found - falling back to static plot.\n")
    } else {
      return(invisible(plotly::ggplotly(p_density)))
    }
  }

  # -- Draw and return invisibly ---------------------------------------------
  switch(which,
    density = { if (draw) print(p_density); invisible(p_density) },
    qq      = { if (draw) print(p_qq);      invisible(p_qq)      },
    both    = { if (draw) .combine_panels(p_density, p_qq, fv_obj = x);
                invisible(NULL) }
  )
}


# -- Density panel -------------------------------------------------------------

#' Build the histogram + density overlay panel (publication quality)
#' @keywords internal
.plot_density_panel <- function(fv_obj, top_n = 3L, xlab = "x") {

  data    <- fv_obj$data
  fit     <- fv_obj$best_fit
  ranking <- fv_obj$ranking
  entry   <- get_dist_entry(fit$dist_key)
  n       <- fv_obj$n

  # -- Collect top_n distributions for overlay ------------------------------
  top_rows <- head(ranking, min(top_n, nrow(ranking)))

  # Ensure best_fit is first so it always gets the solid / thick line.
  best_match <- paste0(fit$dist_key, "_", fit$method)
  top_keys   <- paste0(top_rows$dist_key, "_", top_rows$Method)
  best_pos   <- which(top_keys == best_match)
  if (length(best_pos) > 0 && best_pos[1] != 1L) {
    other_idx <- setdiff(seq_len(nrow(top_rows)), best_pos[1])
    top_rows  <- top_rows[c(best_pos[1], other_idx), ]
  }

  n_curves   <- nrow(top_rows)
  method_lbl <- fit$method

  # -- GoF statistics for subtitle / caption --------------------------------
  ks_p <- get_ks_pvalue(fit)
  ad_p <- get_ad_pvalue(fit)

  subtitle_str <- sprintf(
    "n = %d     |     AIC = %.2f     |     BIC = %.2f",
    n, fit$aic, fit$bic
  )
  caption_str <- if (!is.na(ks_p)) {
    sprintf(
      "Goodness-of-fit:   Kolmogorov-Smirnov p = %.4f     |     Anderson-Darling p = %.4f",
      ks_p, ad_p
    )
  } else {
    sprintf("AIC = %.2f   |   BIC = %.2f   |   n = %d", fit$aic, fit$bic, n)
  }

  # -- x-grid for density evaluation ----------------------------------------
  x_range <- range(data)
  x_buf   <- diff(x_range) * 0.06
  x_grid  <- seq(x_range[1] - x_buf, x_range[2] + x_buf, length.out = 600)

  if (entry$support == "positive") x_grid <- x_grid[x_grid > 1e-10]
  if (entry$support == "0to1")     x_grid <- x_grid[x_grid > 1e-10 & x_grid < 1 - 1e-10]

  # -- Histogram base layer -------------------------------------------------
  df    <- data.frame(value = data)
  nbins <- max(10L, ceiling(log2(n) + 1))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins      = nbins,
      fill      = "#5B9ED6",   # rich medium blue - vivid, clean
      colour    = "#FFFFFF",   # white bar borders - crisp bar separation
      alpha     = 0.72,
      linewidth  = 0.28
    ) +
    # Rug: tick marks at each observation - shows raw data without clutter
    ggplot2::geom_rug(
      colour    = "#0D3B66",
      alpha     = 0.25,
      linewidth  = 0.30,
      length    = ggplot2::unit(0.025, "npc"),
      sides     = "b"
    )

  # -- Build combined data frame for density curve overlays -----------------
  # Using a pre-built data frame with all curves avoids the classic ggplot2
  # lazy-evaluation bug where colour/label aesthetic captures the loop variable
  # NAME rather than its current value.
  curve_segments <- vector("list", n_curves)
  drawn_labels   <- character(0)

  for (i in seq_len(n_curves)) {

    row_i   <- top_rows[i, ]
    key_i   <- row_i$dist_key
    meth_i  <- row_i$Method
    fit_key <- paste0(key_i, "_", meth_i)
    fit_i   <- fv_obj$all_fits[[fit_key]]
    if (is.null(fit_i)) next

    entry_i <- get_dist_entry(key_i)
    d_fun   <- .lookup_dpqr("d", key_i)
    if (is.null(d_fun)) next

    dens_vals <- tryCatch({
      do.call(d_fun, c(list(x_grid), as.list(fit_i$params)))
    }, error = function(e) NULL)
    if (is.null(dens_vals) || !any(is.finite(dens_vals))) next

    is_best <- (length(drawn_labels) == 0L)
    lbl_i   <- sprintf("%s  (%s)", entry_i$display_name, meth_i)

    curve_segments[[i]] <- data.frame(
      x      = x_grid,
      y      = dens_vals,
      label  = lbl_i,
      ltype  = if (is_best) "solid"    else "longdash",
      lwidth = if (is_best) 1.55       else 0.85,
      stringsAsFactors = FALSE
    )
    drawn_labels <- c(drawn_labels, lbl_i)
  }

  curve_segments <- Filter(Negate(is.null), curve_segments)

  if (length(curve_segments) > 0) {
    df_curves <- do.call(rbind, curve_segments)

    n_drawn        <- length(drawn_labels)
    drawn_colours  <- rep_len(.pub_curve_colours, n_drawn)
    names(drawn_colours) <- drawn_labels

    ltype_map <- c(solid = "solid", longdash = "longdash")

    # Draw alternate curves first; best-fit curve on top so it is never hidden
    df_alt  <- df_curves[df_curves$ltype != "solid",  , drop = FALSE]
    df_best <- df_curves[df_curves$ltype == "solid",   , drop = FALSE]

    if (nrow(df_alt) > 0) {
      p <- p +
        ggplot2::geom_line(
          data = df_alt,
          ggplot2::aes(x = x, y = y, colour = label, linetype = ltype),
          linewidth = 0.55
        )
    }
    p <- p +
      ggplot2::geom_line(
        data = df_best,
        ggplot2::aes(x = x, y = y, colour = label, linetype = ltype),
        linewidth = 0.85
      ) +
      ggplot2::scale_colour_manual(values = drawn_colours, name = NULL) +
      ggplot2::scale_linetype_manual(values = ltype_map, guide = "none")
  }

  # -- Labels and theme -----------------------------------------------------
  p +
    ggplot2::labs(
      title    = sprintf("%s  -  %s", entry$display_name, method_lbl),
      subtitle = subtitle_str,
      x        = xlab,
      y        = "Density",
      caption  = caption_str
    ) +
    # theme_light gives a clean panel with a subtle light border - ideal base
    ggplot2::theme_light(base_size = 13) +
    ggplot2::theme(
      # Light blue-grey panel background - professional and easy on the eye
      panel.background   = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = "#D3DCE6", linewidth = 0.40
      ),
      panel.border       = ggplot2::element_rect(
        fill = NA, colour = "#B8C8D8", linewidth = 0.55
      ),
      # White outer background so the panel sits on a clean page
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      # Title hierarchy
      plot.title         = ggplot2::element_text(
        face = "bold", size = 14, colour = "#1A2B3C",
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle      = ggplot2::element_text(
        colour = "#4A6278", size = 10.5,
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption       = ggplot2::element_text(
        colour = "#7090A8", size = 8.5, hjust = 0, face = "italic",
        margin = ggplot2::margin(t = 7)
      ),
      # Axes
      axis.title         = ggplot2::element_text(size = 11.5, colour = "#1A2B3C"),
      axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
      axis.text          = ggplot2::element_text(size = 10.5, colour = "#354A5E"),
      axis.ticks         = ggplot2::element_line(colour = "#B0C0D0", linewidth = 0.4),
      # Legend at BOTTOM - essential for correct ggplotly() conversion
      legend.position    = if (length(drawn_labels) > 1) "bottom" else "none",
      legend.direction   = "horizontal",
      legend.background  = ggplot2::element_blank(),
      legend.key         = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = 10, colour = "#1A2B3C"),
      legend.margin      = ggplot2::margin(2, 0, 2, 0),
      legend.key.width   = ggplot2::unit(2.0, "cm"),
      legend.key.height  = ggplot2::unit(0.5, "cm"),
      legend.spacing.x   = ggplot2::unit(0.3, "cm"),
      plot.margin        = ggplot2::margin(12, 16, 8, 12)
    )
}


# -- Q-Q panel ----------------------------------------------------------------

#' @noRd
.plot_qq_panel <- function(fv_obj, xlab = "x") {

  data  <- fv_obj$data
  fit   <- fv_obj$best_fit
  entry <- get_dist_entry(fit$dist_key)
  n     <- fv_obj$n

  q_fun <- .lookup_dpqr("q", fit$dist_key)

  if (is.null(q_fun)) {
    return(ggplot2::ggplot() +
             ggplot2::labs(
               title = "Q-Q Plot",
               subtitle = "Quantile function not available for this distribution"
             ) +
             ggplot2::theme_bw(base_size = 13))
  }

  # Theoretical quantiles at probability points
  probs <- ppoints(n)
  q_theor <- tryCatch({
    do.call(q_fun, c(list(probs), as.list(fit$params)))
  }, error = function(e) NULL)

  if (is.null(q_theor) || any(!is.finite(q_theor))) {
    return(ggplot2::ggplot() +
             ggplot2::labs(
               title = "Q-Q Plot",
               subtitle = "Theoretical quantiles could not be computed"
             ) +
             ggplot2::theme_bw(base_size = 13))
  }

  q_emp <- sort(data)

  # -- Pointwise 95% CI band via Beta order-statistic percentiles ------------
  # For the i-th order statistic out of n, the CDF value U_(i) ~ Beta(i, n-i+1).
  # The 95% CI on U_(i) is [qbeta(0.025, i, n-i+1), qbeta(0.975, i, n-i+1)].
  # Applying Q() maps probability bounds -> quantile bounds on the empirical axis.
  # The ribbon shows: if the data truly comes from this distribution, the i-th
  # sample quantile should fall in [q_lo, q_hi] with 95% probability.
  df_band <- tryCatch({
    idx  <- seq_len(n)
    p_lo <- stats::qbeta(0.025, idx, n - idx + 1L)
    p_hi <- stats::qbeta(0.975, idx, n - idx + 1L)
    q_lo <- do.call(q_fun, c(list(p_lo), as.list(fit$params)))
    q_hi <- do.call(q_fun, c(list(p_hi), as.list(fit$params)))
    data.frame(x = q_theor, ymin = q_lo, ymax = q_hi)
  }, error = function(e) NULL)

  # IQR-based reference line
  df_qq  <- data.frame(theoretical = q_theor, empirical = q_emp)
  idx_25 <- max(1L, floor(0.25 * n))
  idx_75 <- min(n,  ceiling(0.75 * n))
  slope  <- (q_emp[idx_75]  - q_emp[idx_25]) /
            (q_theor[idx_75] - q_theor[idx_25])
  intcpt <- q_emp[idx_25] - slope * q_theor[idx_25]

  p_qq <- ggplot2::ggplot(df_qq, ggplot2::aes(x = theoretical, y = empirical))

  # 95% CI ribbon (drawn first - behind the points and reference line)
  if (!is.null(df_band)) {
    p_qq <- p_qq +
      ggplot2::geom_ribbon(
        data        = df_band,
        ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill        = "#AED6F1",
        alpha       = 0.40
      )
  }

  p_qq <- p_qq +
    ggplot2::geom_abline(
      intercept = intcpt, slope = slope,
      colour    = "#E63946", linetype = "dashed", linewidth = 0.8
    ) +
    ggplot2::geom_point(
      colour = "#1A2B3C", alpha = 0.60, size = 1.6,
      shape  = 21, fill = "#5B9ED6", stroke = 0.45
    ) +
    ggplot2::labs(
      title    = "Q-Q Plot",
      subtitle = sprintf("%s  (%s)", entry$display_name, fit$method),
      x        = sprintf("Theoretical Quantiles  [%s]", xlab),
      y        = sprintf("Sample Quantiles  [%s]", xlab),
      caption  = "Shaded band: pointwise 95% confidence interval"
    ) +
    ggplot2::theme_light(base_size = 13) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      panel.grid.major   = ggplot2::element_line(colour = "#D3DCE6", linewidth = 0.40),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_rect(
        fill = NA, colour = "#B8C8D8", linewidth = 0.55
      ),
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title         = ggplot2::element_text(
        face = "bold", size = 14, colour = "#1A2B3C",
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle      = ggplot2::element_text(
        colour = "#4A6278", size = 10.5,
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption       = ggplot2::element_text(
        colour = "#7090A8", size = 8.5, hjust = 0, face = "italic",
        margin = ggplot2::margin(t = 7)
      ),
      axis.title         = ggplot2::element_text(size = 11.5, colour = "#1A2B3C"),
      axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
      axis.text          = ggplot2::element_text(size = 10.5, colour = "#354A5E"),
      axis.ticks         = ggplot2::element_line(colour = "#B0C0D0", linewidth = 0.4),
      plot.margin        = ggplot2::margin(12, 16, 8, 12)
    )

  p_qq
}


# -- Formula + parameter footer strip -----------------------------------------

#' Build a two-column footer grob: PDF formula (left) and parameter estimates
#' (right).  Placed below the main panels by \code{.combine_panels()}.
#' @keywords internal
.make_formula_strip <- function(fv_obj) {

  fit   <- fv_obj$best_fit
  entry <- tryCatch(get_dist_entry(fit$dist_key), error = function(e) NULL)
  if (is.null(entry) || !nzchar(entry$plotmath_str)) return(NULL)

  param_str <- format_params(fit$params, digits = 4,
                             labels = entry$param_labels)

  bg_fill  <- "#F7F9FC"   # near-white blue-tinted background
  bdr_col  <- "#C8D0DC"   # subtle border
  lbl_col  <- "#6C7A8D"   # muted section label
  val_col  <- "#1A253A"   # dark ink for values

  # -- Left panel: PDF formula -----------------------------------------------
  p_left <- ggplot2::ggplot() +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::annotate(
      "text", x = 0.05, y = 0.82,
      label = "PROBABILITY DENSITY FUNCTION",
      size  = 2.5, colour = lbl_col, fontface = "bold",
      hjust = 0, family = "sans"
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.05, xend = 0.95, y = 0.72, yend = 0.72,
      colour = bdr_col, linewidth = 0.4
    ) +
    ggplot2::annotate(
      "text", x = 0.50, y = 0.35,
      label = entry$plotmath_str, parse = TRUE,
      size  = 3.5, hjust = 0.5, vjust = 0.5,
      colour = val_col
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_fill, colour = bdr_col, linewidth = 0.5
      ),
      plot.margin = ggplot2::margin(6, 10, 6, 10)
    )

  # -- Right panel: Estimated parameters ------------------------------------
  p_right <- ggplot2::ggplot() +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::annotate(
      "text", x = 0.05, y = 0.82,
      label = "ESTIMATED PARAMETERS",
      size  = 2.5, colour = lbl_col, fontface = "bold",
      hjust = 0, family = "sans"
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.05, xend = 0.95, y = 0.72, yend = 0.72,
      colour = bdr_col, linewidth = 0.4
    ) +
    ggplot2::annotate(
      "text", x = 0.05, y = 0.35,
      label = param_str,
      size  = 3.3, hjust = 0, vjust = 0.5,
      colour = val_col
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_fill, colour = bdr_col, linewidth = 0.5
      ),
      plot.margin = ggplot2::margin(6, 10, 6, 10)
    )

  gridExtra::arrangeGrob(p_left, p_right, ncol = 2L)
}


# -- Combine two ggplot panels side-by-side + formula footer ------------------

#' Arrange two ggplot objects side by side with an optional formula strip below.
#'
#' @param p1     Density panel ggplot.
#' @param p2     Q-Q panel ggplot.
#' @param fv_obj A \code{fitverse} object used to build the formula strip.
#'               Pass \code{NULL} to suppress the strip.
#' @keywords internal
.combine_panels <- function(p1, p2, fv_obj = NULL) {

  main_row <- gridExtra::arrangeGrob(p1, p2, ncol = 2L)

  if (!is.null(fv_obj)) {
    strip <- tryCatch(.make_formula_strip(fv_obj), error = function(e) NULL)
    if (!is.null(strip)) {
      combined <- gridExtra::arrangeGrob(
        main_row, strip,
        nrow    = 2L,
        heights = c(5, 1)   # main panels : footer ratio
      )
      gridExtra::grid.arrange(combined)
      return(invisible(NULL))
    }
  }

  gridExtra::grid.arrange(main_row)
  invisible(NULL)
}


# -- save_plot() ---------------------------------------------------------------

#' Save a fitverse diagnostic plot to a file
#'
#' A thin wrapper around \code{\link[ggplot2]{ggsave}()} that renders the
#' standard two-panel diagnostic plot and saves it to disk at journal-submission
#' resolution (600 DPI by default).
#'
#' @param fit        A \code{fitverse} object.
#' @param file       Output file path.  The extension determines the format
#'                   (\code{".png"}, \code{".pdf"}, \code{".svg"},
#'                   \code{".tiff"}, \code{".jpg"}).
#'                   Default: \code{"fitverse_plot.png"} in the working directory.
#' @param which      \code{"both"} (default), \code{"density"}, or \code{"qq"}.
#' @param top_n      Number of distributions to overlay.  Default 3.
#' @param xlab       X-axis label.  \code{NULL} uses \code{fit$xlab}.
#' @param width      Plot width in inches.  Default 14 (journal two-column).
#' @param height     Plot height in inches.  Default 5.5.
#' @param dpi        Resolution in dots per inch.  Default 600 (journal quality).
#' @param ...        Additional arguments passed to \code{ggplot2::ggsave()}.
#'
#' @return Invisibly, the path to the saved file.
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' x  <- rgamma(300, shape = 2, rate = 0.5)
#' fv <- fitverse(x, dists = "gamma", plot = FALSE, verbose = FALSE)
#' save_plot(fv, file = tempfile(fileext = ".png"))
#' }
save_plot <- function(fit,
                      file   = file.path(tempdir(), "fitverse_plot.png"),
                      which  = "both",
                      top_n  = 3L,
                      xlab   = NULL,
                      width  = 14,
                      height = 5.5,
                      dpi    = 600,
                      ...) {

  stopifnot(inherits(fit, "fitverse"))

  xlab_use <- if (!is.null(xlab)) xlab else if (!is.null(fit$xlab)) fit$xlab else "x"

  p_density <- .plot_density_panel(fit, top_n = top_n, xlab = xlab_use)
  p_qq      <- .plot_qq_panel(fit, xlab = xlab_use)

  if (which == "density") {
    ggplot2::ggsave(file, plot = p_density, width = width / 2,
                    height = height, dpi = dpi, bg = "white", ...)
  } else if (which == "qq") {
    ggplot2::ggsave(file, plot = p_qq, width = width / 2,
                    height = height, dpi = dpi, bg = "white", ...)
  } else {
    # Both panels + formula strip via grid.arrange on an explicit device
    strip_h <- height * 0.22
    total_h <- height + strip_h
    ext     <- tolower(tools::file_ext(file))

    if (ext == "tiff" || ext == "tif") {
      grDevices::tiff(file, width = width, height = total_h,
                      units = "in", res = dpi, compression = "lzw")
    } else if (ext == "pdf") {
      grDevices::pdf(file, width = width, height = total_h)
    } else if (ext == "svg") {
      grDevices::svg(file, width = width, height = total_h)
    } else {
      grDevices::png(file, width = width, height = total_h,
                     units = "in", res = dpi, bg = "white")
    }
    .combine_panels(p_density, p_qq, fv_obj = fit)
    grDevices::dev.off()
  }

  cat(sprintf("[FitVerse] Plot saved: %s  (%g x %g in, %g DPI)\n",
              file, width, height, dpi))
  invisible(file)
}

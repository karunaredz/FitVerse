# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : lmrd.R
# Description: L-Moment Ratio Diagram (LMRD) for distribution
#              family identification.
#
#              Draws the canonical tau3-tau4 (L-skewness vs
#              L-kurtosis) diagram with:
#                - Exact parametric curves for GEV, GPA, GLO
#                - Polynomial approximations for GNO and PE3
#                - Annotated single-point distributions
#                  (Normal, Gumbel, Logistic, Exponential, Uniform)
#                - Sample L-moment ratios overlaid as a red point
#
#              Theoretical formulas: Hosking (1990), Hosking &
#              Wallis (1997).
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


# -- Internal: exact GEV L-moment ratios --------------------------------------
# Derived from Hosking (1990) via probability-weighted moments.
# Sign convention: k < 0 gives a heavy/unbounded upper tail (Frechet type);
#                  k = 0 is the Gumbel distribution;
#                  k > 0 gives a bounded upper tail (Weibull type).
#
# tau3 = -(1 - 3*2^{-k} + 2*3^{-k}) / (1 - 2^{-k})
# tau4 = (1 - 6*2^{-k} + 10*3^{-k} - 5*4^{-k}) / (1 - 2^{-k})
# Both evaluated via L'Hopital at k = 0 (Gumbel).
.gev_lmr <- function(k_vec) {
  rows <- lapply(k_vec, function(k) {
    if (abs(k) < 1e-7)
      return(data.frame(t3 = 0.16993, t4 = 0.15043))
    p2 <- 2^(-k); p3 <- 3^(-k); p4 <- 4^(-k)
    d  <- 1 - p2
    if (abs(d) < 1e-12) return(NULL)
    data.frame(
      t3 = -(1 - 3*p2 + 2*p3) / d,
      t4 = (1 - 6*p2 + 10*p3 - 5*p4) / d
    )
  })
  do.call(rbind, rows[!sapply(rows, is.null)])
}


# -- Internal: exact GPA L-moment ratios --------------------------------------
# For the Generalised Pareto with shape k:
#   tau3 = (1 - k) / (3 + k)
#   tau4 = (1 - k)(2 - k) / [(3 + k)(4 + k)]
# k = 0 -> Exponential; k = 1 -> Uniform; k < 0 -> heavy upper tail.
.gpa_lmr <- function(k_vec) {
  rows <- lapply(k_vec, function(k) {
    d1 <- 3 + k; d2 <- 4 + k
    if (d1 <= 0 || d2 <= 0) return(NULL)
    data.frame(t3 = (1-k)/d1, t4 = (1-k)*(2-k)/(d1*d2))
  })
  do.call(rbind, rows[!sapply(rows, is.null)])
}


# -- Internal: exact GLO L-kurtosis (tau4) ------------------------------------
# For the Generalised Logistic: tau4 = (1 + 5*tau3^2) / 6  (exact).
.glo_t4 <- function(t3) (1 + 5 * t3^2) / 6


# -- Internal: GNO polynomial approximation -----------------------------------
# Generalised Normal (3-parameter lognormal).
# Polynomial fitted to tabulated values; Hosking & Wallis (1997) Table A3.
.gno_t4 <- function(t3) 0.1226 + 0.2946 * t3^2 - 0.0700 * t3^4


# -- Internal: PE3 polynomial approximation ------------------------------------
# Pearson Type III / Gamma family.
.pe3_t4 <- function(t3) 0.1226 + 0.1326 * t3^2 + 0.0030 * t3^4


# -- Internal: fallback PWM-based sample L-moment ratios ----------------------
# Used when lmomco is not available.  Based on unbiased PWM estimators.
.sample_lmr <- function(x) {
  xs <- sort(x[is.finite(x)])
  n  <- length(xs)
  if (n < 4) return(list(t3 = NA_real_, t4 = NA_real_))
  i  <- seq_len(n)

  b0 <- mean(xs)
  b1 <- sum(((i - 1) / (n - 1)) * xs) / n
  b2 <- sum(((i - 1) * (i - 2) / ((n - 1) * (n - 2))) * xs) / n
  b3 <- sum(((i - 1) * (i - 2) * (i - 3) /
               ((n - 1) * (n - 2) * (n - 3))) * xs) / n

  L2 <- 2*b1 - b0
  if (abs(L2) < 1e-14) return(list(t3 = NA_real_, t4 = NA_real_))
  L3 <- 6*b2  - 6*b1  + b0
  L4 <- 20*b3 - 30*b2 + 12*b1 - b0
  list(t3 = L3 / L2, t4 = L4 / L2)
}


# -- Public: L-Moment Ratio Diagram -------------------------------------------

#' L-Moment Ratio Diagram
#'
#' Produces a publication-quality L-moment ratio diagram showing the
#' theoretical L-kurtosis (tau4) vs L-skewness (tau3) curves for the
#' five canonical distribution families used in regional and at-site
#' frequency analysis: GLO, GEV, GNO, PE3, and GPA.
#'
#' The sample L-moment ratios computed from \code{x} are overlaid as
#' a red point, indicating which distribution families are most
#' consistent with the observed data.
#'
#' @param x A \code{fitverse} object, a numeric vector of data, or
#'   \code{NULL} (draws the theoretical diagram only, without a sample
#'   point).
#' @param xlab Character: variable label shown in the subtitle and
#'   sample-point annotation.  Defaults to \code{x$xlab} when \code{x}
#'   is a \code{fitverse} object, or \code{"sample"} otherwise.
#' @param distributions Character vector: which distribution curves to
#'   include.  Any subset of \code{c("glo","gev","gno","pe3","gpa")}.
#'   Default: all five.
#' @param show_ref_points Logical: annotate the single-distribution
#'   reference points (Normal, Gumbel, Logistic, Exponential, Uniform)?
#'   Default \code{TRUE}.
#' @param draw Logical: print the plot?  Default \code{TRUE}.  Set to
#'   \code{FALSE} to retrieve the \code{ggplot} object silently.
#' @return A \code{ggplot2} object, invisibly.
#'
#' @references
#'   Hosking, J. R. M. (1990). L-moments: analysis and estimation of
#'   distributions using linear combinations of order statistics.
#'   \emph{Journal of the Royal Statistical Society B}, \bold{52}(1),
#'   105--124.
#'
#'   Hosking, J. R. M. and Wallis, J. R. (1997).
#'   \emph{Regional Frequency Analysis: An Approach Based on
#'   L-Moments}. Cambridge University Press, Cambridge, UK.
#'
#' @examples
#' set.seed(1)
#' x <- rgamma(200, shape = 2, rate = 0.5)
#' lmrd(x, xlab = "Simulated gamma data")
#'
#' @export
lmrd <- function(x                = NULL,
                 xlab             = NULL,
                 distributions    = c("glo", "gev", "gno", "pe3", "gpa"),
                 show_ref_points  = TRUE,
                 draw             = TRUE) {

  # -- Resolve data source and variable label ----------------------------------
  data_vec  <- NULL
  sample_t3 <- NA_real_
  sample_t4 <- NA_real_

  if (inherits(x, "fitverse")) {
    data_vec <- x$data
    if (is.null(xlab))
      xlab <- if (!is.null(x$xlab) && nzchar(x$xlab)) x$xlab else "sample"
  } else if (is.numeric(x)) {
    data_vec <- x[is.finite(x)]
    if (is.null(xlab)) xlab <- "sample"
  } else {
    if (is.null(xlab)) xlab <- ""
  }

  # -- Compute sample L-moment ratios -----------------------------------------
  if (!is.null(data_vec) && length(data_vec) >= 4) {
    lm_vals <- if (has_pkg("lmomco", "L-moment ratio diagram")) {
      lm_obj <- tryCatch(lmomco::lmoms(data_vec, nmom = 4), error = function(e) NULL)
      if (!is.null(lm_obj) && !is.null(lm_obj$ratios))
        list(t3 = lm_obj$ratios[3], t4 = lm_obj$ratios[4])
      else
        .sample_lmr(data_vec)
    } else {
      .sample_lmr(data_vec)
    }
    sample_t3 <- lm_vals$t3
    sample_t4 <- lm_vals$t4
  }

  # -- Colour and linetype palette ---------------------------------------------
  .cols <- c(glo = "#185FA5", gev = "#D85A30",
             gno = "#0F6E56", pe3 = "#534AB7", gpa = "#BA7517")
  .labs <- c(glo = "GLO - Generalised Logistic",
             gev = "GEV - Generalised Extreme Value",
             gno = "GNO - Generalised Normal",
             pe3 = "PE3 - Pearson Type III",
             gpa = "GPA - Generalised Pareto")
  .lty  <- c(glo = "solid", gev = "solid",
             gno = "solid", pe3 = "solid", gpa = "longdash")

  distributions <- intersect(tolower(distributions), names(.cols))

  # -- Theoretical curve data --------------------------------------------------
  curve_list <- list()

  if ("glo" %in% distributions) {
    t3v <- seq(-0.22, 0.88, length.out = 200)
    curve_list[["glo"]] <- data.frame(t3 = t3v, t4 = .glo_t4(t3v), dist = "glo")
  }

  if ("gev" %in% distributions) {
    kv  <- seq(-0.50, 0.42, length.out = 200)
    tmp <- .gev_lmr(kv)
    keep <- tmp$t3 >= -0.21 & tmp$t3 <= 0.94 &
            tmp$t4 >=  0.00 & tmp$t4 <= 0.44
    tmp  <- tmp[keep, ]
    curve_list[["gev"]] <- data.frame(
      t3 = tmp$t3[order(tmp$t3)],
      t4 = tmp$t4[order(tmp$t3)],
      dist = "gev"
    )
  }

  if ("gno" %in% distributions) {
    t3v <- seq(-0.82, 0.82, length.out = 200)
    curve_list[["gno"]] <- data.frame(t3 = t3v, t4 = .gno_t4(t3v), dist = "gno")
  }

  if ("pe3" %in% distributions) {
    t3v <- seq(-0.82, 0.82, length.out = 200)
    curve_list[["pe3"]] <- data.frame(t3 = t3v, t4 = .pe3_t4(t3v), dist = "pe3")
  }

  if ("gpa" %in% distributions) {
    kv  <- seq(1.04, -0.87, length.out = 200)
    tmp <- .gpa_lmr(kv)
    keep <- tmp$t3 >= -0.03 & tmp$t3 <= 0.94 &
            tmp$t4 >=  0.00 & tmp$t4 <= 0.44
    tmp  <- tmp[keep, ]
    curve_list[["gpa"]] <- data.frame(
      t3 = tmp$t3[order(tmp$t3)],
      t4 = tmp$t4[order(tmp$t3)],
      dist = "gpa"
    )
  }

  all_curves      <- do.call(rbind, curve_list)
  all_curves$dist <- factor(all_curves$dist, levels = names(.cols))

  # -- Reference single-distribution points -----------------------------------
  ref_df <- data.frame(
    t3    = c(0,      0,      0.1699, 1/3,          0),
    t4    = c(0.1226, 0.1667, 0.1504, 1/6,          0),
    label = c("Normal", "Logistic", "Gumbel",
              "Exponential", "Uniform"),
    stringsAsFactors = FALSE
  )

  # -- Sample point data frame -------------------------------------------------
  has_sample <- !is.na(sample_t3) && !is.na(sample_t4)
  samp_df <- if (has_sample)
    data.frame(t3 = sample_t3, t4 = sample_t4,
               label = if (nzchar(xlab)) xlab else "sample")
  else NULL

  # -- Subtitle ---------------------------------------------------------------
  subtitle_str <- if (has_sample) {
    sprintf(
      "%s . tau3 = %.4f . tau4 = %.4f",
      xlab, sample_t3, sample_t4
    )
  } else {
    "Theoretical distribution families"
  }

  # -- Build ggplot ------------------------------------------------------------
  p <- ggplot2::ggplot() +

    # Theoretical curves
    ggplot2::geom_path(
      data      = all_curves,
      ggplot2::aes(x = t3, y = t4, colour = dist, linetype = dist),
      linewidth = 0.85,
      lineend   = "round",
      linejoin  = "round"
    ) +

    ggplot2::scale_colour_manual(
      name   = NULL,
      values = .cols[distributions],
      labels = .labs[distributions],
      breaks = distributions
    ) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = .lty[distributions],
      labels = .labs[distributions],
      breaks = distributions
    ) +

    # Reference points
    { if (show_ref_points)
        ggplot2::geom_point(
          data   = ref_df,
          ggplot2::aes(x = t3, y = t4),
          shape  = 21, size = 2.6,
          colour = "#5F5E5A", fill = "white", stroke = 0.9
        )
    } +
    { if (show_ref_points)
        ggplot2::geom_text(
          data      = ref_df,
          ggplot2::aes(x = t3, y = t4, label = label),
          hjust     = -0.15, vjust = 1.5,
          nudge_y   = 0.018,
          size      = 2.9, colour = "#5F5E5A"
        )
    } +

    # Sample point
    { if (has_sample)
        ggplot2::geom_point(
          data   = samp_df,
          ggplot2::aes(x = t3, y = t4),
          shape  = 21, size = 4.2,
          colour = "#E63946", fill = "#E63946",
          stroke = 1.2, alpha = 0.92
        )
    } +
    { if (has_sample)
        ggplot2::geom_text(
          data     = samp_df,
          ggplot2::aes(x = t3, y = t4, label = label),
          hjust    = -0.18, vjust = 0.4,
          size     = 3.2, colour = "#E63946",
          fontface = "bold"
        )
    } +

    # End-of-curve abbreviated labels so overlapping curves are identifiable
    lapply(intersect(distributions, names(curve_list)), function(d) {
      df_d <- curve_list[[d]]
      last <- df_d[which.max(df_d$t3), ]
      abbr <- sub(" .*", "", .labs[[d]])   # "GLO", "GEV", "GNO", "PE3", "GPA"
      ggplot2::annotate(
        "text",
        x        = last$t3,
        y        = last$t4,
        label    = abbr,
        colour   = .cols[[d]],
        hjust    = -0.14,
        vjust    = 0.4,
        size     = 3.1,
        fontface = "bold"
      )
    }) +

    # Axes and labels
    ggplot2::scale_x_continuous(
      name   = "L-skewness (tau3)",
      limits = c(-0.22, 1.06),
      breaks = seq(-0.2, 1.0, by = 0.1),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      name   = "L-kurtosis (tau4)",
      limits = c(-0.01, 0.44),
      breaks = seq(0, 0.40, by = 0.05),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +

    ggplot2::labs(
      title   = "L-Moment Ratio Diagram",
      subtitle = subtitle_str,
      caption = paste0(
        "Exact curves: GLO, GEV (Hosking 1990), GPA. ",
        "Polynomial approximations: GNO, PE3 (Hosking & Wallis 1997). ",
        "Red point = sample (tau3, tau4)."
      )
    ) +

    # Theme - mirrors plot_fitverse.R density / QQ style
    ggplot2::theme_light(base_size = 13) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      panel.grid.major   = ggplot2::element_line(colour = "#D3DCE6", linewidth = 0.40),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_rect(fill = NA, colour = "#B8C8D8",
                                                  linewidth = 0.55),
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title         = ggplot2::element_text(face = "bold", size = 14,
                                                  colour = "#1A2B3C",
                                                  margin = ggplot2::margin(b = 3)),
      plot.subtitle      = ggplot2::element_text(colour = "#4A6278", size = 10.5,
                                                  margin = ggplot2::margin(b = 8)),
      plot.caption       = ggplot2::element_text(colour = "#7090A8", size = 8.5,
                                                  hjust = 0, face = "italic",
                                                  margin = ggplot2::margin(t = 7)),
      axis.title         = ggplot2::element_text(size = 11.5, colour = "#1A2B3C"),
      axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 7, b = 14)),
      axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
      axis.text          = ggplot2::element_text(size = 10.5, colour = "#354A5E"),
      axis.ticks         = ggplot2::element_line(colour = "#B0C0D0", linewidth = 0.4),
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.background  = ggplot2::element_blank(),
      legend.key         = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = 10, colour = "#1A2B3C"),
      legend.margin      = ggplot2::margin(10, 0, 2, 0),
      legend.key.width   = ggplot2::unit(1.8, "cm"),
      legend.key.height  = ggplot2::unit(0.5, "cm"),
      legend.spacing.x   = ggplot2::unit(0.3, "cm"),
      plot.margin        = ggplot2::margin(12, 16, 8, 12)
    ) +
    ggplot2::guides(
      # colour and linetype guides share the same labels so ggplot2 merges
      # them into a single legend.  Specify override.aes only on the colour
      # guide to avoid the "Duplicated override.aes is ignored" warning.
      colour   = ggplot2::guide_legend(
        nrow = 2,
        override.aes = list(linewidth = 1.6)
      ),
      linetype = ggplot2::guide_legend(nrow = 2)
    )

  if (draw) print(p)
  invisible(p)
}

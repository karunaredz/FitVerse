# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : extreme_value.R
# Description: Threshold selection tools for extreme value analysis.
#
#              ev_threshold() produces a three-panel diagnostic figure:
#                Panel 1 - Mean Residual Life (mean excess) plot with
#                          pointwise 95% confidence band.
#                Panel 2 - GPD shape parameter (xi) stability plot.
#                Panel 3 - GPD modified scale parameter (sigma*) stability
#                          plot.  sigma* = sigma - xi*u is invariant to
#                          threshold when the GPD assumption holds.
#
#              The threshold u above which GPD is appropriate is identified
#              as the lowest value at which:
#                (a) the MRL plot is approximately linear, and
#                (b) the xi and sigma* stability plots are approximately
#                    constant.
#
#              References:
#                Davison, A. C. & Smith, R. L. (1990). Models for
#                exceedances over high thresholds.
#                J. Royal Statistical Society B, 52(3), 393-442.
#
#                Coles, S. (2001). An Introduction to Statistical Modeling
#                of Extreme Values. Springer, London.
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


# -- Shared theme for EV plots - mirrors plot_fitverse.R density / QQ style ---
.ev_theme <- function() {
  ggplot2::theme_light(base_size = 13) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = "#D3DCE6", linewidth = 0.40),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "#B8C8D8",
                                                 linewidth = 0.55),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title        = ggplot2::element_text(face = "bold", size = 14,
                                                 colour = "#1A2B3C",
                                                 margin = ggplot2::margin(b = 3)),
      plot.subtitle     = ggplot2::element_text(colour = "#4A6278", size = 10.5,
                                                 margin = ggplot2::margin(b = 8)),
      plot.caption      = ggplot2::element_text(colour = "#7090A8", size = 8.5,
                                                 hjust = 0, face = "italic",
                                                 margin = ggplot2::margin(t = 7)),
      axis.title        = ggplot2::element_text(size = 11.5, colour = "#1A2B3C"),
      axis.title.x      = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.title.y      = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
      axis.text         = ggplot2::element_text(size = 10.5, colour = "#354A5E"),
      axis.ticks        = ggplot2::element_line(colour = "#B0C0D0", linewidth = 0.4),
      plot.margin       = ggplot2::margin(12, 16, 8, 12)
    )
}


#' @export
#' @noRd
ev_threshold <- function(x,
                         thresholds      = NULL,
                         n_thresholds    = 40L,
                         min_exceedances = 10L,
                         conf            = 0.95,
                         xlab            = "x",
                         draw            = TRUE) {

  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n < 2 * min_exceedances)
    stop(sprintf(
      "Need at least %d observations for threshold analysis (have %d).",
      2L * min_exceedances, n
    ))

  # -- Threshold grid ----------------------------------------------------------
  if (is.null(thresholds)) {
    u_min <- as.numeric(quantile(x, 0.05))
    u_max <- x[n - min_exceedances]        # at u_max exactly min_exc obs exceed
    if (u_min >= u_max)
      stop("Cannot construct a valid threshold grid. ",
           "Try reducing min_exceedances.")
    thresholds <- seq(u_min, u_max, length.out = n_thresholds)
  }

  z_crit <- qnorm(1 - (1 - conf) / 2)

  # -- Panel 1: Mean Residual Life data -----------------------------------------
  mrl_rows <- lapply(thresholds, function(u) {
    exc <- x[x > u] - u
    nu  <- length(exc)
    if (nu < min_exceedances) return(NULL)
    me  <- mean(exc)
    se  <- sd(exc) / sqrt(nu)
    data.frame(
      threshold   = u,
      mean_excess = me,
      lower       = me - z_crit * se,
      upper       = me + z_crit * se,
      n_exc       = nu
    )
  })
  mrl_df <- do.call(rbind, mrl_rows[!sapply(mrl_rows, is.null)])

  # -- Panel 2 & 3: GPD stability data ------------------------------------------
  if (!has_pkg("evd", "GPD stability plots")) {
    message("[FitVerse] evd not available - skipping GPD stability panels.")
    gpd_df <- NULL
  } else {
    gpd_rows <- lapply(thresholds, function(u) {
      tryCatch({
        exc <- x[x > u] - u
        nu  <- length(exc)
        if (nu < min_exceedances) return(NULL)

        fit <- suppressWarnings(
          evd::fpot(x, threshold = u, model = "gpd", std.err = TRUE)
        )
        if (is.null(fit)) return(NULL)

        est <- fit$estimate
        # accept either "shape"/"scale" (evd convention) or "xi"/"sigma"
        xi_nm    <- intersect(names(est), c("shape", "xi"))[1]
        sigma_nm <- intersect(names(est), c("scale", "sigma"))[1]
        if (is.na(xi_nm) || is.na(sigma_nm)) return(NULL)

        xi    <- est[[xi_nm]]
        sigma <- est[[sigma_nm]]
        sigma_star <- sigma - xi * u     # reparameterised scale (Coles 2001)

        # CI via delta method using the var-cov matrix
        vc <- fit$var.cov
        if (!is.null(vc) &&
            !is.null(rownames(vc)) &&
            all(c(xi_nm, sigma_nm) %in% rownames(vc))) {
          var_xi    <- vc[xi_nm,    xi_nm]
          var_sigma <- vc[sigma_nm, sigma_nm]
          cov_xs    <- vc[sigma_nm, xi_nm]
          var_ss    <- var_sigma + u^2 * var_xi - 2 * u * cov_xs
          se_xi     <- sqrt(max(0, var_xi))
          se_ss     <- sqrt(max(0, var_ss))
        } else {
          se_vec <- fit$std.err
          se_xi  <- if (!is.null(se_vec) && xi_nm    %in% names(se_vec)) se_vec[[xi_nm]]    else NA_real_
          se_ss  <- if (!is.null(se_vec) && sigma_nm %in% names(se_vec)) se_vec[[sigma_nm]] else NA_real_
        }

        data.frame(
          threshold  = u,
          xi         = xi,
          sigma_star = sigma_star,
          xi_lo      = xi         - z_crit * se_xi,
          xi_hi      = xi         + z_crit * se_xi,
          ss_lo      = sigma_star - z_crit * se_ss,
          ss_hi      = sigma_star + z_crit * se_ss,
          n_exc      = nu
        )
      }, error = function(e) NULL)
    })
    gpd_df <- do.call(rbind, gpd_rows[!sapply(gpd_rows, is.null)])
    if (!is.null(gpd_df) && nrow(gpd_df) == 0) gpd_df <- NULL
  }

  # -- Build Panel 1: MRL plot ---------------------------------------------------
  p_mrl <- ggplot2::ggplot(mrl_df, ggplot2::aes(x = threshold)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      fill = "#AED6F1", alpha = 0.45
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_excess),
      colour = "#0D3B66", linewidth = 0.85
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = mean_excess),
      colour = "#0D3B66", size = 1.5, shape = 16
    ) +
    ggplot2::labs(
      title    = "Mean Residual Life Plot",
      subtitle = paste0("Choose threshold where the plot becomes approximately linear"),
      x        = paste0("Threshold  (", xlab, ")"),
      y        = "Mean excess  e(u)",
      caption  = paste0(sprintf("%d%% CI shown.", round(conf * 100)),
                        "  Ref: Coles (2001), Ch. 4.")
    ) +
    .ev_theme()

  # -- Build Panel 2: shape stability -------------------------------------------
  if (!is.null(gpd_df)) {
    p_shape <- ggplot2::ggplot(gpd_df, ggplot2::aes(x = threshold)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = xi_lo, ymax = xi_hi),
        fill = "#F0997B", alpha = 0.35
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = xi),
        colour = "#D85A30", linewidth = 0.85
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = xi),
        colour = "#D85A30", size = 1.5, shape = 16
      ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "dotted",
        colour = "#718096", linewidth = 0.55
      ) +
      ggplot2::labs(
        title    = "GPD Shape (xi) Stability",
        subtitle = "Should be approximately constant above the true threshold",
        x        = paste0("Threshold  (", xlab, ")"),
        y        = "Shape parameter  xi",
        caption  = paste0(sprintf("%d%% CI (delta method).  ", round(conf * 100)),
                          "xi > 0: heavy tail;  xi < 0: bounded tail.")
      ) +
      .ev_theme()
  } else {
    p_shape <- NULL
  }

  # -- Build Panel 3: modified scale stability -----------------------------------
  if (!is.null(gpd_df)) {
    p_scale <- ggplot2::ggplot(gpd_df, ggplot2::aes(x = threshold)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ss_lo, ymax = ss_hi),
        fill = "#9FE1CB", alpha = 0.35
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = sigma_star),
        colour = "#0F6E56", linewidth = 0.85
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = sigma_star),
        colour = "#0F6E56", size = 1.5, shape = 16
      ) +
      ggplot2::labs(
        title    = "GPD Modified Scale (sigma*) Stability",
        subtitle = expression(paste(sigma^"*", " = ", sigma, " - ", xi, "u  -  should be constant above the true threshold")),
        x        = paste0("Threshold  (", xlab, ")"),
        y        = "Modified scale  sigma* = sigma - xiu",
        caption  = paste0(sprintf("%d%% CI (delta method).  ", round(conf * 100)),
                          "Ref: Davison & Smith (1990).")
      ) +
      .ev_theme()
  } else {
    p_scale <- NULL
  }

  # -- Combine and draw ----------------------------------------------------------
  if (draw) {
    panels <- Filter(Negate(is.null), list(p_mrl, p_shape, p_scale))
    np     <- length(panels)
    gridExtra::grid.arrange(
      grobs  = lapply(panels, ggplot2::ggplotGrob),
      ncol   = 1,
      heights = rep(1, np)
    )
  }

  invisible(list(
    mrl     = mrl_df,
    gpd     = gpd_df,
    p_mrl   = p_mrl,
    p_shape = p_shape,
    p_scale = p_scale
  ))
}

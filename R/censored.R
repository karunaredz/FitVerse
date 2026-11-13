# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : censored.R
# Description: Maximum-likelihood fitting for censored and
#              truncated data.
#
#              fitverse_censored() fits parametric distributions
#              to data that is:
#                * Right-censored  (survival, reliability)
#                * Left-censored   (below-detection-limit)
#                * Interval-censored
#                * Left- and/or right-truncated
#              or any combination of the above.
#
#              Seven distributions are supported:
#                Normal, Log-Normal, Weibull, Exponential,
#                Gamma, Log-Logistic, Gumbel.
#
#              Status coding (matches survival::Surv convention):
#                1 = completely observed
#                0 = right-censored
#                2 = left-censored
#                3 = interval-censored  (requires x_upper)
#
#              References:
#                Meeker, W. Q. & Escobar, L. A. (1998).
#                Statistical Methods for Reliability Data.
#                Wiley, New York.
#
#                Lawless, J. F. (2003). Statistical Models and
#                Methods for Lifetime Data, 2nd ed.
#                Wiley, Hoboken.
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
# Date    : 2026-06-24
# License : GPL (>= 3)
# ============================================================


# -- Internal: distribution registry -------------------------------------------
# Each entry supplies d/p functions, free-parameter transforms, and a smart
# initialiser.  All optimisation is done in unconstrained "free" space; the
# from_free() transform maps back to natural parameters.

.CENS_DISTS <- list(

  normal = list(
    display       = "Normal",
    n_par         = 2L,
    par_names     = c("mean", "sd"),
    positive_only = FALSE,
    dfun = function(x, p, log = FALSE) dnorm(x, p[1], p[2], log = log),
    pfun = function(x, p)             pnorm(x, p[1], p[2]),
    to_free   = function(p) c(p[1], log(p[2])),
    from_free = function(f) c(f[1], exp(f[2])),
    init = function(x, st) {
      xo <- x[st == 1L]; if (length(xo) < 2L) xo <- x
      c(mean(xo), max(sd(xo), 1e-4))
    }
  ),

  lognormal = list(
    display       = "Log-Normal",
    n_par         = 2L,
    par_names     = c("meanlog", "sdlog"),
    positive_only = TRUE,
    dfun = function(x, p, log = FALSE) dlnorm(x, p[1], p[2], log = log),
    pfun = function(x, p)             plnorm(x, p[1], p[2]),
    to_free   = function(p) c(p[1], log(p[2])),
    from_free = function(f) c(f[1], exp(f[2])),
    init = function(x, st) {
      xo <- x[st == 1L & x > 0]; if (length(xo) < 2L) xo <- x[x > 0]
      lx <- log(xo); c(mean(lx), max(sd(lx), 1e-4))
    }
  ),

  weibull = list(
    display       = "Weibull",
    n_par         = 2L,
    par_names     = c("shape", "scale"),
    positive_only = TRUE,
    dfun = function(x, p, log = FALSE) dweibull(x, p[1], p[2], log = log),
    pfun = function(x, p)             pweibull(x, p[1], p[2]),
    to_free   = function(p) log(p),
    from_free = function(f) exp(f),
    init = function(x, st) {
      xo <- x[st == 1L & x > 0]; if (length(xo) < 2L) xo <- x[x > 0]
      c(1.2, mean(xo))
    }
  ),

  exponential = list(
    display       = "Exponential",
    n_par         = 1L,
    par_names     = "rate",
    positive_only = TRUE,
    dfun = function(x, p, log = FALSE) dexp(x, p[1], log = log),
    pfun = function(x, p)             pexp(x, p[1]),
    to_free   = function(p) log(p),
    from_free = function(f) exp(f),
    init = function(x, st) {
      xo <- x[st == 1L & x > 0]; if (length(xo) < 1L) xo <- x[x > 0]
      1 / mean(xo)
    }
  ),

  gamma = list(
    display       = "Gamma",
    n_par         = 2L,
    par_names     = c("shape", "rate"),
    positive_only = TRUE,
    dfun = function(x, p, log = FALSE) dgamma(x, p[1], p[2], log = log),
    pfun = function(x, p)             pgamma(x, p[1], p[2]),
    to_free   = function(p) log(p),
    from_free = function(f) exp(f),
    init = function(x, st) {
      xo <- x[st == 1L & x > 0]; if (length(xo) < 2L) xo <- x[x > 0]
      m <- mean(xo); v <- max(var(xo), 1e-8)
      c(m^2 / v, m / v)
    }
  ),

  # Log-Logistic via the logistic-log link (no actuar dependency):
  #   X ~ LogLogistic(shape=beta, scale=alpha)
  #   <=> log(X) ~ Logistic(loc=log(alpha), scale=1/beta)
  loglogistic = list(
    display       = "Log-Logistic",
    n_par         = 2L,
    par_names     = c("shape", "scale"),
    positive_only = TRUE,
    dfun = function(x, p, log = FALSE) {
      out <- rep(if (log) -Inf else 0, length(x))
      ok  <- is.finite(x) & x > 0
      if (any(ok)) {
        lx       <- base::log(x[ok])
        ld_ok    <- dlogis(lx, location = base::log(p[2]), scale = 1/p[1]) - lx
        out[ok]  <- if (log) ld_ok else exp(ld_ok)
      }
      out
    },
    pfun = function(x, p) {
      out <- rep(0, length(x))
      ok  <- is.finite(x) & x > 0
      if (any(ok))
        out[ok] <- plogis(base::log(x[ok]),
                          location = base::log(p[2]), scale = 1/p[1])
      out
    },
    # Parameterise in log-space; cap log(scale) at log(max(data)*100) via
    # a soft quadratic penalty to prevent scale->Inf degenerate solutions.
    to_free   = function(p) log(p),
    from_free = function(f) exp(f),
    init = function(x, st) {
      # Use ALL positive data (not just events) for the scale estimate, and
      # log-linear regression to get a sensible shape starting value.
      xp <- x[x > 0]
      if (length(xp) < 2L) return(c(1.5, 1))
      # Method-of-moments on log(x): log(X) ~ Logistic(loc, scale=1/beta)
      # Logistic variance = pi^2/(3 * beta^2)
      lx  <- log(xp)
      vl  <- max(var(lx), 1e-8)
      beta_mm <- pi / sqrt(3 * vl)          # shape from variance of log-data
      alpha_mm <- exp(mean(lx))             # scale from mean of log-data
      c(max(beta_mm, 0.5), alpha_mm)
    }
  ),

  gumbel = list(
    display       = "Gumbel",
    n_par         = 2L,
    par_names     = c("loc", "scale"),
    positive_only = FALSE,
    dfun = function(x, p, log = FALSE) evd::dgumbel(x, loc = p[1], scale = p[2], log = log),
    pfun = function(x, p)             evd::pgumbel(x, loc = p[1], scale = p[2]),
    to_free   = function(p) c(p[1], log(p[2])),
    from_free = function(f) c(f[1], exp(f[2])),
    init = function(x, st) {
      xo <- x[st == 1L]; if (length(xo) < 2L) xo <- x
      s <- max(sd(xo) * sqrt(6) / pi, 1e-4)
      c(mean(xo) - 0.5772 * s, s)
    }
  )
)


# -- Internal: censored negative log-likelihood --------------------------------
.cens_nll <- function(free, x, status, x_upper, tl, tu, entry) {
  par <- entry$from_free(free)

  suppressWarnings({

    # Truncation normalising constant (log scale)
    log_C <- if (is.finite(tl) || is.finite(tu)) {
      lC <- log(max(entry$pfun(tu, par) - entry$pfun(tl, par), .Machine$double.eps))
      if (!is.finite(lC)) return(1e10)
      lC
    } else 0

    ll <- 0

    # Completely observed
    idx <- (status == 1L)
    if (any(idx)) {
      ld <- entry$dfun(x[idx], par, log = TRUE)
      ld[!is.finite(ld)] <- -700
      ll <- ll + sum(ld) - sum(idx) * log_C
    }

    # Right-censored: contribution = log S(c) = log(1 - F(c))
    idx <- (status == 0L)
    if (any(idx)) {
      sp <- log(pmax(1 - entry$pfun(x[idx], par), .Machine$double.eps))
      sp[!is.finite(sp)] <- -700
      ll <- ll + sum(sp) - sum(idx) * log_C
    }

    # Left-censored: contribution = log F(c)
    idx <- (status == 2L)
    if (any(idx)) {
      fp <- log(pmax(entry$pfun(x[idx], par), .Machine$double.eps))
      fp[!is.finite(fp)] <- -700
      ll <- ll + sum(fp) - sum(idx) * log_C
    }

    # Interval-censored: contribution = log(F(u) - F(l))
    idx <- (status == 3L)
    if (any(idx)) {
      ip <- log(pmax(entry$pfun(x_upper[idx], par) - entry$pfun(x[idx], par),
                     .Machine$double.eps))
      ip[!is.finite(ip)] <- -700
      ll <- ll + sum(ip) - sum(idx) * log_C
    }

    if (!is.finite(ll)) return(1e10)

    # Soft penalty: if any free parameter implies a scale > 1000 * max(|x|),
    # add a quadratic penalty proportional to the excess.  Prevents degenerate
    # solutions (scale->Inf) without biasing the MLE in the feasible region.
    max_x   <- max(abs(x), na.rm = TRUE)
    penalty <- 0
    for (fi in free) {
      excess <- fi - log(max_x * 1000)
      if (excess > 0) penalty <- penalty + 1e4 * excess^2
    }

    -(ll) + penalty

  })
}


# -- Internal: fit one distribution --------------------------------------------
.fit_one_cens <- function(d_key, x, status, x_upper, tl, tu, n_starts) {
  entry <- .CENS_DISTS[[d_key]]

  # Skip positive-only distributions if data contains non-positive observed values
  if (entry$positive_only && any(x[status == 1L] <= 0, na.rm = TRUE))
    return(NULL)

  best <- NULL

  for (attempt in seq_len(n_starts)) {
    # Starting parameters - perturb after the first attempt
    p0 <- tryCatch(entry$init(x, status), error = function(e) NULL)
    if (is.null(p0) || !all(is.finite(p0))) next
    if (attempt > 1L) p0 <- p0 * exp(rnorm(length(p0), 0, 0.5 * attempt))

    free0 <- tryCatch(entry$to_free(p0), error = function(e) NULL)
    if (is.null(free0) || !all(is.finite(free0))) next

    opt <- tryCatch(
      optim(
        par     = free0,
        fn      = .cens_nll,
        method  = "BFGS",
        control = list(maxit = 2000L, reltol = 1e-10),
        x       = x, status = status, x_upper = x_upper,
        tl = tl, tu = tu, entry = entry
      ),
      error = function(e) NULL
    )
    if (is.null(opt) || opt$convergence > 1L) next
    if (is.null(best) || opt$value < best$value) best <- opt
  }

  if (is.null(best)) return(NULL)

  par_hat  <- entry$from_free(best$par)
  log_lik  <- -best$value
  n_par    <- entry$n_par
  n_obs    <- length(x)
  aic      <- 2 * n_par - 2 * log_lik
  bic      <- n_par * log(n_obs) - 2 * log_lik

  list(
    dist_key  = d_key,
    display   = entry$display,
    par       = setNames(par_hat, entry$par_names),
    log_lik   = log_lik,
    aic       = aic,
    bic       = bic,
    n_par     = n_par,
    converged = (best$convergence == 0L)
  )
}


# -- Internal: Kaplan-Meier estimator (no survival package needed) -------------
# Returns data frame with (time, surv, lower, upper) for right-censored data.
.kaplan_meier <- function(x, status) {
  # status: 1=event, 0=right-censored
  ord     <- order(x)
  x_s     <- x[ord]
  st_s    <- status[ord]
  n       <- length(x_s)

  ev_times <- sort(unique(x_s[st_s == 1L]))

  # No observed events -> return a two-point frame so geom_step has >= 2 rows
  if (length(ev_times) == 0L) {
    return(data.frame(time  = c(min(x_s), max(x_s)),
                      surv  = c(1, 1),
                      lower = c(1, 1),
                      upper = c(1, 1)))
  }

  S        <- 1
  d_var    <- 0    # Greenwood accumulator for log-log CI

  rows <- vector("list", length(ev_times) + 1L)
  rows[[1L]] <- data.frame(time = min(x_s), surv = 1, lower = 1, upper = 1)

  for (i in seq_along(ev_times)) {
    t       <- ev_times[i]
    n_risk  <- sum(x_s >= t)
    n_ev    <- sum(x_s == t & st_s == 1L)
    S       <- S * (1 - n_ev / n_risk)
    d_var   <- d_var + n_ev / (n_risk * (n_risk - n_ev + 1e-10))

    # Log-log transformed CI (Greenwood's formula)
    if (S > 0 && S < 1) {
      log_log_S <- log(-log(S))
      se_ll     <- sqrt(d_var / (log(S))^2)
      ci_lo     <- exp(-exp(log_log_S + 1.96 * se_ll))
      ci_hi     <- exp(-exp(log_log_S - 1.96 * se_ll))
    } else {
      ci_lo <- ci_hi <- S
    }

    rows[[i + 1L]] <- data.frame(time = t, surv = S,
                                 lower = ci_lo, upper = ci_hi)
  }

  do.call(rbind, rows)
}


# -- Public: plot method -------------------------------------------------------
#' @export
plot.fitverse_censored <- function(x, top_n = 3L, ...) {
  obj <- x
  has_right <- any(obj$status == 0L)
  has_left  <- any(obj$status == 2L)
  show_surv <- has_right && !has_left   # pure right-censored: survival plot

  fits   <- obj$fits
  n_show <- min(top_n, length(fits))
  col_pal <- c("#D85A30", "#185FA5", "#0F6E56", "#534AB7", "#BA7517",
               "#C2185B", "#00796B")

  xrng <- range(obj$x, na.rm = TRUE)
  xseq <- seq(xrng[1] * 0.98, xrng[2] * 1.02, length.out = 300)

  curve_df <- do.call(rbind, lapply(seq_len(n_show), function(i) {
    fit   <- fits[[i]]
    entry <- .CENS_DISTS[[fit$dist_key]]
    cdf   <- entry$pfun(xseq, fit$par)
    data.frame(
      x    = xseq,
      y    = if (show_surv) 1 - cdf else cdf,
      dist = fit$display,
      stringsAsFactors = FALSE
    )
  }))
  curve_df$dist <- factor(curve_df$dist, levels = unique(curve_df$dist))
  pal <- setNames(col_pal[seq_len(n_show)], levels(curve_df$dist))

  p <- ggplot2::ggplot()

  # Empirical layer
  if (show_surv) {
    km_df <- .kaplan_meier(obj$x, obj$status)
    p <- p +
      ggplot2::geom_ribbon(data = km_df,
                           ggplot2::aes(x = time, ymin = lower, ymax = upper,
                                        group = 1L),
                           fill = "#9ECAE1", alpha = 0.35, colour = NA) +
      ggplot2::geom_step(data = km_df,
                         ggplot2::aes(x = time, y = surv, group = 1L),
                         colour = "#2171B5", linewidth = 0.9,
                         direction = "hv") +
      ggplot2::annotate("text", x = min(km_df$time), y = 0.04,
                        label = "Step: Kaplan-Meier  |  Band: 95% CI",
                        hjust = 0, size = 2.9, colour = "#2171B5",
                        fontface = "italic")
  } else {
    # Empirical CDF of observed values
    xo  <- sort(obj$x[obj$status == 1L])
    if (length(xo) < 2L) xo <- sort(obj$x)   # fallback: use all values
    eco     <- seq_along(xo) / (length(xo) + 1L)
    ecdf_df <- data.frame(x = xo, ecdf = eco)
    p <- p +
      ggplot2::geom_step(data = ecdf_df,
                         ggplot2::aes(x = x, y = ecdf, group = 1L),
                         colour = "#2171B5", linewidth = 0.9,
                         direction = "hv") +
      ggplot2::annotate("text", x = min(xo), y = 0.04,
                        label = "Step: Empirical CDF (detected values only)",
                        hjust = 0, size = 2.9, colour = "#2171B5",
                        fontface = "italic")
  }

  # Parametric fitted curves
  p <- p +
    ggplot2::geom_line(data = curve_df,
                       ggplot2::aes(x = x, y = y,
                                    colour = dist, linetype = dist,
                                    group  = dist),
                       linewidth = 0.9) +
    ggplot2::scale_colour_manual(name = NULL, values = pal) +
    ggplot2::scale_linetype_manual(name = NULL,
                                   values = setNames(
                                     c("solid","dashed","dotdash")[seq_len(n_show)],
                                     levels(curve_df$dist))) +
    ggplot2::labs(
      title    = if (show_surv) "Survival Function" else "Cumulative Distribution",
      subtitle = paste0(obj$xlab,
                        "  |  n = ", length(obj$x),
                        "  |  censored: ", sum(obj$status != 1L)),
      x        = obj$xlab,
      y        = if (show_surv) "Survival probability  S(t)" else "Cumulative probability  F(x)",
      caption  = paste0("Top ", n_show, " distributions by ", obj$criterion,
                        ".  MLE with censored likelihood.")
    ) +
    ggplot2::theme_light(base_size = 13) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "#EDF2F7", colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = "#D3DCE6", linewidth = 0.40),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_rect(fill = NA, colour = "#B8C8D8",
                                                 linewidth = 0.55),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title        = ggplot2::element_text(face = "bold", size = 14,
                                                 colour = "#1A2B3C"),
      plot.subtitle     = ggplot2::element_text(colour = "#4A6278", size = 10.5),
      plot.caption      = ggplot2::element_text(colour = "#7090A8", size = 8.5,
                                                 hjust = 0, face = "italic"),
      axis.title        = ggplot2::element_text(size = 11.5, colour = "#1A2B3C"),
      axis.text         = ggplot2::element_text(size = 10.5, colour = "#354A5E"),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.background = ggplot2::element_blank(),
      legend.title      = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(size = 10),
      plot.margin       = ggplot2::margin(12, 16, 8, 12)
    )

  p
}


# -- Public: print method ------------------------------------------------------
#' @export
print.fitverse_censored <- function(x, ...) {
  obj <- x
  n_obs   <- length(obj$x)
  n_cens  <- sum(obj$status != 1L)
  n_right <- sum(obj$status == 0L)
  n_left  <- sum(obj$status == 2L)
  n_int   <- sum(obj$status == 3L)

  cat("\n-- FitVerse Censored / Truncated Fit --------------------\n")
  cat(sprintf("  Variable   : %s\n", obj$xlab))
  cat(sprintf("  n total    : %d   (observed: %d",  n_obs, n_obs - n_cens))
  if (n_right > 0) cat(sprintf("  right-censored: %d", n_right))
  if (n_left  > 0) cat(sprintf("  left-censored: %d",  n_left))
  if (n_int   > 0) cat(sprintf("  interval-censored: %d", n_int))
  cat(")\n")
  if (is.finite(obj$trunc_lower) || is.finite(obj$trunc_upper))
    cat(sprintf("  Truncation : [%s, %s]\n",
                if (is.finite(obj$trunc_lower)) obj$trunc_lower else "-Inf",
                if (is.finite(obj$trunc_upper)) obj$trunc_upper else "+Inf"))
  cat("---------------------------------------------------------\n")
  print(obj$rankings, row.names = FALSE, digits = 4)
  cat("\n  Best fit:", obj$fits[[1L]]$display)
  est <- obj$fits[[1L]]$par
  cat("\n  Parameters:", paste(names(est), round(est, 5), sep = " = ", collapse = "  "), "\n\n")
  invisible(obj)
}


# -- Public: fitverse_censored() -----------------------------------------------

#' Fit parametric distributions to censored or truncated data
#'
#' Estimates distribution parameters by maximising the censored log-likelihood
#' for data that may be right-censored, left-censored, interval-censored, and/or
#' left-/right-truncated.  Distributions are ranked by AIC or BIC.
#'
#' @param x Numeric vector of observed or censored values.  For right-censored
#'   observations this is the censoring time; for left-censored observations it
#'   is the detection limit; for interval-censored observations it is the lower
#'   bound.
#' @param status Integer vector (same length as \code{x}) encoding the
#'   observation type:
#'   \describe{
#'     \item{\code{1}}{Completely observed (default for all if \code{NULL}).}
#'     \item{\code{0}}{Right-censored.}
#'     \item{\code{2}}{Left-censored.}
#'     \item{\code{3}}{Interval-censored (requires \code{x_upper}).}
#'   }
#'   If \code{NULL}, all observations are treated as complete.
#' @param x_upper Numeric vector: upper bounds for interval-censored
#'   observations (\code{status == 3}).  Ignored otherwise.
#' @param trunc_lower Numeric scalar: left truncation bound.  Only observations
#'   with \code{x > trunc_lower} are included in the sample; the likelihood is
#'   normalised accordingly.  Default \code{-Inf} (no truncation).
#' @param trunc_upper Numeric scalar: right truncation bound.  Default
#'   \code{Inf} (no truncation).
#' @param dists Character vector: distributions to fit.  Any subset of
#'   \code{c("normal","lognormal","weibull","exponential","gamma",
#'   "loglogistic","gumbel")}.  Default: all seven.
#' @param criterion Character: \code{"AIC"} (default) or \code{"BIC"} for
#'   ranking fits.
#' @param n_starts Integer: number of optimisation starting points per
#'   distribution.  More starts reduce the risk of local optima.
#'   Default \code{3}.
#' @param xlab Character: variable label for plots.  Default \code{"x"}.
#' @param draw Logical: produce a survival / CDF plot?  Default \code{TRUE}.
#'
#' @return An object of class \code{"fitverse_censored"} with elements:
#'   \describe{
#'     \item{\code{fits}}{Named list of fit objects, sorted by \code{criterion}.}
#'     \item{\code{rankings}}{Data frame: distribution, log-likelihood, AIC, BIC.}
#'     \item{\code{x}, \code{status}, \code{x_upper}}{Input data.}
#'     \item{\code{trunc_lower}, \code{trunc_upper}, \code{xlab}, \code{criterion}}{Settings.}
#'   }
#'
#' @references
#'   Meeker, W. Q. and Escobar, L. A. (1998).
#'   \emph{Statistical Methods for Reliability Data}.
#'   Wiley, New York.
#'
#'   Lawless, J. F. (2003).
#'   \emph{Statistical Models and Methods for Lifetime Data}, 2nd ed.
#'   Wiley, Hoboken.
#'
#' @examples
#' ## Right-censored survival data
#' set.seed(1)
#' times  <- rweibull(100, shape = 1.8, scale = 50)
#' censor <- rexp(100, rate = 1/60)
#' x      <- pmin(times, censor)
#' status <- as.integer(times <= censor)   # 1 = event, 0 = censored
#' fit_s  <- fitverse_censored(x, status, xlab = "Survival time")
#'
#' ## Left-censored environmental data (below detection limit)
#' set.seed(2)
#' true_conc  <- rlnorm(80, meanlog = 1.5, sdlog = 0.7)
#' dl         <- 3.0   # detection limit
#' x_env      <- pmax(true_conc, dl)
#' status_env <- as.integer(true_conc >= dl)  # 1=detected, 2=below DL
#' status_env[status_env == 0L] <- 2L
#' fit_e  <- fitverse_censored(x_env, status_env, xlab = "Concentration (mg/L)")
#'
#' @export
fitverse_censored <- function(x,
                               status      = NULL,
                               x_upper     = NULL,
                               trunc_lower = -Inf,
                               trunc_upper =  Inf,
                               dists       = c("normal", "lognormal", "weibull",
                                               "exponential", "gamma",
                                               "loglogistic", "gumbel"),
                               criterion   = c("AIC", "BIC"),
                               n_starts    = 3L,
                               xlab        = "x",
                               draw        = TRUE) {

  criterion <- match.arg(criterion)
  dists     <- intersect(tolower(dists), names(.CENS_DISTS))
  if (length(dists) == 0L)
    stop("No valid distributions specified.  Choose from: ",
         paste(names(.CENS_DISTS), collapse = ", "))

  x <- as.numeric(x)
  n <- length(x)

  # Default: all complete
  if (is.null(status)) status <- rep(1L, n)
  status <- as.integer(status)
  if (length(status) != n)
    stop("`status` must have the same length as `x`.")
  if (!all(status %in% 0L:3L))
    stop("`status` values must be 0 (right-censored), 1 (observed), ",
         "2 (left-censored), or 3 (interval-censored).")

  # Degenerate case: all observations are purely one-sided censored (right or
  # left only) with no exact or interval observations.
  # MLE based solely on right- or left-censored contributions is ill-posed for
  # most distributions (likelihood maximised by degenerate parameters).
  # Interval-censored observations (status == 3) contribute F(x_upper) -
  # F(x_lower), which IS a proper bounded likelihood term - purely
  # interval-censored data is valid and does NOT trigger this check.
  if (!any(status %in% c(1L, 3L)))
    stop(
      "No completely observed or interval-censored events found.\n",
      "All observations appear to be right-censored (status=0) or left-censored ",
      "(status=2).  At least some exact (status=1) or interval-censored ",
      "(status=3) observations are required for a well-posed likelihood.\n",
      "Check that your 'status' column is coded correctly: ",
      "0 = right-censored, 1 = observed, 2 = left-censored, 3 = interval-censored."
    )

  # Interval-censored upper bounds
  if (any(status == 3L)) {
    if (is.null(x_upper))
      stop("`x_upper` must be supplied for interval-censored observations (status == 3).")
    x_upper <- as.numeric(x_upper)
    if (length(x_upper) != n)
      stop("`x_upper` must have the same length as `x`.")
  } else {
    x_upper <- rep(Inf, n)
  }

  # Remove non-finite values
  ok <- is.finite(x)
  if (!all(ok)) {
    warning(sum(!ok), " non-finite value(s) removed.")
    x       <- x[ok]
    status  <- status[ok]
    x_upper <- x_upper[ok]
  }

  if (length(x) < 4L)
    stop("Need at least 4 observations.")

  # -- Fit each distribution --------------------------------------------------
  raw_fits <- lapply(dists, function(d) {
    tryCatch(
      .fit_one_cens(d, x, status, x_upper, trunc_lower, trunc_upper, n_starts),
      error = function(e) NULL
    )
  })
  raw_fits <- Filter(Negate(is.null), raw_fits)

  if (length(raw_fits) == 0L)
    stop("No distributions converged.  Try increasing n_starts or checking the data.")

  # -- Rank by criterion ------------------------------------------------------
  crit_vals <- vapply(raw_fits, function(f) if (criterion == "AIC") f$aic else f$bic,
                      numeric(1))
  ord       <- order(crit_vals)
  fits      <- raw_fits[ord]

  rankings <- data.frame(
    Rank         = seq_along(fits),
    Distribution = vapply(fits, `[[`, character(1), "display"),
    LogLik       = round(vapply(fits, `[[`, numeric(1), "log_lik"), 3),
    AIC          = round(vapply(fits, `[[`, numeric(1), "aic"),     3),
    BIC          = round(vapply(fits, `[[`, numeric(1), "bic"),     3),
    Converged    = vapply(fits, `[[`, logical(1), "converged"),
    stringsAsFactors = FALSE
  )

  obj <- structure(
    list(
      fits        = fits,
      rankings    = rankings,
      x           = x,
      status      = status,
      x_upper     = x_upper,
      trunc_lower = trunc_lower,
      trunc_upper = trunc_upper,
      xlab        = xlab,
      criterion   = criterion
    ),
    class = "fitverse_censored"
  )

  if (draw) plot(obj)
  invisible(obj)
}

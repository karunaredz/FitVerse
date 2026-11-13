# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : probability_tools.R
# Description: Probability tools that operate on a fitted
#              'fitverse' object:
#
#              dfitverse()  - density of the best-fit distribution
#              pfitverse()  - CDF of the best-fit distribution
#              qfitverse()  - quantile function (inverse CDF)
#              rfitverse()  - random samples from the best-fit
#              return_level() - T-year return level (exceedance probs)
#
#              All functions work seamlessly for all 52 distributions
#              in the registry via the .FITVERSE_DPQR_OVERRIDES lookup
#              table (for internal implementations) with fallback to
#              match.fun / getExportedValue for package-provided functions
#              (evd, sn, mc2d, actuar).
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


# -- Internal closed-form GEV / Gumbel d/p/q/r --------------------------------
# These bypass evd entirely, eliminating any package-version signature issues.
# Convention matches evd: loc (location), scale, shape (xi / kappa with evd sign).

.dgev_internal <- function(x, loc = 0, scale = 1, shape = 0, log = FALSE) {
  z <- (x - loc) / scale
  if (abs(shape) < 1e-8) {          # Gumbel limit
    lp <- -(z + exp(-z)) - log(scale)
  } else {
    t  <- 1 + shape * z
    ok <- t > 0
    lp <- ifelse(ok,
                 -(1/shape + 1) * log(t[ok]) - t[ok]^(-1/shape) - log(scale),
                 -Inf)
  }
  if (log) lp else exp(lp)
}

.pgev_internal <- function(q, loc = 0, scale = 1, shape = 0) {
  z <- (q - loc) / scale
  if (abs(shape) < 1e-8) {
    exp(-exp(-z))
  } else {
    t  <- 1 + shape * z
    ok <- t > 0
    out <- ifelse(ok, exp(-t^(-1/shape)), ifelse(shape < 0, 1, 0))
    out
  }
}

.qgev_internal <- function(p, loc = 0, scale = 1, shape = 0) {
  # Q(p) = loc + scale/shape * ((-log(p))^(-shape) - 1)  for shape != 0
  # Q(p) = loc - scale * log(-log(p))                    for shape == 0
  if (abs(shape) < 1e-8) {
    loc - scale * log(-log(p))
  } else {
    loc + scale / shape * ((-log(p))^(-shape) - 1)
  }
}

.rgev_internal <- function(n, loc = 0, scale = 1, shape = 0) {
  .qgev_internal(stats::runif(n), loc, scale, shape)
}

# Gumbel is GEV with shape == 0; same formulas, same parameter names (loc, scale)
.dgumbel_internal <- function(x, loc = 0, scale = 1, log = FALSE)
  .dgev_internal(x, loc, scale, shape = 0, log = log)

.pgumbel_internal <- function(q, loc = 0, scale = 1)
  .pgev_internal(q, loc, scale, shape = 0)

.qgumbel_internal <- function(p, loc = 0, scale = 1)
  .qgev_internal(p, loc, scale, shape = 0)

.rgumbel_internal <- function(n, loc = 0, scale = 1)
  .rgev_internal(n, loc, scale, shape = 0)

# -- Internal closed-form GPD d/p/q/r -----------------------------------------
# Convention: loc (location/threshold), scale (>0), shape (xi)
# For shape=0: reduces to exponential shifted by loc.
# Support: x >= loc (shape >= 0) or loc <= x <= loc - scale/shape (shape < 0)

.dgpd_internal <- function(x, loc = 0, scale = 1, shape = 0, log = FALSE) {
  z  <- (x - loc) / scale
  if (abs(shape) < 1e-8) {
    lp <- ifelse(z >= 0, -log(scale) - z, -Inf)
  } else {
    t  <- 1 + shape * z
    ok <- t > 0
    lp <- ifelse(ok, -log(scale) - (1 / shape + 1) * log(t), -Inf)
  }
  if (log) lp else exp(lp)
}

.pgpd_internal <- function(q, loc = 0, scale = 1, shape = 0) {
  z <- (q - loc) / scale
  if (abs(shape) < 1e-8) {
    ifelse(z >= 0, 1 - exp(-z), 0)
  } else {
    t  <- 1 + shape * z
    ok <- t > 0
    ifelse(ok, 1 - t^(-1 / shape), ifelse(shape < 0, 1, 0))
  }
}

.qgpd_internal <- function(p, loc = 0, scale = 1, shape = 0) {
  if (abs(shape) < 1e-8) {
    loc - scale * log(1 - p)
  } else {
    loc + scale / shape * ((1 - p)^(-shape) - 1)
  }
}

.rgpd_internal <- function(n, loc = 0, scale = 1, shape = 0) {
  .qgpd_internal(stats::runif(n), loc, scale, shape)
}


# -- Internal closed-form Pearson III d/p/q/r ----------------------------------
# Pearson III is a shifted/scaled Gamma.
# Parameterisation: shape (alpha > 0), scale (beta, may be negative), loc (zeta)
# PDF: f(x) = dgamma(|(x-loc)/scale|, shape) / |scale|
# Support: if scale > 0, x > loc; if scale < 0, x < loc.

.dpearson3_internal <- function(x, shape, scale, loc = 0, log = FALSE) {
  z      <- (x - loc) / scale
  abs_sc <- abs(scale)
  ok     <- if (scale > 0) z > 0 else z < 0
  lp     <- ifelse(ok,
                   stats::dgamma(abs(z), shape = shape, rate = 1, log = TRUE) - log(abs_sc),
                   -Inf)
  if (log) lp else exp(lp)
}

.ppearson3_internal <- function(q, shape, scale, loc = 0) {
  z <- (q - loc) / scale
  if (scale > 0) {
    ifelse(z > 0, stats::pgamma(z,      shape = shape), 0)
  } else {
    ifelse(z < 0, 1 - stats::pgamma(abs(z), shape = shape), 1)
  }
}

.qpearson3_internal <- function(p, shape, scale, loc = 0) {
  if (scale > 0) {
    loc + scale * stats::qgamma(p,     shape = shape)
  } else {
    loc + scale * stats::qgamma(1 - p, shape = shape)
  }
}

.rpearson3_internal <- function(n, shape, scale, loc = 0) {
  .qpearson3_internal(stats::runif(n), shape, scale, loc)
}


# -- Internal Log-Pearson III d/p/q/r -----------------------------------------
# X ~ LP3(shape, scale, loc)  iff  log(X) ~ P3(shape, scale, loc)
# Support: x > 0 and log(x) in P3 support.

.dlpearson3_internal <- function(x, shape, scale, loc = 0, log = FALSE) {
  ok <- x > 0
  lp <- rep(-Inf, length(x))
  if (any(ok)) {
    lp[ok] <- (.dpearson3_internal(log(x[ok]), shape, scale, loc, log = TRUE)
               - log(x[ok]))
  }
  if (log) lp else exp(lp)
}

.plpearson3_internal <- function(q, shape, scale, loc = 0) {
  ok  <- q > 0
  out <- rep(0, length(q))
  if (any(ok)) out[ok] <- .ppearson3_internal(log(q[ok]), shape, scale, loc)
  out
}

.qlpearson3_internal <- function(p, shape, scale, loc = 0) {
  exp(.qpearson3_internal(p, shape, scale, loc))
}

.rlpearson3_internal <- function(n, shape, scale, loc = 0) {
  exp(.rpearson3_internal(n, shape, scale, loc))
}


# -- Internal GLO (Generalised Logistic) d/p/q/r -------------------------------
# Parameterisation: loc (xi), scale (alpha), shape (kappa)
# F(x) = 1 / (1 + t^{1/kappa})  where t = 1 - kappa(x-xi)/alpha  (kappa!=0)
# F(x) = 1 / (1 + exp(-z))                             (kappa=0: standard logistic)

.dglo_internal <- function(x, loc = 0, scale = 1, shape = 0, log = FALSE) {
  z <- (x - loc) / scale
  if (abs(shape) < 1e-8) {
    lp <- -log(scale) - z - 2 * log(1 + exp(-z))
  } else {
    t  <- 1 - shape * z
    ok <- t > 0
    lp <- ifelse(ok,
                 -log(scale) + (1 / shape - 1) * log(t) - 2 * log(1 + t^(1 / shape)),
                 -Inf)
  }
  if (log) lp else exp(lp)
}

.pglo_internal <- function(q, loc = 0, scale = 1, shape = 0) {
  z <- (q - loc) / scale
  if (abs(shape) < 1e-8) {
    1 / (1 + exp(-z))
  } else {
    t  <- 1 - shape * z
    ok <- t > 0
    ifelse(ok,
           1 / (1 + t^(1 / shape)),
           ifelse(shape > 0, 1, 0))
  }
}

.qglo_internal <- function(p, loc = 0, scale = 1, shape = 0) {
  if (abs(shape) < 1e-8) {
    loc + scale * log(p / (1 - p))
  } else {
    loc + scale / shape * (1 - ((1 - p) / p)^shape)
  }
}

.rglo_internal <- function(n, loc = 0, scale = 1, shape = 0) {
  .qglo_internal(stats::runif(n), loc, scale, shape)
}


# -- 3-Parameter Weibull d/p/q/r -----------------------------------------------
# Defined here (before .FITVERSE_DPQR_OVERRIDES) so the list can reference them
# directly.  utils.R loads after probability_tools.R alphabetically, so placing
# them there caused an "object not found" error at package load time.

#' @export
#' @noRd
dweibull3p <- function(x, shape, scale, thres, log = FALSE) {
  y <- x - thres
  if (log) {
    ld <- stats::dweibull(y, shape = shape, scale = scale, log = TRUE)
    ld[y <= 0] <- -Inf
    ld
  } else {
    d <- stats::dweibull(y, shape = shape, scale = scale)
    d[y <= 0] <- 0
    d
  }
}

#' @export
#' @noRd
pweibull3p <- function(q, shape, scale, thres, lower.tail = TRUE, log.p = FALSE) {
  stats::pweibull(q - thres, shape = shape, scale = scale,
                  lower.tail = lower.tail, log.p = log.p)
}

#' @export
#' @noRd
qweibull3p <- function(p, shape, scale, thres, lower.tail = TRUE, log.p = FALSE) {
  stats::qweibull(p, shape = shape, scale = scale,
                  lower.tail = lower.tail, log.p = log.p) + thres
}

#' @export
#' @noRd
rweibull3p <- function(n, shape, scale, thres) {
  stats::rweibull(n, shape = shape, scale = scale) + thres
}


# -- Internal: look up d/p/q/r by dist_key (used by plot_fitverse.R) ----------

#' Resolve a d/p/q/r function by distribution key string
#'
#' Checks the closed-form override table first, then match.fun, then
#' getExportedValue.  Used by both \code{probability_tools.R} and
#' \code{plot_fitverse.R}.
#'
#' @param prefix   One of \code{"d"}, \code{"p"}, \code{"q"}, \code{"r"}.
#' @param dist_key Character: distribution key (e.g. \code{"gev"}).
#' @return A function, or \code{NULL}.
#' @keywords internal
.lookup_dpqr <- function(prefix, dist_key) {
  entry    <- get_dist_entry(dist_key)
  fun_name <- paste0(prefix, entry$r_name)

  if (!is.null(.FITVERSE_DPQR_OVERRIDES[[fun_name]]))
    return(.FITVERSE_DPQR_OVERRIDES[[fun_name]])

  fun <- tryCatch(match.fun(fun_name), error = function(e) NULL)
  if (is.null(fun) && !is.null(entry$package) &&
      !entry$package %in% c("stats", "base") &&
      requireNamespace(entry$package, quietly = TRUE)) {
    fun <- tryCatch(getExportedValue(entry$package, fun_name),
                    error = function(e) NULL)
  }
  fun
}


# -- Internal: resolve d/p/q/r function for a fit -----------------------------

#' Look up a d/p/q/r function for a fit-result list
#'
#' For GEV and Gumbel, returns FitVerse's own closed-form implementations to
#' avoid evd package-version signature issues.  For all other distributions,
#' tries \code{match.fun()} first (works for \pkg{stats} and auto-attached
#' packages), then falls back to \code{getExportedValue()} for packages in
#' Imports/Suggests (e.g. \pkg{actuar}, \pkg{sn}, \pkg{mc2d}).
#'
#' @param fit     A fit-result list (element of \code{fitverse$all_fits}).
#' @param prefix  One of \code{"d"}, \code{"p"}, \code{"q"}, \code{"r"}.
#' @return A function, or \code{NULL} if not found.
#' @keywords internal
.resolve_dpqr <- function(fit, prefix) {
  .lookup_dpqr(prefix, fit$dist_key)
}


# -- dfitverse -----------------------------------------------------------------

#' Evaluate the density of the best-fit distribution
#'
#' @param x     Numeric vector of values at which to evaluate the density.
#' @param fit   A \code{fitverse} object.
#' @param log   Logical: return log-density? Default \code{FALSE}.
#' @return Numeric vector of density values (same length as \code{x}).
#' @export
#' @examples
#' set.seed(1)
#' dat <- rgamma(200, 2, 0.5)
#' fv  <- fitverse(dat, dists = "gamma", plot = FALSE, verbose = FALSE)
#' dfitverse(c(1, 2, 3), fv)
dfitverse <- function(x, fit, log = FALSE) {
  stopifnot(inherits(fit, "fitverse"))
  d_fun <- .resolve_dpqr(fit$best_fit, "d")
  if (is.null(d_fun))
    stop(sprintf("Could not find density function for '%s'.",
                 fit$best_fit$dist_key), call. = FALSE)
  # Call without log= to maximise compatibility; apply log manually.
  out <- tryCatch(
    do.call(d_fun, c(list(x = x), as.list(fit$best_fit$params))),
    error = function(e) stop(sprintf(
      "dfitverse failed for '%s': %s", fit$best_fit$dist_key, conditionMessage(e)),
      call. = FALSE)
  )
  if (log) log(out) else out
}


# -- pfitverse -----------------------------------------------------------------

#' @export
#' @noRd
#' @examples
#' set.seed(1)
#' dat <- rgamma(200, 2, 0.5)
#' fv  <- fitverse(dat, dists = "gamma", plot = FALSE, verbose = FALSE)
#' pfitverse(c(1, 3, 5), fv)
pfitverse <- function(q, fit, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(inherits(fit, "fitverse"))
  p_fun <- .resolve_dpqr(fit$best_fit, "p")
  if (is.null(p_fun))
    stop(sprintf("Could not find CDF function for '%s'.",
                 fit$best_fit$dist_key), call. = FALSE)
  # Call with q and params only - apply lower.tail / log.p manually for
  # full compatibility across packages (evd, actuar, mc2d, sn, etc.).
  out <- tryCatch(
    do.call(p_fun, c(list(q = q), as.list(fit$best_fit$params))),
    error = function(e) stop(sprintf(
      "pfitverse failed for '%s': %s", fit$best_fit$dist_key, conditionMessage(e)),
      call. = FALSE)
  )
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}


# -- qfitverse -----------------------------------------------------------------

#' @export
#' @noRd
#' @examples
#' set.seed(1)
#' dat <- rgamma(200, 2, 0.5)
#' fv  <- fitverse(dat, dists = "gamma", plot = FALSE, verbose = FALSE)
#' qfitverse(c(0.25, 0.50, 0.75, 0.90, 0.99), fv)
qfitverse <- function(p, fit, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(inherits(fit, "fitverse"))
  q_fun <- .resolve_dpqr(fit$best_fit, "q")
  if (is.null(q_fun))
    stop(sprintf("Could not find quantile function for '%s'.",
                 fit$best_fit$dist_key), call. = FALSE)
  # Apply lower.tail and log.p to p BEFORE calling the quantile function,
  # rather than passing them as named args.  This ensures full compatibility
  # with packages (evd, actuar, mc2d, sn) whose q* functions may not
  # support these optional flags.
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  tryCatch(
    do.call(q_fun, c(list(p = p), as.list(fit$best_fit$params))),
    error = function(e) stop(sprintf(
      "qfitverse failed for '%s': %s", fit$best_fit$dist_key, conditionMessage(e)),
      call. = FALSE)
  )
}


# -- rfitverse -----------------------------------------------------------------

#' @export
#' @noRd
#' @examples
#' set.seed(1)
#' dat <- rgamma(200, 2, 0.5)
#' fv  <- fitverse(dat, dists = "gamma", plot = FALSE, verbose = FALSE)
#' rfitverse(10, fv)
rfitverse <- function(n, fit) {
  stopifnot(inherits(fit, "fitverse"), is.numeric(n), n >= 1)
  r_fun <- .resolve_dpqr(fit$best_fit, "r")
  if (is.null(r_fun))
    stop(sprintf("Could not find random-generation function for '%s'.",
                 fit$best_fit$dist_key), call. = FALSE)
  do.call(r_fun, c(list(n = as.integer(n)), as.list(fit$best_fit$params)))
}


# -- return_level --------------------------------------------------------------

#' Compute return levels for the best-fit distribution
#'
#' A \emph{T}-year return level is the value \eqn{x_T} such that
#' \eqn{P(X > x_T) = 1/T}, i.e. it is the \eqn{(1 - 1/T)} quantile.
#'
#' This is directly interpretable for annual maxima: the 100-year return level
#' has a 1\% chance of being exceeded in any given year.
#'
#' @param fit            A \code{fitverse} object.
#' @param return_periods Numeric vector of return periods (years, or any unit
#'                       consistent with the data).  Default: common
#'                       engineering return periods.
#' @return A data frame with columns \code{return_period},
#'   \code{exceedance_prob} (\eqn{1/T}), and \code{return_level}.
#' @export
#' @examples
#' set.seed(42)
#' dat <- rgev(200, loc = 10, scale = 2, shape = 0.1)
#' fv  <- fitverse(dat, dists = "gev", method = "MLE", plot = FALSE)
#' return_level(fv, return_periods = c(2, 5, 10, 25, 50, 100))
return_level <- function(fit,
                         return_periods = c(2, 5, 10, 20, 50, 100, 200, 500)) {

  stopifnot(inherits(fit, "fitverse"))
  if (any(return_periods < 1))
    stop("Return periods must be >= 1.", call. = FALSE)

  probs  <- 1 - 1 / return_periods
  levels <- qfitverse(probs, fit)

  data.frame(
    return_period   = return_periods,
    exceedance_prob = round(1 / return_periods, 6),
    return_level    = levels,
    stringsAsFactors = FALSE
  )
}


# ==============================================================================
# Custom d/p/q/r implementations for 9 new distributions (added 2026-06-21)
# All functions follow the FitVerse convention:
#   - scalar or vector x/q/p supported via vectorised ifelse
#   - log = TRUE returns log-density (avoids underflow)
#   - out-of-support inputs yield -Inf (log) or 0 (density) gracefully
# ==============================================================================

# -- Right Triangular (mode fixed at min) -------------------------------------
# PDF: 2(b-x)/(b-a)^2,  a <= x <= b
# CDF: 1 - (b-x)^2/(b-a)^2
# QF : b - (b-a)*sqrt(1-u)

#' @export
#' @noRd
drtriangle <- function(x, min, max, log = FALSE) {
  if (max <= min) return(rep(if (log) -Inf else 0, length(x)))
  rng <- max - min
  ok  <- x >= min & x <= max
  lp  <- rep(-Inf, length(x))
  lp[ok] <- log(2) + log(pmax(max - x[ok], 0)) - 2 * log(rng)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
prtriangle <- function(q, min, max) {
  if (max <= min) return(ifelse(q >= max, 1, 0))
  rng <- max - min
  out <- ifelse(q <= min, 0,
         ifelse(q >= max, 1,
                1 - ((max - q) / rng)^2))
  out
}

#' @export
#' @noRd
qrtriangle <- function(p, min, max) {
  max - (max - min) * sqrt(1 - p)
}

#' @export
#' @noRd
rrtriangle <- function(n, min, max) {
  qrtriangle(stats::runif(n), min, max)
}


# -- Kumaraswamy ---------------------------------------------------------------
# PDF: a*b*x^(a-1)*(1-x^a)^(b-1),  0 < x < 1
# CDF: 1 - (1 - x^a)^b
# QF : (1 - (1-u)^(1/b))^(1/a)

#' @export
#' @noRd
dkumaraswamy <- function(x, shape1, shape2, log = FALSE) {
  a <- shape1; b <- shape2
  ok <- x > 0 & x < 1
  lp <- rep(-Inf, length(x))
  xa <- x[ok]^a
  lp[ok] <- log(a) + log(b) + (a - 1) * log(x[ok]) + (b - 1) * log1p(-xa)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pkumaraswamy <- function(q, shape1, shape2) {
  a <- shape1; b <- shape2
  out <- ifelse(q <= 0, 0,
         ifelse(q >= 1, 1,
                1 - (1 - q^a)^b))
  out
}

#' @export
#' @noRd
qkumaraswamy <- function(p, shape1, shape2) {
  (1 - (1 - p)^(1 / shape2))^(1 / shape1)
}

#' @export
#' @noRd
rkumaraswamy <- function(n, shape1, shape2) {
  qkumaraswamy(stats::runif(n), shape1, shape2)
}


# -- Inverse Gaussian (Wald) ---------------------------------------------------
# PDF: sqrt(lambda/(2*pi*x^3)) * exp(-lambda*(x-mu)^2/(2*mu^2*x)),  x > 0
# CDF: Phi(sqrt(lambda/x)*((x/mu)-1)) + exp(2*lambda/mu)*Phi(-sqrt(lambda/x)*((x/mu)+1))

#' @export
#' @noRd
dinvgaussian <- function(x, mu, lambda, log = FALSE) {
  ok <- x > 0 & is.finite(x)
  lp <- rep(-Inf, length(x))
  xx <- x[ok]
  lp[ok] <- 0.5 * (log(lambda) - log(2 * pi) - 3 * log(xx)) -
            lambda * (xx - mu)^2 / (2 * mu^2 * xx)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pinvgaussian <- function(q, mu, lambda) {
  out <- rep(0, length(q))
  ok  <- q > 0 & is.finite(q)
  qok <- q[ok]
  sqrl <- sqrt(lambda / qok)
  t1   <- sqrl * (qok / mu - 1)
  t2   <- sqrl * (qok / mu + 1)
  out[ok] <- stats::pnorm(t1) +
             exp(2 * lambda / mu) * stats::pnorm(-t2)
  out[q >= Inf] <- 1
  out
}

#' @export
#' @noRd
qinvgaussian <- function(p, mu, lambda) {
  # Numerical inversion via uniroot for each probability
  vapply(p, function(pi) {
    if (pi <= 0) return(0)
    if (pi >= 1) return(Inf)
    upper <- mu * 100
    tryCatch(
      stats::uniroot(function(q) pinvgaussian(q, mu, lambda) - pi,
                     lower = 1e-10, upper = upper,
                     tol = .Machine$double.eps^0.5)$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
}

#' @export
#' @noRd
rinvgaussian <- function(n, mu, lambda) {
  # Michael, Schucany & Haas (1976) algorithm
  y  <- stats::rnorm(n)^2
  x1 <- mu + mu^2 * y / (2 * lambda) -
        mu / (2 * lambda) * sqrt(4 * mu * lambda * y + mu^2 * y^2)
  u  <- stats::runif(n)
  ifelse(u <= mu / (mu + x1), x1, mu^2 / x1)
}


# -- Lomax (Pareto Type II, zero-shifted) -------------------------------------
# PDF: (alpha/lambda) * (1+x/lambda)^(-(alpha+1)),  x >= 0
# CDF: 1 - (1+x/lambda)^(-alpha)
# QF : lambda * ((1-u)^(-1/alpha) - 1)

#' @export
#' @noRd
dlomax <- function(x, alpha, lambda, log = FALSE) {
  ok <- x >= 0 & is.finite(x)
  lp <- rep(-Inf, length(x))
  lp[ok] <- log(alpha) - log(lambda) - (alpha + 1) * log1p(x[ok] / lambda)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
plomax <- function(q, alpha, lambda) {
  ifelse(q < 0, 0, 1 - (1 + q / lambda)^(-alpha))
}

#' @export
#' @noRd
qlomax <- function(p, alpha, lambda) {
  lambda * ((1 - p)^(-1 / alpha) - 1)
}

#' @export
#' @noRd
rlomax <- function(n, alpha, lambda) {
  qlomax(stats::runif(n), alpha, lambda)
}


# -- Laplace (Double Exponential) ---------------------------------------------
# PDF: exp(-|x-mu|/b) / (2b)
# CDF: 0.5*exp((x-mu)/b)  if x < mu
#      1 - 0.5*exp(-(x-mu)/b)  if x >= mu

#' @export
#' @noRd
dlaplace <- function(x, mu, b, log = FALSE) {
  if (b <= 0) return(rep(if (log) -Inf else 0, length(x)))
  lp <- -log(2) - log(b) - abs(x - mu) / b
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
plaplace <- function(q, mu, b) {
  z <- (q - mu) / b
  ifelse(q < mu,
         0.5 * exp(z),
         1 - 0.5 * exp(-z))
}

#' @export
#' @noRd
qlaplace <- function(p, mu, b) {
  mu - b * sign(p - 0.5) * log(1 - 2 * abs(p - 0.5))
}

#' @export
#' @noRd
rlaplace <- function(n, mu, b) {
  qlaplace(stats::runif(n), mu, b)
}


# -- Generalised Gamma ---------------------------------------------------------
# PDF: (p/Gamma(d/p)) * (x/b)^(d-1) * exp(-(x/b)^p) / b
# CDF: pgamma((x/b)^p, d/p)  [regularised incomplete gamma]
# QF : b * qgamma(u, d/p)^(1/p)
# Params: p (shape1 > 0), d (shape2 > 0), b (scale > 0)

#' @export
#' @noRd
dgengamma <- function(x, p, d, b, log = FALSE) {
  lp     <- rep(-Inf, length(x))
  ok_vec <- x > 0 & is.finite(x)
  if (p > 0 && d > 0 && b > 0 && any(ok_vec)) {
    lp[ok_vec] <- log(p) - lgamma(d / p) + (d - 1) * log(x[ok_vec] / b) -
                  (x[ok_vec] / b)^p - log(b)
  }
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pgengamma <- function(q, p, d, b) {
  out <- rep(0, length(q))
  ok  <- q > 0 & is.finite(q)
  out[ok] <- stats::pgamma((q[ok] / b)^p, shape = d / p)
  out[q >= Inf] <- 1
  out
}

#' @export
#' @noRd
qgengamma <- function(prob, p, d, b) {
  b * stats::qgamma(prob, shape = d / p)^(1 / p)
}

#' @export
#' @noRd
rgengamma <- function(n, p, d, b) {
  b * stats::rgamma(n, shape = d / p)^(1 / p)
}


# -- Gompertz -----------------------------------------------------------------
# PDF: alpha * exp(beta*x) * exp(-alpha/beta*(exp(beta*x)-1)),  x >= 0
# CDF: 1 - exp(-alpha/beta*(exp(beta*x)-1))
# QF : log(1 - beta/alpha * log(1-u)) / beta

#' @export
#' @noRd
dgompertz <- function(x, alpha, beta, log = FALSE) {
  ok <- x >= 0 & is.finite(x) & alpha > 0 & beta > 0
  lp <- rep(-Inf, length(x))
  ok_vec <- x >= 0 & is.finite(x)
  bx   <- beta * x[ok_vec]
  lp[ok_vec] <- log(alpha) + bx - (alpha / beta) * (exp(bx) - 1)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pgompertz <- function(q, alpha, beta) {
  out <- rep(0, length(q))
  ok  <- q >= 0 & is.finite(q)
  out[ok] <- 1 - exp(-(alpha / beta) * (exp(beta * q[ok]) - 1))
  out[q >= Inf] <- 1
  out
}

#' @export
#' @noRd
qgompertz <- function(p, alpha, beta) {
  log(1 - (beta / alpha) * log(1 - p)) / beta
}

#' @export
#' @noRd
rgompertz <- function(n, alpha, beta) {
  qgompertz(stats::runif(n), alpha, beta)
}


# -- Birnbaum-Saunders (Fatigue Life) -----------------------------------------
# PDF: phi(z/alpha) * (sqrt(x/beta) + sqrt(beta/x)) / (2*alpha*x)
#      where z = sqrt(x/beta) - sqrt(beta/x)
# CDF: Phi(z/alpha)
# QF : beta * (alpha*z/2 + sqrt((alpha*z/2)^2+1))^2   for z = qnorm(u)

#' @export
#' @noRd
dbirnbaum_saunders <- function(x, alpha, beta, log = FALSE) {
  ok <- x > 0 & is.finite(x) & alpha > 0 & beta > 0
  lp <- rep(-Inf, length(x))
  ok_vec <- x > 0 & is.finite(x)
  xx <- x[ok_vec]
  z  <- sqrt(xx / beta) - sqrt(beta / xx)
  w  <- sqrt(xx / beta) + sqrt(beta / xx)
  lp[ok_vec] <- stats::dnorm(z / alpha, log = TRUE) + log(w) -
                log(2) - log(alpha) - log(xx)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pbirnbaum_saunders <- function(q, alpha, beta) {
  out <- rep(0, length(q))
  ok  <- q > 0 & is.finite(q)
  qq  <- q[ok]
  z   <- sqrt(qq / beta) - sqrt(beta / qq)
  out[ok] <- stats::pnorm(z / alpha)
  out[q >= Inf] <- 1
  out
}

#' @export
#' @noRd
qbirnbaum_saunders <- function(p, alpha, beta) {
  z  <- stats::qnorm(p)
  t  <- alpha * z / 2
  beta * (t + sqrt(t^2 + 1))^2
}

#' @export
#' @noRd
rbirnbaum_saunders <- function(n, alpha, beta) {
  z <- stats::rnorm(n)
  t <- alpha * z / 2
  beta * (t + sqrt(t^2 + 1))^2
}


# -- Student-t Location-Scale --------------------------------------------------
# PDF: dt((x-mu)/sigma, nu) / sigma
# CDF: pt((x-mu)/sigma, nu)
# QF : mu + sigma * qt(p, nu)

#' @export
#' @noRd
dstudent_t <- function(x, nu, mu, sigma, log = FALSE) {
  z  <- (x - mu) / sigma
  lp <- stats::dt(z, df = nu, log = TRUE) - log(sigma)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pstudent_t <- function(q, nu, mu, sigma) {
  stats::pt((q - mu) / sigma, df = nu)
}

#' @export
#' @noRd
qstudent_t <- function(p, nu, mu, sigma) {
  mu + sigma * stats::qt(p, df = nu)
}

#' @export
#' @noRd
rstudent_t <- function(n, nu, mu, sigma) {
  mu + sigma * stats::rt(n, df = nu)
}



# -- Rayleigh ------------------------------------------------------------------
# PDF : (x/sigma^2) * exp(-x^2 / (2*sigma^2)),  x >= 0
# MLE : sigma_hat = sqrt(mean(x^2) / 2)   [closed-form]
# Note: special case of Weibull(shape=2, scale=sigma*sqrt(2))

#' @export
#' @noRd
drayleigh <- function(x, sigma, log = FALSE) {
  ld <- ifelse(x < 0, -Inf,
               log(x) - 2*log(sigma) - x^2 / (2*sigma^2))
  if (!log) exp(ld) else ld
}

#' @export
#' @noRd
prayleigh <- function(q, sigma, lower.tail = TRUE, log.p = FALSE) {
  p <- ifelse(q < 0, 0, 1 - exp(-q^2 / (2*sigma^2)))
  if (!lower.tail) p <- 1 - p
  if (log.p) log(p) else p
}

#' @export
#' @noRd
qrayleigh <- function(p, sigma, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  sigma * sqrt(-2 * log(1 - p))
}

#' @export
#' @noRd
rrayleigh <- function(n, sigma) {
  sigma * sqrt(-2 * log(stats::runif(n)))
}


# -- Half-Normal ---------------------------------------------------------------
# X = |Z|, Z ~ N(0, sigma^2)
# PDF : sqrt(2/pi)/sigma * exp(-x^2 / (2*sigma^2)),  x >= 0
# MLE : sigma_hat = sqrt(mean(x^2))   [closed-form]

#' @export
#' @noRd
dhalfnormal <- function(x, sigma, log = FALSE) {
  ld <- ifelse(x < 0, -Inf,
               0.5*log(2/pi) - log(sigma) - x^2 / (2*sigma^2))
  if (!log) exp(ld) else ld
}

#' @export
#' @noRd
phalfnormal <- function(q, sigma, lower.tail = TRUE, log.p = FALSE) {
  p <- ifelse(q < 0, 0, 2*stats::pnorm(q / sigma) - 1)
  if (!lower.tail) p <- 1 - p
  if (log.p) log(p) else p
}

#' @export
#' @noRd
qhalfnormal <- function(p, sigma, lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  sigma * stats::qnorm((1 + p) / 2)
}

#' @export
#' @noRd
rhalfnormal <- function(n, sigma) {
  abs(stats::rnorm(n, 0, sigma))
}


# -- Exponentiated Weibull -----------------------------------------------------
# CDF : (1 - exp(-(x/scale)^shape1))^shape2
# PDF : shape2*(shape1/scale)*(x/scale)^(shape1-1)*exp(-(x/scale)^shape1)
#        * (1-exp(-(x/scale)^shape1))^(shape2-1)
# QF  : scale * (-log(1 - p^(1/shape2)))^(1/shape1)

#' @export
#' @noRd
dexpweibull <- function(x, shape1, scale, shape2, log = FALSE) {
  ld <- rep(-Inf, length(x))
  ok <- x > 0
  if (any(ok)) {
    u        <- (x[ok] / scale)^shape1
    ev       <- exp(-u)
    one_ev   <- 1 - ev
    one_ev   <- pmax(one_ev, .Machine$double.eps)
    ld[ok]   <- log(shape2) + log(shape1) - log(scale) +
                 (shape1 - 1)*log(x[ok] / scale) - u +
                 (shape2 - 1)*log(one_ev)
  }
  if (!log) exp(ld) else ld
}

#' @export
#' @noRd
pexpweibull <- function(q, shape1, scale, shape2,
                        lower.tail = TRUE, log.p = FALSE) {
  p <- ifelse(q <= 0, 0,
              (1 - exp(-(q / scale)^shape1))^shape2)
  if (!lower.tail) p <- 1 - p
  if (log.p) log(p) else p
}

#' @export
#' @noRd
qexpweibull <- function(p, shape1, scale, shape2,
                        lower.tail = TRUE, log.p = FALSE) {
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  scale * (-log(1 - p^(1 / shape2)))^(1 / shape1)
}

#' @export
#' @noRd
rexpweibull <- function(n, shape1, scale, shape2) {
  u <- stats::runif(n)
  scale * (-log(1 - u^(1 / shape2)))^(1 / shape1)
}


# -- Gamma (3-Parameter) -------------------------------------------------------
# X - thres ~ Gamma(shape, scale),   thres = location threshold
# Note: mathematically equivalent to Pearson Type III; offered here with
#       the engineer-friendly (shape, scale, thres) parameterisation.

#' @export
#' @noRd
dgamma3 <- function(x, shape, scale, thres, log = FALSE) {
  ld <- ifelse(x <= thres, -Inf,
               stats::dgamma(x - thres, shape = shape, scale = scale,
                             log = TRUE))
  if (!log) exp(ld) else ld
}

#' @export
#' @noRd
pgamma3 <- function(q, shape, scale, thres,
                    lower.tail = TRUE, log.p = FALSE) {
  stats::pgamma(q - thres, shape = shape, scale = scale,
                lower.tail = lower.tail, log.p = log.p)
}

#' @export
#' @noRd
qgamma3 <- function(p, shape, scale, thres,
                    lower.tail = TRUE, log.p = FALSE) {
  thres + stats::qgamma(p, shape = shape, scale = scale,
                        lower.tail = lower.tail, log.p = log.p)
}

#' @export
#' @noRd
rgamma3 <- function(n, shape, scale, thres) {
  thres + stats::rgamma(n, shape = shape, scale = scale)
}


# -- Log-Normal (3-Parameter) --------------------------------------------------
# X - thres ~ LogNormal(meanlog, sdlog),   thres = lower threshold

#' @export
#' @noRd
dlnorm3 <- function(x, meanlog, sdlog, thres, log = FALSE) {
  ld <- ifelse(x <= thres, -Inf,
               stats::dlnorm(x - thres, meanlog = meanlog, sdlog = sdlog,
                             log = TRUE))
  if (!log) exp(ld) else ld
}

#' @export
#' @noRd
plnorm3 <- function(q, meanlog, sdlog, thres,
                    lower.tail = TRUE, log.p = FALSE) {
  stats::plnorm(q - thres, meanlog = meanlog, sdlog = sdlog,
                lower.tail = lower.tail, log.p = log.p)
}

#' @export
#' @noRd
qlnorm3 <- function(p, meanlog, sdlog, thres,
                    lower.tail = TRUE, log.p = FALSE) {
  thres + stats::qlnorm(p, meanlog = meanlog, sdlog = sdlog,
                        lower.tail = lower.tail, log.p = log.p)
}

#' @export
#' @noRd
rlnorm3 <- function(n, meanlog, sdlog, thres) {
  thres + stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog)
}


# ==============================================================================
# Internal d/p/q/r implementations for distributions 40-52
# ==============================================================================


# -- GB2 (Generalised Beta of the Second Kind) ---------------------------------
# params: shape1=p, scale=b, shape2=a, shape3=q_s (second shape; 'q' avoided as
#         name because the CDF p-function's first arg is already named 'q')

#' @export
#' @noRd
dgb2 <- function(x, shape1, scale, shape2, shape3, log = FALSE) {
  p_s <- shape1; b <- scale; a <- shape2; qs <- shape3
  ok  <- x > 0
  lp  <- rep(-Inf, length(x))
  if (any(ok)) {
    z       <- x[ok] / b
    lp[ok]  <- log(abs(a)) + (a * p_s - 1) * log(x[ok]) -
                a * p_s * log(b) - lbeta(p_s, qs) -
                (p_s + qs) * log1p(z^a)
  }
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pgb2 <- function(q, shape1, scale, shape2, shape3,
                  lower.tail = TRUE, log.p = FALSE) {
  p_s <- shape1; b <- scale; a <- shape2; qs <- shape3
  out <- rep(0.0, length(q))
  ok  <- q > 0
  if (any(ok)) {
    z        <- (q[ok] / b)^a
    out[ok]  <- stats::pbeta(z / (1 + z), p_s, qs)
  }
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qgb2 <- function(p, shape1, scale, shape2, shape3,
                  lower.tail = TRUE, log.p = FALSE) {
  p_s <- shape1; b <- scale; a <- shape2; qs <- shape3
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  u <- stats::qbeta(p, p_s, qs)
  b * (u / (1 - u))^(1 / a)
}

#' @export
#' @noRd
rgb2 <- function(n, shape1, scale, shape2, shape3) {
  qgb2(stats::runif(n), shape1, scale, shape2, shape3)
}


# -- Kappa-4 (via lmomco) -----------------------------------------------------
# Hosking (1994) four-parameter distribution encompassing GEV, GPD, logistic.
# params: xi (location), alpha (scale), kappa (shape1), h (shape2)

#' @export
#' @noRd
dkap4 <- function(x, xi, alpha, kappa, h, log = FALSE) {
  if (!requireNamespace("lmomco", quietly = TRUE))
    stop("Package 'lmomco' is required for the Kappa-4 distribution.", call. = FALSE)
  if (!is.finite(alpha) || alpha <= 0 || !is.finite(xi) ||
      !is.finite(kappa) || !is.finite(h)) {
    if (log) return(rep(-Inf, length(x))) else return(rep(0, length(x)))
  }
  para <- lmomco::vec2par(c(xi, alpha, kappa, h), type = "kap")
  out  <- lmomco::pdfkap(x, para)
  # IMPORTANT: do NOT use pmax(..., 1e-300) here.  lmomco::pdfkap returns
  # exactly 0 for points outside the bounded support of kappa4.  Clamping
  # those zeros to 1e-300 makes the log-likelihood finite even when the
  # support does not cover the data, so the MLE optimizer converges to
  # completely wrong parameters.  Returning -Inf for zero-density points
  # creates a proper barrier that pushes the optimizer to valid regions.
  if (log) ifelse(out > 0, log(out), -Inf) else out
}

#' @export
#' @noRd
pkap4 <- function(q, xi, alpha, kappa, h, lower.tail = TRUE, log.p = FALSE) {
  if (!requireNamespace("lmomco", quietly = TRUE))
    stop("Package 'lmomco' is required for the Kappa-4 distribution.", call. = FALSE)
  para <- lmomco::vec2par(c(xi, alpha, kappa, h), type = "kap")
  out  <- lmomco::cdfkap(q, para)
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qkap4 <- function(p, xi, alpha, kappa, h, lower.tail = TRUE, log.p = FALSE) {
  if (!requireNamespace("lmomco", quietly = TRUE))
    stop("Package 'lmomco' is required for the Kappa-4 distribution.", call. = FALSE)
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  para <- lmomco::vec2par(c(xi, alpha, kappa, h), type = "kap")
  lmomco::quakap(p, para)
}

#' @export
#' @noRd
rkap4 <- function(n, xi, alpha, kappa, h) {
  qkap4(stats::runif(n), xi, alpha, kappa, h)
}


# -- Lindley ------------------------------------------------------------------
# One-parameter mixture of Exp(theta) and Gamma(2, theta).
# f(x) = theta^2/(1+theta) * (1+x) * exp(-theta*x),  x>0

#' @export
#' @noRd
dlindley <- function(x, theta, log = FALSE) {
  ok  <- x >= 0 & theta > 0
  lp  <- rep(-Inf, length(x))
  if (any(ok))
    lp[ok] <- 2 * log(theta) - log1p(theta) + log1p(x[ok]) - theta * x[ok]
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
plindley <- function(q, theta, lower.tail = TRUE, log.p = FALSE) {
  out <- rep(0.0, length(q))
  ok  <- q > 0 & theta > 0
  if (any(ok))
    out[ok] <- 1 - (1 + theta * q[ok] / (1 + theta)) * exp(-theta * q[ok])
  out <- pmax(0, pmin(1, out))
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qlindley <- function(p, theta, lower.tail = TRUE, log.p = FALSE) {
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  vapply(p, function(pi) {
    if (pi <= 0) return(0)
    if (pi >= 1) return(Inf)
    tryCatch(
      stats::uniroot(function(x) plindley(x, theta) - pi,
                     interval = c(0, 1e6), tol = 1e-10)$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
}

#' @export
#' @noRd
rlindley <- function(n, theta) {
  qlindley(stats::runif(n), theta)
}


# -- Generalised Normal (Exponential Power) ------------------------------------
# f(x) = beta/(2*sigma*Gamma(1/beta)) * exp(-(|x-mu|/sigma)^beta)
# beta=2: Normal, beta=1: Laplace, beta->0: uniform, beta->Inf: uniform on [mu-sigma,mu+sigma]

#' @export
#' @noRd
dgnorm <- function(x, mu, sigma, beta, log = FALSE) {
  lp <- log(beta) - log(2) - log(sigma) - lgamma(1 / beta) -
        (abs(x - mu) / sigma)^beta
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pgnorm <- function(q, mu, sigma, beta, lower.tail = TRUE, log.p = FALSE) {
  z  <- abs(q - mu) / sigma
  p  <- 0.5 + sign(q - mu) * 0.5 *
        stats::pgamma(z^beta, shape = 1 / beta, rate = 1)
  p  <- pmax(0, pmin(1, p))
  if (!lower.tail) p <- 1 - p
  if (log.p) log(p) else p
}

#' @export
#' @noRd
qgnorm <- function(p, mu, sigma, beta, lower.tail = TRUE, log.p = FALSE) {
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  vapply(p, function(pi) {
    if (pi <= 0) return(-Inf)
    if (pi >= 1) return(Inf)
    tryCatch(
      stats::uniroot(function(x) pgnorm(x, mu, sigma, beta) - pi,
                     interval = c(mu - 100 * sigma, mu + 100 * sigma),
                     tol = 1e-10)$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
}

#' @export
#' @noRd
rgnorm <- function(n, mu, sigma, beta) {
  qgnorm(stats::runif(n), mu, sigma, beta)
}


# -- Dagum (Burr III) ----------------------------------------------------------
# F(x) = (1 + (x/b)^(-a))^(-p)
# f(x) = a*p/x * (x/b)^(a*p) / (1+(x/b)^a)^(p+1),  x>0
# params: shape1=p, scale=b, shape2=a

#' @export
#' @noRd
ddagum <- function(x, shape1, scale, shape2, log = FALSE) {
  p_s <- shape1; b <- scale; a <- shape2
  ok  <- x > 0
  lp  <- rep(-Inf, length(x))
  if (any(ok))
    lp[ok] <- log(a) + log(p_s) - log(x[ok]) +
              a * p_s * log(x[ok] / b) - (p_s + 1) * log1p((x[ok] / b)^a)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pdagum <- function(q, shape1, scale, shape2, lower.tail = TRUE, log.p = FALSE) {
  p_s <- shape1; b <- scale; a <- shape2
  out <- rep(0.0, length(q))
  ok  <- q > 0
  if (any(ok))
    out[ok] <- (1 + (q[ok] / b)^(-a))^(-p_s)
  out <- pmax(0, pmin(1, out))
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qdagum <- function(p, shape1, scale, shape2, lower.tail = TRUE, log.p = FALSE) {
  p_s <- shape1; b <- scale; a <- shape2
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  b * (p^(-1 / p_s) - 1)^(-1 / a)
}

#' @export
#' @noRd
rdagum <- function(n, shape1, scale, shape2) {
  qdagum(stats::runif(n), shape1, scale, shape2)
}


# -- Johnson SU ----------------------------------------------------------------
# Z = gamma + delta * arcsinh((X - xi) / lambda),  Z ~ N(0,1)
# params: gamma, delta (>0), xi, lambda (>0)

#' @export
#' @noRd
djohnsonsu <- function(x, gamma, delta, xi, lambda, log = FALSE) {
  z  <- (x - xi) / lambda
  w  <- gamma + delta * asinh(z)
  lp <- log(delta) - log(lambda) - 0.5 * log(2 * pi) -
        0.5 * log(1 + z^2) - 0.5 * w^2
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pjohnsonsu <- function(q, gamma, delta, xi, lambda,
                        lower.tail = TRUE, log.p = FALSE) {
  z  <- (q - xi) / lambda
  w  <- gamma + delta * asinh(z)
  out <- stats::pnorm(w)
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qjohnsonsu <- function(p, gamma, delta, xi, lambda,
                        lower.tail = TRUE, log.p = FALSE) {
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  z_std <- stats::qnorm(p)
  xi + lambda * sinh((z_std - gamma) / delta)
}

#' @export
#' @noRd
rjohnsonsu <- function(n, gamma, delta, xi, lambda) {
  qjohnsonsu(stats::runif(n), gamma, delta, xi, lambda)
}


# -- Asymmetric Laplace -------------------------------------------------------
# kappa > 0: asymmetry parameter (kappa=1 gives symmetric Laplace)
# Correct density (Kozubowski & Podgorski 2000):
#   f(x) = kappa/(sigma*(1+kappa^2)) * exp(-kappa*(x-mu)/sigma)  for x >= mu
#   f(x) = kappa/(sigma*(1+kappa^2)) * exp((x-mu)/(kappa*sigma)) for x < mu
# Both branches share the SAME normalisation constant kappa/(sigma*(1+kappa^2)).
# CDF:  F(q) = kappa^2/(1+kappa^2) * exp((q-mu)/(kappa*sigma)) for q <= mu
#        F(q) = 1 - 1/(1+kappa^2) * exp(-kappa*(q-mu)/sigma)   for q > mu

#' @export
#' @noRd
dalaplace <- function(x, mu, sigma, kappa, log = FALSE) {
  k2  <- kappa^2
  u   <- (x - mu) / sigma
  # log-normalisation: log(kappa) - log(sigma) - log(1+kappa^2)
  lp  <- log(kappa) - log(sigma) - log1p(k2) +
         ifelse(x >= mu, -kappa * u, u / kappa)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
palaplace <- function(q, mu, sigma, kappa, lower.tail = TRUE, log.p = FALSE) {
  k2  <- kappa^2
  p0  <- k2 / (1 + k2)       # P(X <= mu) = kappa^2/(1+kappa^2)
  u   <- (q - mu) / sigma
  out <- ifelse(q <= mu,
                p0 * exp(u / kappa),
                1 - 1 / (1 + k2) * exp(-kappa * u))
  out <- pmax(0, pmin(1, out))
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qalaplace <- function(p, mu, sigma, kappa, lower.tail = TRUE, log.p = FALSE) {
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  k2  <- kappa^2
  p0  <- k2 / (1 + k2)
  ifelse(p <= p0,
         mu + sigma * kappa * log(p * (1 + k2) / k2),
         mu - sigma / kappa * log((1 - p) * (1 + k2)))
}

#' @export
#' @noRd
ralaplace <- function(n, mu, sigma, kappa) {
  qalaplace(stats::runif(n), mu, sigma, kappa)
}


# -- Exponentiated Exponential -------------------------------------------------
# F(x) = (1 - exp(-lambda*x))^alpha,  x>0
# f(x) = alpha*lambda*exp(-lambda*x)*(1-exp(-lambda*x))^(alpha-1)

#' @export
#' @noRd
dexpexp <- function(x, alpha, lambda, log = FALSE) {
  ok  <- x > 0
  lp  <- rep(-Inf, length(x))
  if (any(ok))
    lp[ok] <- log(alpha) + log(lambda) - lambda * x[ok] +
               (alpha - 1) * log1p(-exp(-lambda * x[ok]))
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pexpexp <- function(q, alpha, lambda, lower.tail = TRUE, log.p = FALSE) {
  out <- ifelse(q > 0, (1 - exp(-lambda * q))^alpha, 0)
  out <- pmax(0, pmin(1, out))
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qexpexp <- function(p, alpha, lambda, lower.tail = TRUE, log.p = FALSE) {
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  -log1p(-p^(1 / alpha)) / lambda
}

#' @export
#' @noRd
rexpexp <- function(n, alpha, lambda) {
  qexpexp(stats::runif(n), alpha, lambda)
}


# -- NIG (Normal Inverse Gaussian) -- pure R via besselK ----------------------
# No external package required.  Uses the normal variance-mean mixture
# representation (Barndorff-Nielsen 1997) for simulation and the closed-form
# density via the modified Bessel function K_1 available in base R.
# params: mu (location), delta (scale), alpha (shape), beta (skew).
# Constraint: 0 <= |beta| < alpha, delta > 0.

#' @export
#' @noRd
dnig <- function(x, mu, delta, alpha, beta, log = FALSE) {
  # Hard validity check: NIG requires alpha > |beta| and delta > 0.
  # Return -Inf (log) or 0 (density) for invalid params so that the MLE
  # optimizer sees a proper barrier and does NOT wander into invalid regions.
  # (The old pmax(..., 0) clamp silently produced wrong finite values and
  #  caused the optimizer to converge to nonsense parameters.)
  if (!is.finite(alpha) || !is.finite(beta) || !is.finite(delta) ||
      alpha <= 0 || delta <= 0 || abs(beta) >= alpha) {
    if (log) return(rep(-Inf, length(x))) else return(rep(0, length(x)))
  }
  # Try GeneralizedHyperbolic first; fall back to pure-R
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    out <- tryCatch(
      GeneralizedHyperbolic::dnig(x, mu = mu, delta = delta,
                                  alpha = alpha, beta = beta),
      error = function(e) NULL
    )
    if (!is.null(out) && all(is.finite(out))) {
      if (log) return(log(pmax(out, 1e-300))) else return(out)
    }
  }
  # Pure-R: f(x) = (alpha*delta/pi) * K1(alpha*rho)/rho * exp(delta*gamma + beta*(x-mu))
  # where rho = sqrt(delta^2 + (x-mu)^2), gamma = sqrt(alpha^2 - beta^2)
  gam <- sqrt(alpha^2 - beta^2)   # guaranteed positive (checked above)
  rho <- sqrt(delta^2 + (x - mu)^2)
  # Use expon.scaled=TRUE: besselK(t, nu, TRUE) = K_nu(t)*exp(t)
  # => log K_1(alpha*rho) = log(besselK(alpha*rho, 1, TRUE)) - alpha*rho
  lK1 <- log(pmax(besselK(alpha * rho, nu = 1L, expon.scaled = TRUE), 1e-300)) -
         alpha * rho
  lp  <- log(alpha) + log(delta) - log(pi) - log(rho) + lK1 +
         delta * gam + beta * (x - mu)
  if (log) lp else exp(lp)
}

# Fast cumulative-integration CDF helper (used by pnig and pghyp).
# Sorts query points and integrates over successive short intervals rather than
# calling integrate(-Inf, x_i) n times independently.
# Work is O(range_of_data) instead of O(n * range_of_data), so n=1000 runs
# in roughly the same wall time as a single wide integration.
#' @keywords internal
.fast_cdf_cumulative <- function(q, dfun, lo, ...) {
  n   <- length(q)
  if (n == 0L) return(numeric(0L))
  ord <- order(q)
  qs  <- q[ord]
  out <- numeric(n)
  # Integral from lo to qs[1]
  cumval <- tryCatch(
    stats::integrate(dfun, lower = lo, upper = qs[1L], ...,
                     rel.tol = 1e-5, stop.on.error = FALSE)$value,
    error = function(e) 0
  )
  cumval       <- max(cumval, 0)
  out[ord[1L]] <- min(cumval, 1)
  # Successive short intervals
  for (i in seq_len(n - 1L) + 1L) {
    if (qs[i] <= qs[i - 1L]) {
      out[ord[i]] <- out[ord[i - 1L]]
      next
    }
    inc <- tryCatch(
      stats::integrate(dfun, lower = qs[i - 1L], upper = qs[i], ...,
                       rel.tol = 1e-5, stop.on.error = FALSE)$value,
      error = function(e) 0
    )
    cumval       <- cumval + max(inc, 0)
    out[ord[i]]  <- min(cumval, 1)
  }
  out
}

#' @export
#' @noRd
pnig <- function(q, mu, delta, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    out <- tryCatch(
      GeneralizedHyperbolic::pnig(q, mu = mu, delta = delta,
                                  alpha = alpha, beta = beta),
      error = function(e) NULL
    )
    if (!is.null(out) && all(is.finite(out))) {
      out <- pmax(0, pmin(1, out))   # clamp: goftest::ad.test errors on out-of-range
      if (!lower.tail) out <- 1 - out
      if (log.p)       out <- log(out)
      return(out)
    }
    # Package call failed or returned non-finite values; fall through to pure-R.
  }
  # Pure-R: fast cumulative integration.
  gamma_  <- sqrt(max(alpha^2 - beta^2, 1e-12))
  mean_   <- mu + beta * delta / gamma_
  sd_     <- sqrt(delta * alpha^2 / gamma_^3)
  lo      <- mean_ - 9 * sd_
  out <- .fast_cdf_cumulative(q, dnig, lo,
                               mu = mu, delta = delta,
                               alpha = alpha, beta = beta)
  out <- pmax(0, pmin(1, out))
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qnig <- function(p, mu, delta, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    if (log.p)       p <- exp(p)
    if (!lower.tail) p <- 1 - p
    return(GeneralizedHyperbolic::qnig(p, mu = mu, delta = delta,
                                       alpha = alpha, beta = beta))
  }
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  # Pure-R: uniroot inversion of pnig
  vapply(p, function(pi) {
    if (pi <= 0) return(-Inf)
    if (pi >= 1) return( Inf)
    span <- max(10 * delta, 5)
    lo   <- mu - span * 10
    hi   <- mu + span * 10
    tryCatch(
      stats::uniroot(function(x) pnig(x, mu, delta, alpha, beta) - pi,
                     interval = c(lo, hi), tol = 1e-8)$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
}

#' @export
#' @noRd
rnig <- function(n, mu, delta, alpha, beta) {
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE))
    return(GeneralizedHyperbolic::rnig(n, mu = mu, delta = delta,
                                       alpha = alpha, beta = beta))
  # Pure-R: normal variance-mean mixture
  # V ~ InvGaussian(delta/gamma, delta^2),  X = mu + beta*V + sqrt(V)*Z
  gam   <- sqrt(pmax(alpha^2 - beta^2, 1e-15))
  mu_ig <- delta / gam
  lam   <- delta^2
  # Michael-Schucany-Haas algorithm for InvGaussian
  y  <- stats::rnorm(n)^2
  x1 <- mu_ig + mu_ig^2 * y / (2 * lam) -
        mu_ig / (2 * lam) * sqrt(4 * mu_ig * lam * y + mu_ig^2 * y^2)
  z  <- stats::runif(n)
  v  <- ifelse(z <= mu_ig / (mu_ig + x1), x1, mu_ig^2 / x1)
  mu + beta * v + sqrt(v) * stats::rnorm(n)
}


# -- Wakeby (via lmomco, quantile-based) ---------------------------------------
# Q(F) = xi + alpha/beta*(1-(1-F)^beta) - gamma/delta*(1-(1-F)^(-delta))
# Exact PDF via quantile derivative: f(x) = 1/Q'(F(x))
# where Q'(F) = alpha*(1-F)^(beta-1) + gamma*(1-F)^(-delta-1)
# params: xi, alpha, beta, gamma, delta

#' @keywords internal
.wakeby_para <- function(xi, alpha, beta, gamma, delta) {
  lmomco::vec2par(c(xi, alpha, beta, gamma, delta), type = "wak")
}

#' Fast Wakeby CDF via Q-grid interpolation
#'
#' Pre-computes the quantile function on a fine grid and uses linear
#' interpolation to invert it.  This replaces n calls to uniroot (inside
#' lmomco::cdfwak) with a single vectorised quawak call + approx(), giving
#' ~100x speed-up for n = 1000 and making the GoF 10-second window safe.
#'
#' @keywords internal
.wakeby_cdf_fast <- function(x, para, n_grid = 2001L) {
  f_grid <- seq(1e-7, 1 - 1e-7, length.out = n_grid)
  q_grid <- tryCatch(lmomco::quawak(f_grid, para), error = function(e) NULL)
  if (is.null(q_grid) || length(q_grid) != n_grid ||
      !all(is.finite(q_grid)) || is.unsorted(q_grid)) {
    # Fallback: direct per-point inversion (slow but correct)
    return(tryCatch(lmomco::cdfwak(x, para),
                    error = function(e) rep(NA_real_, length(x))))
  }
  approx(q_grid, f_grid, xout = x, method = "linear",
         yleft = 0, yright = 1)$y
}

#' @export
#' @noRd
dwakeby <- function(x, xi, alpha, beta, gamma, delta, log = FALSE) {
  if (!requireNamespace("lmomco", quietly = TRUE))
    stop("Package 'lmomco' is required for the Wakeby distribution.", call. = FALSE)
  para <- .wakeby_para(xi, alpha, beta, gamma, delta)

  # Density via quantile-function derivative (no numerical differentiation):
  #   f(x) = 1 / Q'(F(x))  where  Q'(F) = alpha*(1-F)^{beta-1} + gamma*(1-F)^{-delta-1}
  # CDF F(x) obtained via fast grid interpolation of the quantile function.
  F   <- .wakeby_cdf_fast(x, para)
  ok  <- !is.na(F) & F > 0 & F < 1
  u   <- 1 - F
  qd  <- rep(0, length(x))
  if (any(ok)) {
    qd[ok] <- alpha * u[ok]^(beta - 1) + gamma * u[ok]^(-delta - 1)
  }
  out <- ifelse(ok & qd > 0, 1 / qd, 0)
  if (log) log(pmax(out, 1e-300)) else out
}

#' @export
#' @noRd
pwakeby <- function(q, xi, alpha, beta, gamma, delta,
                     lower.tail = TRUE, log.p = FALSE) {
  if (!requireNamespace("lmomco", quietly = TRUE))
    stop("Package 'lmomco' is required for the Wakeby distribution.", call. = FALSE)
  para <- .wakeby_para(xi, alpha, beta, gamma, delta)
  out  <- .wakeby_cdf_fast(q, para)
  out[is.na(out)] <- 0   # treat CDF failures as F=0
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qwakeby <- function(p, xi, alpha, beta, gamma, delta,
                     lower.tail = TRUE, log.p = FALSE) {
  if (!requireNamespace("lmomco", quietly = TRUE))
    stop("Package 'lmomco' is required for the Wakeby distribution.", call. = FALSE)
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  para <- .wakeby_para(xi, alpha, beta, gamma, delta)
  lmomco::quawak(p, para)
}

#' @export
#' @noRd
rwakeby <- function(n, xi, alpha, beta, gamma, delta) {
  qwakeby(stats::runif(n), xi, alpha, beta, gamma, delta)
}


# -- Generalised Hyperbolic -- pure R via besselK -----------------------------
# No external package required.  Density via the GHyp formula using K_{lambda-0.5}
# from base R besselK().  Simulation via GIG normal variance-mean mixture.
# params: lambda (index), alpha (shape), beta (skew), delta (scale), mu (loc)
# Constraint: alpha > |beta|, delta > 0.

#' @export
#' @noRd
dghyp <- function(x, lambda, alpha, beta, delta, mu, log = FALSE) {
  # Hard validity check: GHyp requires alpha > |beta| and delta > 0.
  # Same issue as dnig: the old pmax(alpha^2-beta^2, 1e-15) clamp silently
  # computed wrong finite density in invalid regions, misleading the optimizer.
  if (!is.finite(alpha) || !is.finite(beta) || !is.finite(delta) ||
      alpha <= 0 || delta <= 0 || abs(beta) >= alpha) {
    if (log) return(rep(-Inf, length(x))) else return(rep(0, length(x)))
  }
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    out <- tryCatch(
      GeneralizedHyperbolic::dghyp(x, mu = mu, delta = delta,
                                   alpha = alpha, beta = beta,
                                   lambda = lambda),
      error = function(e) NULL
    )
    if (!is.null(out) && all(is.finite(out))) {
      if (log) return(log(pmax(out, 1e-300))) else return(out)
    }
  }
  # Pure-R GHyp density (Barndorff-Nielsen 1977)
  # f(x) = C * (d^2+(x-mu)^2)^((lam-0.5)/2) * K_{lam-0.5}(alpha*rho) * exp(beta*(x-mu))
  # C = (alpha^2-beta^2)^(lam/2) / (sqrt(2*pi)*alpha^(lam-0.5)*delta^lam*K_lam(delta*sqrt(alpha^2-beta^2)))
  gam  <- sqrt(alpha^2 - beta^2)   # guaranteed positive (checked above)
  nu_c <- lambda         # order for normalisation K
  nu_d <- lambda - 0.5  # order for density K
  rho  <- sqrt(delta^2 + (x - mu)^2)
  # log normalisation constant (stable via expon.scaled)
  lKc  <- log(pmax(besselK(delta * gam, nu = nu_c, expon.scaled = TRUE), 1e-300)) -
          delta * gam
  lC   <- (lambda / 2) * log(alpha^2 - beta^2) -
          (lambda - 0.5) * log(alpha) - lambda * log(delta) -
          0.5 * log(2 * pi) - lKc
  # log density K term (stable)
  lKd  <- log(pmax(besselK(alpha * rho, nu = nu_d, expon.scaled = TRUE), 1e-300)) -
          alpha * rho
  lp   <- lC + (nu_d / 2) * log(delta^2 + (x - mu)^2) + lKd + beta * (x - mu)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pghyp <- function(q, lambda, alpha, beta, delta, mu,
                   lower.tail = TRUE, log.p = FALSE) {
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    out <- tryCatch(
      GeneralizedHyperbolic::pghyp(q, mu = mu, delta = delta,
                                   alpha = alpha, beta = beta,
                                   lambda = lambda),
      error = function(e) NULL
    )
    if (!is.null(out) && all(is.finite(out))) {
      out <- pmax(0, pmin(1, out))   # clamp: goftest::ad.test errors on out-of-range
      if (!lower.tail) out <- 1 - out
      if (log.p)       out <- log(out)
      return(out)
    }
    # Package call failed or returned non-finite values; fall through to pure-R.
  }
  # Pure-R: fast cumulative integration.
  gamma_  <- sqrt(max(alpha^2 - beta^2, 1e-12))
  span    <- max(10 * delta / gamma_, 5 * abs(delta))
  lo      <- mu + beta * delta / gamma_ - span
  out <- .fast_cdf_cumulative(q, dghyp, lo,
                               lambda = lambda, alpha = alpha, beta = beta,
                               delta = delta, mu = mu)
  out <- pmax(0, pmin(1, out))
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qghyp <- function(p, lambda, alpha, beta, delta, mu,
                   lower.tail = TRUE, log.p = FALSE) {
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    if (log.p)       p <- exp(p)
    if (!lower.tail) p <- 1 - p
    return(GeneralizedHyperbolic::qghyp(p, mu = mu, delta = delta,
                                        alpha = alpha, beta = beta,
                                        lambda = lambda))
  }
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  # Pure-R: uniroot on pghyp
  vapply(p, function(pi) {
    if (pi <= 0) return(-Inf)
    if (pi >= 1) return( Inf)
    span <- max(10 * delta, 5)
    tryCatch(
      stats::uniroot(function(x) pghyp(x, lambda, alpha, beta, delta, mu) - pi,
                     interval = c(mu - span * 10, mu + span * 10),
                     tol = 1e-8)$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
}

#' @export
#' @noRd
rghyp <- function(n, lambda, alpha, beta, delta, mu) {
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE))
    return(GeneralizedHyperbolic::rghyp(n, mu = mu, delta = delta,
                                        alpha = alpha, beta = beta,
                                        lambda = lambda))
  # Pure-R: GIG normal variance-mean mixture
  # V ~ GIG(lambda, delta^2, alpha^2-beta^2); X = mu + beta*V + sqrt(V)*Z
  # GIG simulation via ratio-of-uniforms (Dagpunar 1989, simplified for lambda)
  gam <- sqrt(pmax(alpha^2 - beta^2, 1e-15))
  v   <- .rgig_simple(n, lambda = lambda, chi = delta^2, psi = gam^2)
  mu + beta * v + sqrt(v) * stats::rnorm(n)
}

# Simple GIG sampler (Generalised Inverse Gaussian) via Devroye (2014) method
# Works for lambda >= 0.5 and general lambda via transformations.
# For our typical GHyp use-cases (lambda near -0.5) this is sufficient.
#' @keywords internal
.rgig_simple <- function(n, lambda, chi, psi) {
  # GIG(lambda, chi, psi): mean = sqrt(chi/psi)*K_{lam+1}(sqrt(chi*psi))/K_lam(sqrt(chi*psi))
  # Use accept-reject around an approximation.  For robustness, use the
  # mode-based squeeze (Hoermann & Leydold 2014 R implementation logic).
  omega <- sqrt(chi * psi)
  alpha <- sqrt(omega^2 + lambda^2) - lambda  # mode (approx for |lambda| small)
  mode_v <- (sqrt((lambda - 1)^2 + omega^2) + (lambda - 1)) / omega
  if (!is.finite(mode_v) || mode_v <= 0) mode_v <- 1
  # Simple fallback: if lambda = -0.5 (NIG case), use InvGaussian
  if (abs(lambda + 0.5) < 1e-4 && is.finite(chi) && is.finite(psi)) {
    # InvGaussian(mu_ig, lam_ig): mu_ig = sqrt(chi/psi), lam_ig = chi
    mu_ig <- sqrt(chi / psi)
    lam_ig <- chi
    y  <- stats::rnorm(n)^2
    x1 <- mu_ig + mu_ig^2 * y / (2 * lam_ig) -
          mu_ig / (2 * lam_ig) * sqrt(4 * mu_ig * lam_ig * y + mu_ig^2 * y^2)
    z  <- stats::runif(n)
    return(ifelse(z <= mu_ig / (mu_ig + x1), x1, mu_ig^2 / x1))
  }
  # General case: ratio-of-uniforms (simplified, may be slow for extreme params)
  log_f <- function(v) {
    if (any(v <= 0)) return(rep(-Inf, length(v)))
    (lambda - 1) * log(v) - 0.5 * (chi / v + psi * v)
  }
  # Bounding box for ratio-of-uniforms
  h_mode <- mode_v
  u_max  <- sqrt(exp(log_f(h_mode)))
  vl     <- max(h_mode * (1 - 2 / sqrt(n + 4)), 1e-10)
  vr     <- h_mode * (1 + 2 / sqrt(n + 4)) * 4
  w_max  <- max(sqrt(vl^2 * exp(log_f(vl))),
                sqrt(vr^2 * exp(log_f(vr))), na.rm = TRUE)
  result <- numeric(n)
  done   <- 0L
  while (done < n) {
    u <- stats::runif(n - done, 0, u_max)
    w <- stats::runif(n - done, 0, w_max)
    x <- w / u
    ok <- x > 0 & log(u) <= 0.5 * log_f(x)
    take <- min(sum(ok), n - done)
    if (take > 0) {
      result[(done + 1):(done + take)] <- x[ok][seq_len(take)]
      done <- done + take
    }
  }
  result
}


# -- Truncated Normal -- pure R, no external package required -----------------
# f(x; mu,sigma,a,b) = phi((x-mu)/sigma) / (sigma*(Phi((b-mu)/sigma)-Phi((a-mu)/sigma)))
# Everything uses base-R dnorm/pnorm/qnorm; truncnorm package used only as
# a faster/more-accurate enhancement when available.
# params: mean (mu), sd (sigma), a (lower bound), b (upper bound)

#' @export
#' @noRd
dtruncnorm <- function(x, mean, sd, a, b, log = FALSE) {
  if (requireNamespace("truncnorm", quietly = TRUE)) {
    out <- truncnorm::dtruncnorm(x, a = a, b = b, mean = mean, sd = sd)
    if (log) return(log(pmax(out, 1e-300))) else return(out)
  }
  # Pure-R: f(x) = dnorm(x, mean, sd) / (pnorm(b, mean, sd) - pnorm(a, mean, sd))
  # on [a, b]; 0 outside.
  Z   <- stats::pnorm(b, mean, sd) - stats::pnorm(a, mean, sd)
  lp  <- ifelse(x >= a & x <= b,
                stats::dnorm(x, mean, sd, log = TRUE) - log(pmax(Z, 1e-300)),
                -Inf)
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
ptruncnorm <- function(q, mean, sd, a, b, lower.tail = TRUE, log.p = FALSE) {
  if (requireNamespace("truncnorm", quietly = TRUE)) {
    out <- truncnorm::ptruncnorm(q, a = a, b = b, mean = mean, sd = sd)
    if (!lower.tail) out <- 1 - out
    if (log.p)       out <- log(out)
    return(out)
  }
  # Pure-R: F(x) = (Phi((x-mu)/sigma) - Phi((a-mu)/sigma)) / (Phi((b-mu)/sigma) - Phi((a-mu)/sigma))
  Fa  <- stats::pnorm(a, mean, sd)
  Fb  <- stats::pnorm(b, mean, sd)
  Z   <- pmax(Fb - Fa, 1e-300)
  out <- pmin(pmax((stats::pnorm(q, mean, sd) - Fa) / Z, 0), 1)
  out[q < a] <- 0; out[q > b] <- 1
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qtruncnorm <- function(p, mean, sd, a, b, lower.tail = TRUE, log.p = FALSE) {
  if (requireNamespace("truncnorm", quietly = TRUE)) {
    if (log.p)       p <- exp(p)
    if (!lower.tail) p <- 1 - p
    return(truncnorm::qtruncnorm(p, a = a, b = b, mean = mean, sd = sd))
  }
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  # Pure-R: Q(p) = Phi^{-1}(Phi(a*) + p*(Phi(b*)-Phi(a*))) * sigma + mean
  Fa  <- stats::pnorm(a, mean, sd)
  Fb  <- stats::pnorm(b, mean, sd)
  stats::qnorm(Fa + p * (Fb - Fa), mean, sd)
}

#' @export
#' @noRd
rtruncnorm <- function(n, mean, sd, a, b) {
  if (requireNamespace("truncnorm", quietly = TRUE))
    return(truncnorm::rtruncnorm(n, a = a, b = b, mean = mean, sd = sd))
  # Pure-R: quantile inversion (exact)
  u <- stats::runif(n)
  qtruncnorm(u, mean = mean, sd = sd, a = a, b = b)
}


# -- Nakagami -----------------------------------------------------------------
# f(x) = 2*m^m/(Gamma(m)*Omega^m) * x^(2m-1) * exp(-m*x^2/Omega),  x>0
# F(x) = pgamma(m*x^2/Omega, m)

#' @export
#' @noRd
dnakagami <- function(x, m, Omega, log = FALSE) {
  ok  <- x > 0
  lp  <- rep(-Inf, length(x))
  if (any(ok))
    lp[ok] <- log(2) + m * log(m) - lgamma(m) - m * log(Omega) +
               (2 * m - 1) * log(x[ok]) - m * x[ok]^2 / Omega
  if (log) lp else exp(lp)
}

#' @export
#' @noRd
pnakagami <- function(q, m, Omega, lower.tail = TRUE, log.p = FALSE) {
  out <- rep(0.0, length(q))
  ok  <- q > 0
  if (any(ok))
    out[ok] <- stats::pgamma(m * q[ok]^2 / Omega, shape = m, rate = 1)
  if (!lower.tail) out <- 1 - out
  if (log.p)       out <- log(out)
  out
}

#' @export
#' @noRd
qnakagami <- function(p, m, Omega, lower.tail = TRUE, log.p = FALSE) {
  if (log.p)       p <- exp(p)
  if (!lower.tail) p <- 1 - p
  sqrt(Omega / m * stats::qgamma(p, shape = m, rate = 1))
}

#' @export
#' @noRd
rnakagami <- function(n, m, Omega) {
  qnakagami(stats::runif(n), m, Omega)
}


# ==============================================================================
# Central d/p/q/r override table
# ------------------------------------------------------------------------------
# All distributions whose d/p/q/r functions are NOT on the search path are
# registered here.  .lookup_dpqr() checks this table first before falling back
# to match.fun() / getExportedValue().
#
# IMPORTANT: this list must be defined AFTER all function bodies above so that
# every name referenced here already exists in the package namespace.
# ==============================================================================

.FITVERSE_DPQR_OVERRIDES <- list(
  # -- GEV ---------------------------------------------------------------------
  dgev               = .dgev_internal,
  pgev               = .pgev_internal,
  qgev               = .qgev_internal,
  rgev               = .rgev_internal,
  # -- Gumbel ------------------------------------------------------------------
  dgumbel            = .dgumbel_internal,
  pgumbel            = .pgumbel_internal,
  qgumbel            = .qgumbel_internal,
  rgumbel            = .rgumbel_internal,
  # -- GPD ---------------------------------------------------------------------
  dgpd               = .dgpd_internal,
  pgpd               = .pgpd_internal,
  qgpd               = .qgpd_internal,
  rgpd               = .rgpd_internal,
  # -- Pearson III -------------------------------------------------------------
  dpearson3          = .dpearson3_internal,
  ppearson3          = .ppearson3_internal,
  qpearson3          = .qpearson3_internal,
  rpearson3          = .rpearson3_internal,
  # -- Log-Pearson III ---------------------------------------------------------
  dlpearson3         = .dlpearson3_internal,
  plpearson3         = .plpearson3_internal,
  qlpearson3         = .qlpearson3_internal,
  rlpearson3         = .rlpearson3_internal,
  # -- Generalised Logistic (GLO) -----------------------------------------------
  dglo               = .dglo_internal,
  pglo               = .pglo_internal,
  qglo               = .qglo_internal,
  rglo               = .rglo_internal,
  # -- Weibull 3P --------------------------------------------------------------
  dweibull3p         = dweibull3p,
  pweibull3p         = pweibull3p,
  qweibull3p         = qweibull3p,
  rweibull3p         = rweibull3p,
  # -- Right Triangular --------------------------------------------------------
  drtriangle         = drtriangle,
  prtriangle         = prtriangle,
  qrtriangle         = qrtriangle,
  rrtriangle         = rrtriangle,
  # -- Kumaraswamy -------------------------------------------------------------
  dkumaraswamy       = dkumaraswamy,
  pkumaraswamy       = pkumaraswamy,
  qkumaraswamy       = qkumaraswamy,
  rkumaraswamy       = rkumaraswamy,
  # -- Inverse Gaussian --------------------------------------------------------
  dinvgaussian       = dinvgaussian,
  pinvgaussian       = pinvgaussian,
  qinvgaussian       = qinvgaussian,
  rinvgaussian       = rinvgaussian,
  # -- Lomax -------------------------------------------------------------------
  dlomax             = dlomax,
  plomax             = plomax,
  qlomax             = qlomax,
  rlomax             = rlomax,
  # -- Laplace -----------------------------------------------------------------
  dlaplace           = dlaplace,
  plaplace           = plaplace,
  qlaplace           = qlaplace,
  rlaplace           = rlaplace,
  # -- Generalised Gamma -------------------------------------------------------
  dgengamma          = dgengamma,
  pgengamma          = pgengamma,
  qgengamma          = qgengamma,
  rgengamma          = rgengamma,
  # -- Gompertz ----------------------------------------------------------------
  dgompertz          = dgompertz,
  pgompertz          = pgompertz,
  qgompertz          = qgompertz,
  rgompertz          = rgompertz,
  # -- Birnbaum-Saunders -------------------------------------------------------
  dbirnbaum_saunders = dbirnbaum_saunders,
  pbirnbaum_saunders = pbirnbaum_saunders,
  qbirnbaum_saunders = qbirnbaum_saunders,
  rbirnbaum_saunders = rbirnbaum_saunders,
  # -- Student-t Location-Scale ------------------------------------------------
  dstudent_t         = dstudent_t,
  pstudent_t         = pstudent_t,
  qstudent_t         = qstudent_t,
  rstudent_t         = rstudent_t,
  # -- Rayleigh ----------------------------------------------------------------
  drayleigh          = drayleigh,
  prayleigh          = prayleigh,
  qrayleigh          = qrayleigh,
  rrayleigh          = rrayleigh,
  # -- Half-Normal -------------------------------------------------------------
  dhalfnormal        = dhalfnormal,
  phalfnormal        = phalfnormal,
  qhalfnormal        = qhalfnormal,
  rhalfnormal        = rhalfnormal,
  # -- Exponentiated Weibull ---------------------------------------------------
  dexpweibull        = dexpweibull,
  pexpweibull        = pexpweibull,
  qexpweibull        = qexpweibull,
  rexpweibull        = rexpweibull,
  # -- Gamma 3P ----------------------------------------------------------------
  dgamma3            = dgamma3,
  pgamma3            = pgamma3,
  qgamma3            = qgamma3,
  rgamma3            = rgamma3,
  # -- Log-Normal 3P -----------------------------------------------------------
  dlnorm3            = dlnorm3,
  plnorm3            = plnorm3,
  qlnorm3            = qlnorm3,
  rlnorm3            = rlnorm3,
  # -- Skew-Normal (sn package) -------------------------------------------------
  # Explicit wrappers ensure the sn namespace is accessed directly, avoiding
  # match.fun() failures when sn is loaded but not attached to the search path.
  # Pure-R fallbacks use only base-R stats:: functions so GoF always computes.
  dsn = function(x, xi = 0, omega = 1, alpha = 0, log = FALSE) {
          if (requireNamespace("sn", quietly = TRUE)) {
            out <- tryCatch(
              as.numeric(sn::dsn(x, xi = xi, omega = omega, alpha = alpha, log = log)),
              error = function(e) NULL)
            if (!is.null(out) && all(is.finite(out))) return(out)
          }
          # Pure-R: f(x) = (2/omega)*phi(z)*Phi(alpha*z), z=(x-xi)/omega
          z  <- (x - xi) / omega
          lp <- stats::dnorm(z, log = TRUE) - log(omega) + log(2) +
                stats::pnorm(alpha * z, log.p = TRUE)
          if (log) lp else exp(lp)
        },
  psn = function(q, xi = 0, omega = 1, alpha = 0,
                 lower.tail = TRUE, log.p = FALSE) {
          if (requireNamespace("sn", quietly = TRUE)) {
            out <- tryCatch(
              as.numeric(sn::psn(q, xi = xi, omega = omega, alpha = alpha,
                                 lower.tail = lower.tail, log.p = log.p)),
              error = function(e) NULL)
            if (!is.null(out) && all(is.finite(out))) {
              if (!log.p) out <- pmax(0, pmin(1, out))
              return(out)
            }
          }
          # Pure-R fallback via cumulative integration using the pure-R dsn above.
          dsn_pure <- function(x, xi, omega, alpha) {
            z <- (x - xi) / omega
            exp(stats::dnorm(z, log = TRUE) - log(omega) + log(2) +
                stats::pnorm(alpha * z, log.p = TRUE))
          }
          lo  <- xi - 8 * omega
          out <- .fast_cdf_cumulative(q, dsn_pure, lo,
                                      xi = xi, omega = omega, alpha = alpha)
          out <- pmax(0, pmin(1, out))
          if (!lower.tail) out <- 1 - out
          if (log.p)       out <- log(out)
          out
        },
  qsn = function(p,  xi = 0, omega = 1, alpha = 0,
                 lower.tail = TRUE, log.p = FALSE)
          as.numeric(sn::qsn(p, xi = xi, omega = omega, alpha = alpha,
                             lower.tail = lower.tail, log.p = log.p)),
  rsn = function(n,  xi = 0, omega = 1, alpha = 0)
          as.numeric(sn::rsn(n, xi = xi, omega = omega, alpha = alpha)),
  # -- GB2 (Generalised Beta of the Second Kind) --------------------------------
  dgb2               = dgb2,
  pgb2               = pgb2,
  qgb2               = qgb2,
  rgb2               = rgb2,
  # -- Kappa-4 -----------------------------------------------------------------
  dkap4              = dkap4,
  pkap4              = pkap4,
  qkap4              = qkap4,
  rkap4              = rkap4,
  # -- Lindley -----------------------------------------------------------------
  dlindley           = dlindley,
  plindley           = plindley,
  qlindley           = qlindley,
  rlindley           = rlindley,
  # -- Generalised Normal ------------------------------------------------------
  dgnorm             = dgnorm,
  pgnorm             = pgnorm,
  qgnorm             = qgnorm,
  rgnorm             = rgnorm,
  # -- Dagum -------------------------------------------------------------------
  ddagum             = ddagum,
  pdagum             = pdagum,
  qdagum             = qdagum,
  rdagum             = rdagum,
  # -- Johnson SU --------------------------------------------------------------
  djohnsonsu         = djohnsonsu,
  pjohnsonsu         = pjohnsonsu,
  qjohnsonsu         = qjohnsonsu,
  rjohnsonsu         = rjohnsonsu,
  # -- Asymmetric Laplace ------------------------------------------------------
  dalaplace          = dalaplace,
  palaplace          = palaplace,
  qalaplace          = qalaplace,
  ralaplace          = ralaplace,
  # -- Exponentiated Exponential -----------------------------------------------
  dexpexp            = dexpexp,
  pexpexp            = pexpexp,
  qexpexp            = qexpexp,
  rexpexp            = rexpexp,
  # -- NIG (Normal Inverse Gaussian) -------------------------------------------
  dnig               = dnig,
  pnig               = pnig,
  qnig               = qnig,
  rnig               = rnig,
  # -- Wakeby ------------------------------------------------------------------
  dwakeby            = dwakeby,
  pwakeby            = pwakeby,
  qwakeby            = qwakeby,
  rwakeby            = rwakeby,
  # -- Generalised Hyperbolic --------------------------------------------------
  dghyp              = dghyp,
  pghyp              = pghyp,
  qghyp              = qghyp,
  rghyp              = rghyp,
  # -- Truncated Normal --------------------------------------------------------
  dtruncnorm         = dtruncnorm,
  ptruncnorm         = ptruncnorm,
  qtruncnorm         = qtruncnorm,
  rtruncnorm         = rtruncnorm,
  # -- Nakagami ----------------------------------------------------------------
  dnakagami          = dnakagami,
  pnakagami          = pnakagami,
  qnakagami          = qnakagami,
  rnakagami          = rnakagami
)


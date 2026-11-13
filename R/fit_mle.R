# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : fit_mle.R
# Description: Maximum Likelihood Estimation (MLE) for all 52
#              distributions in the FitVerse registry.
#
#              Strategy:
#               - fitdistrplus::fitdist() handles all stats and
#                 actuar distributions automatically.
#               - evd distributions (Gumbel, GEV) use a custom
#                 wrapper around stats::optim().
#               - The sn (Skew-Normal) distribution uses
#                 sn::selm() when the 'sn' package is available.
#               - Triangular and PERT use fitdistrplus with the
#                 mc2d d-function registered in the search path.
#
#              Return value (all fitters):
#               A named list with fields:
#                 dist_key   : character key from registry
#                 method     : "MLE"
#                 params     : named numeric vector of estimates
#                 loglik     : numeric log-likelihood
#                 aic        : AIC
#                 bic        : BIC
#                 n_params   : integer number of free parameters
#                 n          : sample size used
#                 converged  : logical
#                 fit_obj    : the raw fit object (for diagnostics)
#
#              Returns NULL silently if fitting fails.
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


# -- Main dispatcher -----------------------------------------------------------

#' Fit a single distribution by Maximum Likelihood Estimation
#'
#' @param x        Numeric vector of clean (finite) data.
#' @param dist_key Character key from the FitVerse registry (e.g. "gamma").
#' @return Named list with fit results (see file header), or NULL on failure.
#' @keywords internal
fit_mle <- function(x, dist_key) {

  entry <- get_dist_entry(dist_key)

  # Delegate to specialised fitters for distributions that cannot use fitdistrplus
  result <- switch(dist_key,
    # Symmetric / unbounded
    skewnormal        = .mle_sn(x, entry),
    laplace           = .mle_laplace(x),
    # Right-skewed (positive support)
    weibull3p         = .mle_weibull3p(x, entry),
    pearson3          = .mle_pearson3(x, entry),
    lpearson3         = .mle_lpearson3(x, entry),
    invgaussian       = .mle_invgaussian(x),
    gengamma          = .mle_gengamma(x),
    gompertz          = .mle_gompertz(x),
    birnbaum_saunders = .mle_birnbaum_saunders(x),
    rayleigh          = .mle_rayleigh(x),
    halfnormal        = .mle_halfnormal(x),
    exponential       = .mle_exponential(x),
    expweibull        = .mle_expweibull(x, entry),
    gamma3            = .mle_gamma3(x),
    lnorm3            = .mle_lnorm3(x),
    # Heavy-tailed
    lomax             = .mle_custom_optim(x, "lomax",      entry),
    # actuar distributions: fitdistrplus cannot find actuar d/p/q functions via
    # get() because they live in the actuar namespace (not on the search path).
    # Route through .mle_custom_optim which uses .lookup_dpqr -> getExportedValue.
    loglogistic  = .mle_custom_optim(x, "loglogistic",  entry),
    pareto       = .mle_custom_optim(x, "pareto",       entry),
    burr         = .mle_burr(x, entry),
    invgamma     = .mle_custom_optim(x, "invgamma",     entry),
    invweibull   = .mle_invweibull(x, entry),
    paralogistic = .mle_custom_optim(x, "paralogistic", entry),
    # Extreme-value
    gumbel            = .mle_evd_gumbel(x, entry),
    gev               = .mle_evd_gev(x, entry),
    gpd               = .mle_gpd(x, entry),
    glo               = .mle_glo(x, entry),
    # Bounded
    triangular        = .mle_triangular(x),
    rtriangle         = .mle_rtriangle(x),
    pert              = .mle_pert(x),
    kumaraswamy       = .mle_custom_optim(x, "kumaraswamy", entry),
    student_t         = .mle_student_t(x, entry),
    # Extended families (entries 40-52)
    gb2               = .mle_gb2(x, entry),
    kappa4            = .mle_custom_optim(x, "kappa4",      entry),
    lindley           = .mle_lindley(x),
    gnorm             = .mle_custom_optim(x, "gnorm",       entry),
    dagum             = .mle_custom_optim(x, "dagum",       entry),
    johnsonsu         = .mle_custom_optim(x, "johnsonsu",   entry),
    alaplace          = .mle_custom_optim(x, "alaplace",    entry),
    expexp            = .mle_expexp(x),
    nig               = .mle_nig(x, entry),
    wakeby            = NULL,   # LMOM only
    ghyp              = .mle_ghyp(x, entry),
    truncnorm         = .mle_truncnorm(x),
    nakagami          = .mle_nakagami(x),
    # Default: use fitdistrplus for all remaining distributions (stats package only)
    .mle_fitdistrplus(x, dist_key, entry)
  )

  # Global sentinel guard: if any fitter returns a result whose log-likelihood
  # is at or below the sentinel value (-1e300), the optimizer was stuck and
  # never found a valid fit.  Return NULL so no 2e+300 AIC appears in the table.
  if (!is.null(result) && !is.null(result$loglik) &&
      (!is.finite(result$loglik) || result$loglik < -1e290)) {
    return(NULL)
  }

  result
}


# -- fitdistrplus-based MLE (default route for standard distributions) --------

#' MLE via fitdistrplus::fitdist
#'
#' This covers all distributions whose d/p/q/r functions are available on the
#' search path via stats or actuar: normal, lognormal, gamma, exponential,
#' weibull, beta, logistic, cauchy, uniform, loglogistic, pareto, burr,
#' invgamma, invweibull, paralogistic (15 distributions).  All others have
#' dedicated fitters and are NOT routed through this function.
#'
#' @keywords internal
.mle_fitdistrplus <- function(x, dist_key, entry) {

  n     <- length(x)
  start <- entry$start_fun(x)

  # fitdistrplus requires BOTH lower and upper (same length as npar) or NEITHER.
  # Pass the FULL named vectors - fitdistrplus internally replaces [U+00B1]Inf with
  # [U+00B1].Machine$double.xmax before calling L-BFGS-B.
  # Stripping one side to a shorter vector causes rep_len(upper, npar) to fail.
  lower_raw <- entry$lower
  upper_raw <- entry$upper
  has_constraints <- any(is.finite(lower_raw)) || any(is.finite(upper_raw))
  lower_arg <- if (has_constraints) lower_raw else NULL
  upper_arg <- if (has_constraints) upper_raw else NULL

  # Make d/p/q functions from actuar or mc2d visible to fitdistrplus.
  # fitdistrplus uses get() on the search path; Imports packages are not
  # attached, so we assign the functions into this frame for lookup.
  .expose_dist_fns(entry$package, entry$r_name)

  fit <- safe_fit(
    # suppressMessages silences the <simpleError...> that try() inside
    # fitdistrplus prints to stderr before re-throwing the condition.
    # safe_fit's tryCatch still catches the re-thrown error normally.
    suppressMessages(fitdistrplus::fitdist(
      data   = x,
      distr  = entry$r_name,
      method = "mle",
      start  = start,
      lower  = lower_arg,
      upper  = upper_arg
    )),
    dist_name = entry$display_name,
    method    = "MLE"
  )

  if (is.null(fit)) return(NULL)

  params <- fit$estimate
  ll     <- fit$loglik
  k      <- length(params)

  list(
    dist_key  = dist_key,
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = (fit$convergence == 0),
    fit_obj   = fit
  )
}


# -- Gumbel MLE (analytical profile-likelihood, no optim convergence issues) ---

#' MLE for the Gumbel distribution via analytical profile likelihood.
#' The MLE scale beta solves the 1-D fixed-point equation
#'   beta = mean(x) - sum(x * w) / sum(w),  w = exp(-x/beta)
#' (Prescott & Walden, 1980). Given beta, mu = -beta * log(mean(exp(-x/beta))).
#' This avoids evd::fgev's optim iteration-limit false non-convergence.
#' @keywords internal
.mle_evd_gumbel <- function(x, entry) {

  n <- length(x)

  # Profile-likelihood fixed-point function for scale (beta)
  beta_eq <- function(beta) {
    if (beta <= 0) return(1e300)
    w   <- exp(-x / beta)
    sw  <- sum(w)
    if (!is.finite(sw) || sw == 0) return(1e300)
    mean(x) - sum(x * w) / sw - beta
  }

  # Bracket: beta is in (0, sd(x)*10)
  beta_lo <- 1e-6
  beta_hi <- stats::sd(x) * 20
  if (!is.finite(beta_hi) || beta_hi <= beta_lo) return(NULL)

  # Check that root exists in bracket
  lo_val <- tryCatch(beta_eq(beta_lo), error = function(e) NA)
  hi_val <- tryCatch(beta_eq(beta_hi), error = function(e) NA)
  if (is.na(lo_val) || is.na(hi_val)) return(NULL)
  if (lo_val * hi_val > 0) {
    # Root not bracketed - expand upper bound
    beta_hi <- beta_hi * 10
    hi_val  <- tryCatch(beta_eq(beta_hi), error = function(e) NA)
    if (is.na(hi_val) || lo_val * hi_val > 0) return(NULL)
  }

  beta_hat <- tryCatch(
    stats::uniroot(beta_eq, lower = beta_lo, upper = beta_hi,
                   tol = .Machine$double.eps^0.5)$root,
    error = function(e) NA_real_
  )
  if (is.na(beta_hat) || beta_hat <= 0) return(NULL)

  # Closed-form mu given beta
  w       <- exp(-x / beta_hat)
  mu_hat  <- -beta_hat * log(mean(w))
  if (!is.finite(mu_hat)) return(NULL)

  params  <- c(loc = mu_hat, scale = beta_hat)
  ld      <- evd::dgumbel(x, loc = mu_hat, scale = beta_hat, log = TRUE)
  if (any(!is.finite(ld))) return(NULL)
  ll      <- sum(ld)
  k       <- 2L

  list(
    dist_key  = "gumbel",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = TRUE,           # analytical solution always converges
    fit_obj   = NULL
  )
}


# -- GEV MLE (via evd package) ------------------------------------------------

#' MLE for the Generalised Extreme Value distribution using evd::fgev
#' @keywords internal
.mle_evd_gev <- function(x, entry) {

  if (!has_pkg("evd", "GEV distribution fitting")) return(NULL)

  n <- length(x)

  fit <- safe_fit(
    evd::fgev(x, control = list(maxit = 2000, reltol = 1e-8)),
    dist_name = "Generalised Extreme Value",
    method    = "MLE"
  )

  if (is.null(fit)) return(NULL)

  params <- fit$estimate   # loc, scale, shape
  ll     <- -fit$deviance / 2   # evd stores deviance = -2 * loglik
  k      <- length(params)

  # convergence=1 from optim means "iteration limit reached" but solution can
  # still be valid - check parameters are finite instead of trusting the flag
  conv <- isTRUE(fit$convergence == 0) ||
          (all(is.finite(params)) && is.finite(ll))

  list(
    dist_key  = "gev",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = conv,
    fit_obj   = fit
  )
}


# -- Skew-Normal MLE (via sn package) -----------------------------------------

#' MLE for the Skew-Normal distribution using sn::selm
#' @keywords internal
.mle_sn <- function(x, entry) {

  if (!has_pkg("sn", "Skew-Normal distribution fitting")) return(NULL)

  n  <- length(x)
  df <- data.frame(y = x)

  fit <- safe_fit(
    sn::selm(y ~ 1, data = df, family = "SN"),
    dist_name = "Skew-Normal",
    method    = "MLE"
  )

  if (is.null(fit)) return(NULL)

  # Extract direct parameters (xi, omega, alpha) from the selm object.
  # unname() is critical: dp is a named vector, so c(xi = dp[1]) would
  # produce "xi.xi" (name concatenation). Strip names first.
  dp     <- unname(sn::extractSECdistr(fit)@dp)
  params <- c(xi = dp[1], omega = dp[2], alpha = dp[3])

  # Compute log-likelihood manually (selm stores it inside the object)
  ll <- as.numeric(sn::logLik(fit))
  k  <- 3L

  list(
    dist_key  = "skewnormal",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = TRUE,
    fit_obj   = fit
  )
}


# -- Triangular MLE (custom: fix min/max, search for mode) --------------------

#' MLE for the Triangular distribution
#'
#' The MLE for min and max are the sample extremes.  We profile over mode
#' using stats::optimize() on the interval (min, max), calling mc2d::dtriang
#' directly (bypassing fitdistrplus and search-path lookup issues).
#'
#' @keywords internal
.mle_triangular <- function(x) {

  if (!has_pkg("mc2d", "Triangular distribution fitting")) return(NULL)

  n <- length(x)
  # Expand support slightly so no observation sits exactly on the boundary
  # (triangular density is 0 at a and b, causing -Inf log-densities there)
  rng <- max(x) - min(x)
  a <- min(x) - 1e-4 * rng
  b <- max(x) + 1e-4 * rng
  if (b <= a) return(NULL)

  # Profile log-likelihood: for given mode c, loglik = sum(log dtriang(x; a,b,c))
  neg_ll <- function(c_val) {
    if (c_val <= a || c_val >= b) return(1e300)
    ld <- tryCatch(
      mc2d::dtriang(x, min = a, max = b, mode = c_val, log = TRUE),
      error = function(e) rep(-1e300, length(x))
    )
    if (any(!is.finite(ld))) return(1e300)
    -sum(ld)
  }

  opt <- safe_fit(
    stats::optimize(neg_ll, interval = c(a + 1e-6 * (b - a),
                                         b - 1e-6 * (b - a))),
    dist_name = "Triangular",
    method    = "MLE"
  )
  if (is.null(opt)) return(NULL)

  params <- c(min = a, max = b, mode = opt$minimum)
  ll     <- -opt$objective
  # k=1: only mode is a free optim parameter; min/max are fixed at sample
  # extremes (the MLE for boundaries in a triangular model is min(x)/max(x),
  # derived analytically, not by optimisation).  Using k=3 would impose an
  # unfair AIC penalty relative to 2-parameter distributions.
  k      <- 1L

  list(
    dist_key  = "triangular",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- PERT MLE (custom: fix min/max, search for mode) --------------------------

#' MLE for the PERT distribution
#'
#' The practical MLE profiles over mode using stats::optimize(), with min and
#' max expanded beyond the sample extremes (shape parameter fixed at the
#' standard PERT value of 4).
#'
#' WHY A LARGER BUFFER THAN TRIANGULAR:
#' The PERT density approaches 0 at its boundaries as (x - min)^(alpha1 - 1)
#' where alpha1 > 1 (e.g. alpha1 ~= 2.6 for mode near the midpoint).  This
#' power-law decay is faster than the linear decay of the Triangular density.
#' If min/max are set too close to the sample extremes, every observation near
#' the true boundary gets an artificially tiny log-density, collapsing the
#' total log-likelihood.  A buffer of rng / sqrt(n) scales the expansion with
#' sample size: large n needs less padding because sample extremes are already
#' close to the true bounds, while small n benefits from wider coverage.
#'
#' @keywords internal
.mle_pert <- function(x) {

  if (!has_pkg("mc2d", "PERT distribution fitting")) return(NULL)

  n <- length(x)
  rng <- max(x) - min(x)
  buf <- rng / sqrt(n)     # adaptive buffer: larger than 1e-4*rng for small n
  a <- min(x) - buf
  b <- max(x) + buf
  if (b <= a) return(NULL)

  neg_ll <- function(m_val) {
    if (m_val <= a || m_val >= b) return(1e300)
    ld <- tryCatch(
      mc2d::dpert(x, min = a, mode = m_val, max = b, log = TRUE),
      error = function(e) rep(-1e300, length(x))
    )
    if (any(!is.finite(ld))) return(1e300)
    -sum(ld)
  }

  opt <- safe_fit(
    stats::optimize(neg_ll, interval = c(a + 1e-6 * (b - a),
                                         b - 1e-6 * (b - a))),
    dist_name = "PERT",
    method    = "MLE"
  )
  if (is.null(opt)) return(NULL)

  params <- c(min = a, mode = opt$minimum, max = b)
  ll     <- -opt$objective
  k      <- 1L   # same reasoning as triangular: only mode is free

  list(
    dist_key  = "pert",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- GPD MLE (dedicated fitter - fixes loc, constrains shape) -----------------
#
#  Root cause of AIC-gaming: the generic optim allowed shape -> -Inf, squeezing
#  GPD's bounded support to bracket the data range -> artificially high logLik.
#
#  Fix:
#    * loc fixed analytically at min(x) - [U+03B5]  (standard POT threshold)
#    * shape constrained to (-0.5, 2.0)  (standard hydrological range;
#      ensures finite variance and prevents degenerate bounded-support gaming)
#    * k = 2  (only scale and shape are free parameters)
#
#' @keywords internal
.mle_gpd <- function(x, entry) {

  n   <- length(x)
  rng <- max(x) - min(x)
  loc <- min(x) - 1e-4 * max(rng, 1)   # threshold just below all observations

  y   <- x - loc   # exceedances (all positive)

  # Starting values from MOM of exceedances
  m   <- mean(y);  v <- stats::var(y)
  xi0 <- if (v > 0) max(-0.4, 0.5 * (1 - m^2 / v)) else 0
  sc0 <- max(m * (1 - xi0), 1e-3)

  neg_ll <- function(par) {
    sc <- par[1];  xi <- par[2]
    if (sc <= 0) return(1e300)
    ld <- .dgpd_internal(x, loc = loc, scale = sc, shape = xi, log = TRUE)
    if (any(!is.finite(ld))) return(1e300)
    -sum(ld)
  }

  opt <- safe_fit(
    stats::optim(
      par    = c(sc0, xi0),
      fn     = neg_ll,
      method = "L-BFGS-B",
      lower  = c(1e-9, -0.5),
      upper  = c(1e6,   2.0)
    ),
    dist_name = "Generalised Pareto",
    method    = "MLE"
  )
  if (is.null(opt)) return(NULL)

  sc_hat <- opt$par[1];  xi_hat <- opt$par[2]
  params <- c(loc = loc, scale = sc_hat, shape = xi_hat)
  ll     <- -opt$value
  # Sentinel guard: optimizer stuck at 1e300 (data incompatible with GPD support)
  if (!is.finite(ll) || ll < -1e290) return(NULL)
  k      <- 2L   # loc is analytically fixed, not a free optim parameter

  list(
    dist_key  = "gpd",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = isTRUE(opt$convergence == 0),
    fit_obj   = opt
  )
}


# -- Pearson Type III dedicated MLE -------------------------------------------

#' MLE for the Pearson Type III distribution
#'
#' Pearson III is a shifted/scaled Gamma: X = loc + scale * Gamma(shape).
#' When scale > 0 (right-skewed), the support is x > loc.  The generic
#' \code{.mle_custom_optim} path can produce starting values where loc >= min(x),
#' giving log-likelihood = -Inf at the first step; L-BFGS-B cannot move.
#'
#' Fix: use L-moments for starting values (robust to near-zero skewness) and
#' clamp loc strictly below min(x) before entering the optimizer.
#'
#' @keywords internal
.mle_pearson3 <- function(x, entry) {

  n <- length(x)

  # -- Starting values via L-moments (robust to near-zero skewness) -----------
  start_list <- tryCatch({
    lm  <- lmomco::lmoms(x, nmom = 4)
    pe3 <- lmomco::parpe3(lm)
    mu  <- unname(pe3$para["mu"])
    sig <- unname(pe3$para["sigma"])
    gam <- unname(pe3$para["gamma"])
    if (!is.finite(gam) || abs(gam) < 0.01) gam <- 0.1
    list(shape = 4 / gam^2, scale = sig * gam / 2, loc = mu - 2 * sig / gam)
  }, error = function(e) {
    mu  <- mean(x); sig <- stats::sd(x)
    gam <- tryCatch(sum(((x - mu) / sig)^3) / n, error = function(e) 0.1)
    if (!is.finite(gam) || abs(gam) < 0.01) gam <- 0.1
    list(shape = 4 / gam^2, scale = sig * gam / 2, loc = mu - 2 * sig / gam)
  })

  # -- Negative log-likelihood -------------------------------------------------
  neg_ll <- function(par) {
    shape <- par[1]; scale <- par[2]; loc <- par[3]
    if (shape <= 0) return(1e300)
    ld <- .dpearson3_internal(x, shape = shape, scale = scale, loc = loc, log = TRUE)
    if (any(!is.finite(ld))) return(1e300)
    s <- -sum(ld)
    if (!is.finite(s)) 1e300 else s
  }

  sv <- c(shape = unname(start_list$shape),
          scale = unname(start_list$scale),
          loc   = unname(start_list$loc))

  # Clamp loc: for scale > 0 (right-skewed), support is x > loc, so loc < min(x).
  # For scale < 0 (left-skewed), support is x < loc, handled symmetrically below.
  eps <- 0.01 * diff(range(x))
  if (is.finite(sv["scale"]) && sv["scale"] > 0) {
    if (!is.finite(sv["loc"]) || sv["loc"] >= min(x))
      sv["loc"] <- min(x) - eps
  } else if (is.finite(sv["scale"]) && sv["scale"] < 0) {
    if (!is.finite(sv["loc"]) || sv["loc"] <= max(x))
      sv["loc"] <- max(x) + eps
  }

  # If starting LL is still degenerate, apply a safe fallback
  if (neg_ll(sv) >= 1e299) {
    sv <- c(shape = 4,
            scale = stats::sd(x) / 2,
            loc   = min(x) - eps)
  }

  # -- Optimise ----------------------------------------------------------------
  opt <- safe_fit(
    stats::optim(
      par    = sv,
      fn     = neg_ll,
      method = "L-BFGS-B",
      lower  = c(1e-9, -1e6, -1e6),
      upper  = c(1e6,   1e6,  1e6)
    ),
    dist_name = "Pearson Type III",
    method    = "MLE"
  )
  if (is.null(opt)) return(NULL)

  params        <- opt$par
  names(params) <- c("shape", "scale", "loc")
  ll <- -opt$value
  if (!is.finite(ll) || ll < -1e290) return(NULL)
  k  <- 3L

  list(
    dist_key  = "pearson3",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = (opt$convergence == 0),
    fit_obj   = opt
  )
}


# -- Log-Pearson III dedicated MLE --------------------------------------------

#' MLE for the Log-Pearson III distribution
#'
#' Strategy: fit Pearson III directly on \code{log(x)} and adjust the
#' log-likelihood for the Jacobian: \code{loglik_LP3 = loglik_P3(log x) - [U+03A3]log(x)}.
#'
#' Root cause of past failures: the MOM skewness estimate of log(x) can be
#' near-zero for some random seeds, producing shape = 4/gam^2 -> Inf.  The Gamma
#' density at data points is then effectively 0 everywhere, \code{optim} gets a
#' flat 1e300 objective from the first step, cannot compute a gradient, and
#' errors out.  L-moments (via \code{lmomco::parpe3}) are robust to this.
#'
#' @keywords internal
.mle_lpearson3 <- function(x, entry) {

  if (any(x <= 0)) {
    message("  [FitVerse] LP3 requires x > 0 -- skipping MLE.")
    return(NULL)
  }

  n  <- length(x)
  lx <- log(x)

  # -- Starting values: try L-moments first, fall back to MOM, then constants --
  start_list <- tryCatch({
    lm  <- lmomco::lmoms(lx, nmom = 4)
    pe3 <- lmomco::parpe3(lm)
    mu  <- pe3$para["mu"]
    sig <- pe3$para["sigma"]
    gam <- pe3$para["gamma"]
    if (!is.finite(gam) || abs(gam) < 0.01) gam <- 0.1
    list(shape = 4 / gam^2,
         scale = sig * gam / 2,
         loc   = mu - 2 * sig / gam)
  }, error = function(e) {
    # MOM fallback
    mu  <- mean(lx); sig <- stats::sd(lx)
    gam <- tryCatch(sum(((lx - mu) / sig)^3) / n, error = function(e) 0.1)
    if (!is.finite(gam) || abs(gam) < 0.01) gam <- 0.1
    list(shape = 4 / gam^2,
         scale = sig * gam / 2,
         loc   = mu - 2 * sig / gam)
  })

  # -- Negative log-likelihood (Pearson III on log-space) ---------------------
  neg_ll_p3 <- function(par) {
    shape <- par[1]; scale <- par[2]; loc <- par[3]
    if (shape <= 0) return(1e300)
    ld <- .dpearson3_internal(lx, shape = shape, scale = scale, loc = loc, log = TRUE)
    if (any(!is.finite(ld))) return(1e300)
    s <- -sum(ld)
    if (!is.finite(s)) 1e300 else s
  }

  sv <- c(shape = unname(start_list$shape),
          scale = unname(start_list$scale),
          loc   = unname(start_list$loc))

  # Safety: if starting params give 1e300 penalty, try a safe fallback.
  # loc MUST be below min(lx) so all data lie within the P3 support.
  if (neg_ll_p3(sv) >= 1e299) {
    # Ensure loc is safely below the minimum observed log-value
    loc_safe <- min(min(lx) - 0.01 * abs(diff(range(lx))),
                    mean(lx) - 2 * stats::sd(lx))
    sv <- c(shape = 4,
            scale = stats::sd(lx) / 2,
            loc   = loc_safe)
  }
  # Final guard: if loc is still at or above min(lx), clamp it down
  if (!is.finite(sv["loc"]) || sv["loc"] >= min(lx)) {
    sv["loc"] <- min(lx) - 0.01 * abs(diff(range(lx)))
  }

  # -- Optimise -----------------------------------------------------------------
  opt <- safe_fit(
    stats::optim(
      par    = sv,
      fn     = neg_ll_p3,
      method = "L-BFGS-B",
      lower  = c(1e-9, -1e6, -1e6),
      upper  = c(1e6,   1e6,  1e6)
    ),
    dist_name = "Log-Pearson III",
    method    = "MLE"
  )
  if (is.null(opt)) return(NULL)

  params        <- opt$par
  names(params) <- c("shape", "scale", "loc")

  # LP3 loglik = P3 loglik on log(x)  minus  [U+03A3]log(x)  [change-of-variables Jacobian]
  ll_lp3 <- -opt$value - sum(lx)
  if (!is.finite(ll_lp3) || ll_lp3 < -1e290) return(NULL)
  k       <- 3L

  list(
    dist_key  = "lpearson3",
    method    = "MLE",
    params    = params,
    loglik    = ll_lp3,
    aic       = compute_aic(ll_lp3, k),
    bic       = compute_bic(ll_lp3, k, n),
    n_params  = k,
    n         = n,
    converged = (opt$convergence == 0),
    fit_obj   = opt
  )
}


# -- Generalised Logistic (GLO) dedicated MLE ---------------------------------

#' MLE for the Generalised Logistic distribution
#'
#' The generic `.mle_custom_optim` route starts with shape = 0 (logistic).
#' Because the `.dglo_internal` density switches to the logistic formula when
#' |shape| < 1e-8, the numerical gradient in the shape direction is effectively
#' zero at the starting point, and L-BFGS-B never moves shape away from 0.
#'
#' Fix: use L-moments (via \code{lmomco::parglo}) as starting values, which
#' give an accurate initial shape estimate.  Fall back to a small positive
#' shape (0.1) if L-moments fail.
#'
#' @keywords internal
.mle_glo <- function(x, entry) {

  n <- length(x)

  # L-moment starting values - GLO is well-identified by L-moments.
  start_vec <- tryCatch({
    lm  <- lmomco::lmoms(x, nmom = 4)
    gl  <- lmomco::parglo(lm)
    c(loc   = unname(gl$para["xi"]),
      scale  = unname(gl$para["alpha"]),
      shape  = unname(gl$para["kappa"]))
  }, error = function(e) {
    # Fallback: logistic scale + small nonzero shape to break the symmetry trap.
    c(loc   = stats::median(x),
      scale  = stats::IQR(x) / (2 * log(3)),
      shape  = 0.1)
  })

  neg_ll <- function(par) {
    loc <- par[1]; scale <- par[2]; shape <- par[3]
    if (scale <= 0) return(1e300)
    ld <- .dglo_internal(x, loc = loc, scale = scale, shape = shape, log = TRUE)
    if (any(!is.finite(ld))) return(1e300)
    -sum(ld)
  }

  opt <- safe_fit(
    stats::optim(
      par    = start_vec,
      fn     = neg_ll,
      method = "L-BFGS-B",
      lower  = c(-1e6, 1e-9, -1e6),
      upper  = c( 1e6, 1e6,   1e6)
    ),
    dist_name = "Generalised Logistic",
    method    = "MLE"
  )
  if (is.null(opt)) return(NULL)

  params        <- opt$par
  names(params) <- c("loc", "scale", "shape")
  ll <- -opt$value
  if (!is.finite(ll) || ll < -1e290) return(NULL)
  k  <- 3L

  list(
    dist_key  = "glo",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = (opt$convergence == 0),
    fit_obj   = opt
  )
}


# -- Custom optim-based MLE (Pearson III, LP3, GLO) ---------------------------

#' Generic MLE via stats::optim using the internal d/p/q/r override table
#'
#' Used for distributions whose d-functions are in .FITVERSE_DPQR_OVERRIDES
#' and cannot be passed to fitdistrplus directly.
#'
#' @keywords internal
.mle_custom_optim <- function(x, dist_key, entry) {

  n     <- length(x)
  start <- entry$start_fun(x)

  # Build negative log-likelihood function using the internal density
  d_fun <- .lookup_dpqr("d", dist_key)
  if (is.null(d_fun)) {
    message(sprintf("  [FitVerse] No density function found for '%s' -- skipping MLE.",
                    entry$display_name))
    return(NULL)
  }

  neg_ll <- function(par_vec) {
    params <- as.list(par_vec)
    names(params) <- names(start)
    # Additional positivity constraints
    if (!is.null(entry$lower)) {
      lo <- entry$lower[names(start)]
      if (any(par_vec <= lo, na.rm = TRUE)) return(1e300)
    }
    ll <- tryCatch({
      ld <- do.call(d_fun, c(list(x = x), params, list(log = TRUE)))
      # If ANY data point is outside support (log-density non-finite),
      # the parameter set is invalid - penalise the whole proposal.
      if (any(!is.finite(ld))) return(1e300)
      -sum(ld)
    }, error = function(e) 1e300)
    if (is.nan(ll) || is.infinite(ll)) return(1e300)
    ll
  }

  start_vec <- unlist(start)

  # Build lower/upper vectors for L-BFGS-B
  lo <- entry$lower[names(start)]
  up <- entry$upper[names(start)]
  # Replace infinite bounds with large finite numbers for L-BFGS-B
  lo_bfgs <- ifelse(is.finite(lo),  lo,    -1e6)
  up_bfgs <- ifelse(is.finite(up),  up,     1e6)
  # Ensure scale stays positive ONLY when the registry lower bound is >= 0.
  # (Pearson III / LP3 have lower["scale"] = -Inf, so scale CAN be negative.)
  if ("scale" %in% names(lo_bfgs) &&
      !is.na(lo["scale"]) && is.finite(lo["scale"]) && lo["scale"] >= 0) {
    lo_bfgs["scale"] <- max(lo_bfgs["scale"], 1e-9)
  }

  opt <- safe_fit(
    stats::optim(
      par    = start_vec,
      fn     = neg_ll,
      method = "L-BFGS-B",
      lower  = lo_bfgs,
      upper  = up_bfgs
    ),
    dist_name = entry$display_name,
    method    = "MLE"
  )

  if (is.null(opt)) return(NULL)

  params <- opt$par
  names(params) <- names(start)
  ll   <- -opt$value
  k    <- length(params)

  # If optimizer converged at the sentinel value (1e300), the fit genuinely
  # failed -- the distribution is incompatible with this data.  Return NULL
  # so the ranking table never shows AIC = 2e300.
  if (!is.finite(ll) || ll < -1e290) return(NULL)

  list(
    dist_key  = dist_key,
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = (opt$convergence == 0),
    fit_obj   = opt
  )
}


# -- Inverse Weibull MLE (profile likelihood) ----------------------------------

#' MLE for the Inverse Weibull distribution via profile likelihood
#' @noRd
.mle_invweibull <- function(x, entry) {

  n <- length(x)
  if (any(x <= 0)) return(NULL)

  slx <- sum(log(x))    # constant: sum(log(x_i))

  # Profile negative log-likelihood over tau (shape)
  # Analytic profile: theta_hat = (n / sum(x^{-tau}))^{1/tau}
  # l(tau) = n*log(tau) - n*log(sum(x^{-tau})) - (tau+1)*slx  (+ constants)
  neg_profile_ll <- function(tau) {
    if (!is.finite(tau) || tau <= 0) return(1e300)
    log_xm  <- -tau * log(x)               # log(x_i^{-tau})
    max_lxm <- max(log_xm)
    # Log-sum-exp for numerical stability
    log_sw  <- max_lxm + log(sum(exp(log_xm - max_lxm)))
    if (!is.finite(log_sw)) return(1e300)
    s <- -(n * log(tau) - n * log_sw - (tau + 1L) * slx)
    if (!is.finite(s)) 1e300 else s
  }

  # Coarse grid to bracket the optimum
  tau_grid  <- c(0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 8, 10, 15, 20)
  nll_grid  <- vapply(tau_grid, neg_profile_ll, numeric(1))
  best_idx  <- which.min(nll_grid)
  if (!is.finite(nll_grid[best_idx])) return(NULL)
  tau0 <- tau_grid[best_idx]

  # 1-D golden-section search in a window centred on best grid point
  lo <- max(0.01,  tau0 / 4)
  hi <- min(200,   tau0 * 4)

  opt <- tryCatch(
    stats::optimise(neg_profile_ll, interval = c(lo, hi)),
    error = function(e) NULL
  )
  if (is.null(opt) || !is.finite(opt$objective) || opt$objective >= 1e290)
    return(NULL)

  tau_hat <- opt$minimum
  # Recover scale from analytic formula
  log_sw_hat <- tau_hat * max(-log(x)) +
                log(sum(exp(-tau_hat * log(x) - tau_hat * max(-log(x)))))
  theta_hat  <- exp((log(n) - log_sw_hat) / tau_hat)

  if (!is.finite(tau_hat) || !is.finite(theta_hat) ||
      tau_hat <= 0 || theta_hat <= 0) return(NULL)

  # Full log-likelihood at the MLE
  d_fun <- .lookup_dpqr("d", "invweibull")
  if (is.null(d_fun)) return(NULL)
  ll <- tryCatch(
    sum(d_fun(x, shape = tau_hat, scale = theta_hat, log = TRUE)),
    error = function(e) NA_real_
  )
  if (!is.finite(ll) || ll < -1e290) return(NULL)

  params <- c(shape = tau_hat, scale = theta_hat)
  k <- 2L
  list(
    dist_key  = "invweibull",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = TRUE,
    fit_obj   = opt
  )
}


# -- Burr XII MLE -------------------------------------------------------------

#' MLE for the Burr XII distribution with multi-start optimisation
#'
#' The generic \code{.mle_custom_optim} uses a single starting point that can
#' fail to converge on Burr XII's three-parameter likelihood surface.  This
#' dedicated fitter tries a 3x3 grid of (shape1, shape2) starting values with
#' scale fixed at the sample median, returning the best result.
#'
#' @keywords internal
.mle_burr <- function(x, entry) {

  n     <- length(x)
  d_fun <- .lookup_dpqr("d", "burr")
  if (is.null(d_fun)) return(NULL)

  neg_ll <- function(par_vec) {
    sh1 <- par_vec[1]; sh2 <- par_vec[2]; sc <- par_vec[3]
    if (sh1 <= 0 || sh2 <= 0 || sc <= 0) return(1e300)
    ld <- tryCatch(
      d_fun(x, shape1 = sh1, shape2 = sh2, scale = sc, log = TRUE),
      error = function(e) rep(-Inf, n)
    )
    if (any(!is.finite(ld))) return(1e300)
    s <- -sum(ld)
    if (!is.finite(s)) 1e300 else s
  }

  sc0      <- stats::median(x)
  sh1_grid <- c(1, 2, 3)
  sh2_grid <- c(1, 1.5, 2)

  best_nll <- Inf
  best_opt <- NULL

  for (sh1_0 in sh1_grid) {
    for (sh2_0 in sh2_grid) {
      sv   <- c(shape1 = sh1_0, shape2 = sh2_0, scale = sc0)
      nll0 <- neg_ll(sv)
      if (!is.finite(nll0) || nll0 >= 1e290) next
      opt <- tryCatch(
        stats::optim(
          par    = sv,
          fn     = neg_ll,
          method = "L-BFGS-B",
          lower  = c(1e-9, 1e-9, 1e-9),
          upper  = c(1e6,  1e6,  1e6)
        ),
        error = function(e) NULL
      )
      if (is.null(opt) || opt$value >= best_nll) next
      best_nll <- opt$value
      best_opt <- opt
    }
  }

  if (is.null(best_opt)) return(NULL)
  ll <- -best_opt$value
  if (!is.finite(ll) || ll < -1e290) return(NULL)

  params <- best_opt$par
  names(params) <- c("shape1", "shape2", "scale")
  k <- 3L

  list(
    dist_key  = "burr",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, k),
    bic       = compute_bic(ll, k, n),
    n_params  = k,
    n         = n,
    converged = (best_opt$convergence == 0),
    fit_obj   = best_opt
  )
}


# -- Student-t (Location-Scale) MLE -------------------------------------------

#' MLE for the Student-t (Location-Scale) distribution with multi-start
#'
#' The generic \code{.mle_custom_optim} uses a fixed starting value of nu=10.
#' For data with heavy tails (true nu = 3--5), the optimizer can converge to
#' a poor local solution when started near the Normal.  This dedicated fitter
#' tries five candidate starting values for nu (derived from the sample excess
#' kurtosis plus a fixed grid) and returns the result with the highest
#' log-likelihood.
#'
#' @keywords internal
.mle_student_t <- function(x, entry) {

  n   <- length(x)
  mu0 <- mean(x)
  s0  <- stats::sd(x)

  d_fun <- .lookup_dpqr("d", "student_t")
  if (is.null(d_fun)) return(NULL)

  # Kurtosis-based nu estimate: excess kurtosis = 6 / (nu - 4) for nu > 4
  k4        <- tryCatch(mean((x - mu0)^4) / var(x)^2 - 3, error = function(e) NA_real_)
  nu_kurt   <- if (is.finite(k4) && k4 > 0.1) min(max(6 / k4 + 4, 2.5), 100) else NA_real_

  # Grid of starting values; prepend the kurtosis estimate if valid
  nu_grid   <- unique(c(if (is.finite(nu_kurt)) nu_kurt else NULL,
                        2.5, 3, 5, 10, 30))

  neg_ll <- function(par_vec) {
    params <- list(nu = par_vec[1], mu = par_vec[2], sigma = par_vec[3])
    ll <- tryCatch({
      ld <- do.call(d_fun, c(list(x = x), params, list(log = TRUE)))
      if (any(!is.finite(ld))) return(1e300)
      -sum(ld)
    }, error = function(e) 1e300)
    if (is.nan(ll) || is.infinite(ll)) return(1e300)
    ll
  }

  best_ll  <- -Inf
  best_opt <- NULL

  for (nu0 in nu_grid) {
    sigma0    <- s0 * sqrt(max(nu0 - 2, 0.01) / nu0)
    start_vec <- c(nu = nu0, mu = mu0, sigma = max(sigma0, 1e-9))

    opt <- safe_fit(
      stats::optim(
        par    = start_vec,
        fn     = neg_ll,
        method = "L-BFGS-B",
        lower  = c(nu = 1e-9, mu = -1e6, sigma = 1e-9),
        upper  = c(nu = 1e6,  mu =  1e6, sigma = 1e6)
      ),
      dist_name = entry$display_name,
      method    = "MLE"
    )

    if (is.null(opt)) next
    ll_cand <- -opt$value
    if (is.finite(ll_cand) && ll_cand > best_ll) {
      best_ll  <- ll_cand
      best_opt <- opt
    }
  }

  if (is.null(best_opt) || !is.finite(best_ll) || best_ll < -1e290) return(NULL)

  params <- best_opt$par
  names(params) <- c("nu", "mu", "sigma")
  k <- 3L

  list(
    dist_key  = "student_t",
    method    = "MLE",
    params    = params,
    loglik    = best_ll,
    aic       = compute_aic(best_ll, k),
    bic       = compute_bic(best_ll, k, n),
    n_params  = k,
    n         = n,
    converged = (best_opt$convergence == 0),
    fit_obj   = best_opt
  )
}


# -- 3-Parameter Weibull MLE ---------------------------------------------------

#' MLE for the 3-Parameter Weibull distribution
#'
#' Uses \code{stats::optim} with L-BFGS-B directly so that the critical
#' constraint \eqn{\mu < \min(x)} can be enforced via a dynamic upper bound
#' on the threshold parameter - something fitdistrplus cannot do because its
#' bounds are static.
#'
#' The log-likelihood is evaluated through \code{dweibull3p} (defined in
#' utils.R) so the same density is used consistently across MLE, GoF, and
#' plotting.
#'
#' @keywords internal
.mle_weibull3p <- function(x, entry) {

  n      <- length(x)
  xmin   <- min(x)
  start  <- entry$start_fun(x)
  thres0 <- start$thres

  # Negative log-likelihood
  neg_ll <- function(par) {
    shape_v <- par[1]; scale_v <- par[2]; thres_v <- par[3]
    if (shape_v <= 0 || scale_v <= 0 || thres_v >= xmin) return(1e300)
    ld <- dweibull3p(x, shape = shape_v, scale = scale_v,
                     thres = thres_v, log = TRUE)
    val <- -sum(ld[is.finite(ld)])
    if (!is.finite(val)) 1e300 else val
  }

  # L-BFGS-B: upper bound on thres is just below the sample minimum
  eps_thres <- 1e-6 * (diff(range(x)) + 1)

  opt <- safe_fit(
    stats::optim(
      par    = c(shape = start$shape, scale = start$scale, thres = thres0),
      fn     = neg_ll,
      method = "L-BFGS-B",
      lower  = c(shape = 1e-9, scale = 1e-9, thres = -1e9),
      upper  = c(shape = 1e6,  scale = 1e9,  thres = xmin - eps_thres)
    ),
    dist_name = "Weibull (3-Parameter)",
    method    = "MLE"
  )

  if (is.null(opt) || opt$convergence > 1) return(NULL)

  params        <- opt$par
  names(params) <- c("shape", "scale", "thres")
  ll            <- -opt$value

  list(
    dist_key  = "weibull3p",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 3L),
    bic       = compute_bic(ll, 3L, n),
    n_params  = 3L,
    n         = n,
    converged = (opt$convergence == 0),
    fit_obj   = opt
  )
}


# -- Right Triangular MLE (closed-form: min=min(x), max=max(x)) --------------

#' MLE for the Right Triangular distribution
#'
#' The right triangular PDF is f(x) = 2(b-x)/(b-a)^2 on [a,b].
#' The MLE of a is min(x) and b is max(x) (boundary estimators).
#' Only 1 effective free parameter (k=1) for AIC purposes, same logic as
#' Triangular and PERT.
#'
#' @keywords internal
.mle_rtriangle <- function(x) {

  n    <- length(x)
  rng  <- max(x) - min(x)
  if (rng <= 0) return(NULL)

  a    <- min(x)           # MLE for lower bound is exactly min(x)
  xmax <- max(x)

  # MLE for upper bound b:
  #   log L(b) = n*log(2) - 2n*log(b-a) + sum(log(b-x_i)),  b > xmax
  # Solved via 1-D optimise (objective is smooth and unimodal on (xmax, xmax+100*rng)).
  neg_ll_b <- function(b) {
    if (!is.finite(b) || b <= xmax) return(1e300)
    -(n * log(2) - 2 * n * log(b - a) + sum(log(b - x)))
  }

  opt <- tryCatch(
    stats::optimise(neg_ll_b,
                    lower = xmax + 1e-8 * rng,
                    upper = xmax + 100  * rng),
    error = function(e) NULL)

  if (is.null(opt) || !is.finite(opt$objective) || opt$objective >= 1e290)
    return(NULL)

  b  <- opt$minimum
  ll <- tryCatch(sum(drtriangle(x, min = a, max = b, log = TRUE)),
                 error = function(e) NA_real_)
  if (!is.finite(ll)) return(NULL)

  params <- c(min = a, max = b)
  k      <- 1L   # only b optimised; a is fixed to min(x)

  list(dist_key = "rtriangle", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = opt)
}


# -- Inverse Gaussian MLE (closed-form) ----------------------------------------

#' MLE for the Inverse Gaussian (Wald) distribution
#'
#' Closed-form MLEs:
#'   mu_hat     = mean(x)
#'   lambda_hat = n / sum(1/x - 1/mean(x))
#'
#' @keywords internal
.mle_invgaussian <- function(x) {

  if (any(x <= 0)) {
    message("  [FitVerse] Inverse Gaussian requires x > 0 -- skipping MLE.")
    return(NULL)
  }

  n      <- length(x)
  mu_hat <- mean(x)
  lam_hat <- n / sum(1 / x - 1 / mu_hat)

  if (!is.finite(mu_hat) || !is.finite(lam_hat) ||
      mu_hat <= 0 || lam_hat <= 0) return(NULL)

  params <- c(mu = mu_hat, lambda = lam_hat)
  ll     <- sum(dinvgaussian(x, mu = mu_hat, lambda = lam_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k      <- 2L

  list(dist_key = "invgaussian", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Laplace MLE (closed-form) -------------------------------------------------

#' MLE for the Laplace (double-exponential) distribution
#'
#' Closed-form MLEs:
#'   mu_hat = median(x)
#'   b_hat  = mean(|x - mu_hat|)
#'
#' @keywords internal
.mle_laplace <- function(x) {

  n      <- length(x)
  mu_hat <- stats::median(x)
  b_hat  <- mean(abs(x - mu_hat))

  if (!is.finite(b_hat) || b_hat <= 0) return(NULL)

  params <- c(mu = mu_hat, b = b_hat)
  ll     <- sum(dlaplace(x, mu = mu_hat, b = b_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k      <- 2L

  list(dist_key = "laplace", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Birnbaum-Saunders MLE (iterative closed-form) ----------------------------

#' MLE for the Birnbaum-Saunders (fatigue-life) distribution
#'
#' Correct closed-form MLEs (Birnbaum & Saunders 1969; Ng et al. 2003):
#'   nu_tilde  = mean(sqrt(x))
#'   mu_tilde  = mean(1 / sqrt(x))
#'   beta_hat  = nu_tilde / mu_tilde
#'   alpha_hat = sqrt(mean(x)/beta_hat + beta_hat*mean(1/x) - 2)
#'
#' @keywords internal
.mle_birnbaum_saunders <- function(x) {

  if (any(x <= 0)) {
    message("  [FitVerse] Birnbaum-Saunders requires x > 0 -- skipping MLE.")
    return(NULL)
  }

  n <- length(x)

  # Correct closed-form MLEs (Birnbaum & Saunders 1969; Ng et al. 2003):
  #
  #   beta[U+0302] = nu[U+0303] / mu[U+0303]       where nu[U+0303] = mean(sqrtx[U+1D62]), mu[U+0303] = mean(1/sqrtx[U+1D62])
  #   alpha[U+0302] = sqrt( mean(x[U+1D62])/beta[U+0302] + beta[U+0302].mean(1/x[U+1D62]) - 2 )
  #
  # Note: the naive formula sqrt(nu[U+0303].mu[U+0303] - 1) is WRONG for large alpha - it
  # converges to a biased limit and can produce alpha[U+0302] << true alpha.

  nu_tilde  <- mean(sqrt(x))
  mu_tilde  <- mean(1 / sqrt(x))
  beta_hat  <- nu_tilde / mu_tilde

  alpha2    <- mean(x) / beta_hat + beta_hat * mean(1 / x) - 2
  if (!is.finite(beta_hat) || beta_hat <= 0 ||
      !is.finite(alpha2)   || alpha2  <= 0) return(NULL)

  alpha_hat <- sqrt(alpha2)
  params <- c(alpha = alpha_hat, beta = beta_hat)
  ll     <- sum(dbirnbaum_saunders(x, alpha = alpha_hat, beta = beta_hat,
                                   log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L

  list(dist_key = "birnbaum_saunders", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Gompertz MLE (log-linear CDF regression starting values) -----------------

#' MLE for the Gompertz distribution
#'
#' Starting values via log-linear regression on the empirical CDF:
#'   log(-log(1-F(x))) = log(alpha/beta) + beta*x
#' so slope = beta, intercept = log(alpha/beta) => alpha = beta*exp(intercept).
#' This reliably recovers small beta values that trip up generic starting heuristics.
#'
#' @keywords internal
.mle_gompertz <- function(x) {

  if (any(x < 0)) {
    message("  [FitVerse] Gompertz requires x >= 0 -- skipping MLE.")
    return(NULL)
  }

  n <- length(x)

  # -- Starting values via log-linear regression ------------------------------
  x_ord <- sort(x)
  # Blom plotting positions (avoid 0 and 1)
  p     <- (seq_len(n) - 0.375) / (n + 0.25)
  p     <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  lhs   <- log(-log(1 - p))   # = log(alpha/beta) + beta * x

  # Use middle 80% to avoid leverage from tails
  idx   <- seq(max(1L, ceiling(0.1 * n)), min(n, floor(0.9 * n)))
  fit   <- tryCatch(
    stats::lm.fit(cbind(1, x_ord[idx]), lhs[idx]),
    error = function(e) NULL
  )

  if (!is.null(fit) && length(fit$coefficients) == 2 &&
      is.finite(fit$coefficients[2]) && fit$coefficients[2] > 0) {
    beta0  <- fit$coefficients[2]
    alpha0 <- beta0 * exp(fit$coefficients[1])
  } else {
    # Fallback: exponential-like starting values
    beta0  <- 1 / (stats::sd(x) + 1e-9)
    alpha0 <- 1 / (mean(x) + 1e-9)
  }
  alpha0 <- max(alpha0, 1e-6)
  beta0  <- max(beta0,  1e-6)

  # -- Negative log-likelihood ------------------------------------------------
  neg_ll <- function(par) {
    a <- par[1]; b <- par[2]
    if (a <= 0 || b <= 0) return(1e300)
    ld <- tryCatch(dgompertz(x, alpha = a, beta = b, log = TRUE),
                   error = function(e) rep(-1e300, n))
    if (any(!is.finite(ld))) return(1e300)
    s <- -sum(ld)
    if (!is.finite(s)) 1e300 else s
  }

  opt <- safe_fit(
    stats::optim(
      par    = c(alpha = alpha0, beta = beta0),
      fn     = neg_ll,
      method = "L-BFGS-B",
      lower  = c(1e-9, 1e-9),
      upper  = c(1e6,  1e6)
    ),
    dist_name = "Gompertz",
    method    = "MLE"
  )

  if (is.null(opt)) return(NULL)
  params        <- opt$par
  names(params) <- c("alpha", "beta")
  ll            <- -opt$value
  k             <- 2L

  list(dist_key = "gompertz", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = (opt$convergence == 0), fit_obj = opt)
}


# -- Generalised Gamma MLE (profile likelihood over p) ------------------------
#
#' Profile-likelihood MLE for the Generalised Gamma distribution
#'
#' Reduces the 3-parameter optimisation to a 1-D problem:
#'   for fixed p, let Y = X^p ~ Gamma(d/p, scale = b^p).
#'   The Gamma MLE for Y gives d and b analytically via Newton-Raphson.
#'   A grid search + 1-D refinement over p finds the global optimum reliably.
#'
#' @keywords internal
.mle_gengamma <- function(x) {

  if (any(x <= 0)) {
    message("  [FitVerse] Generalised Gamma requires x > 0 -- skipping MLE.")
    return(NULL)
  }

  n      <- length(x)
  log_x  <- log(x)

  # -- Inner: Gamma MLE on Y = x^p ------------------------------------------
  # Returns list(shape, scale, ll) or NULL
  gamma_mle_on_power <- function(p) {
    y     <- x^p
    logy  <- p * log_x
    my    <- mean(y)
    mly   <- mean(logy)
    s     <- log(my) - mly          # s = log(mean) - mean(log) > 0 always
    if (!is.finite(s) || s <= 0) return(NULL)

    # Choi & Wette (1969) initial approximation for Gamma shape
    shape <- (3 - s + sqrt((s - 3)^2 + 24 * s)) / (12 * s)

    # Newton-Raphson: solve log(k) - digamma(k) = s
    for (iter in seq_len(50L)) {
      g  <- log(shape) - digamma(shape) - s
      dg <- 1 / shape - trigamma(shape)
      if (abs(dg) < 1e-14 || !is.finite(g) || !is.finite(dg)) break
      shape <- shape - g / dg
      if (shape <= 0 || !is.finite(shape)) return(NULL)
      if (abs(g) < 1e-10) break
    }
    scale <- my / shape
    if (scale <= 0 || !is.finite(scale)) return(NULL)

    # Map back to (d, b)
    d_hat <- shape * p
    b_hat <- scale^(1 / p)
    if (d_hat <= 0 || b_hat <= 0) return(NULL)

    ll <- tryCatch(
      sum(dgengamma(x, p = p, d = d_hat, b = b_hat, log = TRUE)),
      error = function(e) -1e300
    )
    if (!is.finite(ll)) return(NULL)
    list(shape = shape, scale = scale, d = d_hat, b = b_hat, ll = ll)
  }

  # -- Grid search over p ----------------------------------------------------
  p_grid <- c(0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0)
  ll_grid <- vapply(p_grid, function(p) {
    r <- tryCatch(gamma_mle_on_power(p), error = function(e) NULL)
    if (is.null(r)) -1e300 else r$ll
  }, numeric(1L))

  if (all(ll_grid <= -1e299)) return(NULL)
  p_best <- p_grid[which.max(ll_grid)]

  # -- 1-D refinement over p -------------------------------------------------
  profile_fn <- function(p) {
    r <- tryCatch(gamma_mle_on_power(p), error = function(e) NULL)
    if (is.null(r)) -1e300 else r$ll
  }

  lo  <- max(p_best / 3, 0.05)
  hi  <- p_best * 4
  opt1 <- tryCatch(
    stats::optimize(profile_fn, interval = c(lo, hi), maximum = TRUE,
                    tol = 1e-6),
    error = function(e) NULL
  )
  if (!is.null(opt1)) p_best <- opt1$maximum

  # -- Final parameter set ---------------------------------------------------
  r_final <- gamma_mle_on_power(p_best)
  if (is.null(r_final)) return(NULL)

  params        <- c(p = p_best, d = r_final$d, b = r_final$b)
  ll            <- r_final$ll
  k             <- 3L

  list(dist_key = "gengamma", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Rayleigh MLE (closed-form) ------------------------------------------------
#' sigma_hat = sqrt(mean(x^2) / 2)
#' @keywords internal
.mle_rayleigh <- function(x) {
  if (any(x < 0)) return(NULL)
  n         <- length(x)
  sigma_hat <- sqrt(mean(x^2) / 2)
  if (!is.finite(sigma_hat) || sigma_hat <= 0) return(NULL)
  params <- c(sigma = sigma_hat)
  ll     <- sum(drayleigh(x, sigma = sigma_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 1L
  list(dist_key = "rayleigh", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Half-Normal MLE (closed-form) ---------------------------------------------
#' sigma_hat = sqrt(mean(x^2))
#' @keywords internal
.mle_halfnormal <- function(x) {
  if (any(x < 0)) return(NULL)
  n         <- length(x)
  sigma_hat <- sqrt(mean(x^2))
  if (!is.finite(sigma_hat) || sigma_hat <= 0) return(NULL)
  params <- c(sigma = sigma_hat)
  ll     <- sum(dhalfnormal(x, sigma = sigma_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 1L
  list(dist_key = "halfnormal", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Exponentiated Weibull MLE (L-BFGS-B, Weibull starting values) -------------
#' @keywords internal
.mle_expweibull <- function(x, entry) {
  if (any(x <= 0)) return(NULL)
  n <- length(x)

  # Start at Weibull 2P (shape2 = 1 reduces to Weibull)
  w_fit <- tryCatch(
    fitdistrplus::fitdist(x, "weibull"),
    error = function(e) NULL
  )
  if (!is.null(w_fit)) {
    sh1_0 <- unname(w_fit$estimate["shape"])
    sc_0  <- unname(w_fit$estimate["scale"])
  } else {
    sh1_0 <- 1.5
    sc_0  <- mean(x)
  }

  neg_ll <- function(par) {
    s1 <- par[1]; sc <- par[2]; s2 <- par[3]
    if (s1 <= 0 || sc <= 0 || s2 <= 0) return(1e300)
    ld <- tryCatch(dexpweibull(x, shape1 = s1, scale = sc, shape2 = s2,
                               log = TRUE),
                   error = function(e) rep(-1e300, n))
    if (any(!is.finite(ld))) return(1e300)
    s <- -sum(ld)
    if (!is.finite(s)) 1e300 else s
  }

  opt <- safe_fit(
    stats::optim(
      par    = c(shape1 = sh1_0, scale = sc_0, shape2 = 1.0),
      fn     = neg_ll,
      method = "L-BFGS-B",
      lower  = c(1e-4, 1e-9, 1e-4),
      upper  = c(50,   1e9,  50)
    ),
    dist_name = "Exponentiated Weibull", method = "MLE"
  )
  if (is.null(opt)) return(NULL)

  params        <- opt$par
  names(params) <- c("shape1", "scale", "shape2")
  ll            <- -opt$value
  k             <- 3L
  list(dist_key = "expweibull", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = (opt$convergence == 0), fit_obj = opt)
}


# -- Gamma 3P MLE (profile likelihood over threshold) -------------------------
#' For fixed thres, Y = x - thres ~ Gamma(shape, scale).
#' The Gamma MLE on Y (Newton-Raphson) gives shape and scale analytically.
#' A 1-D search over thres finds the global MLE robustly.
#' @keywords internal
.mle_gamma3 <- function(x) {
  n     <- length(x)
  x_min <- min(x)
  sx    <- stats::sd(x)

  # Minimum gap between the threshold and the smallest data point.
  # Keeping thres at least this far below x_min prevents the well-known
  # degenerate MLE solution for 3-parameter Gamma (Cheng & Amin 1983):
  # as thres -> x_min the profile likelihood is unbounded because shape -> 0
  # and scale -> Inf, producing a near-step CDF that fits no real data.
  min_gap <- max(1e-6, 1e-3 * sx)

  gamma_mle <- function(thres) {
    if (thres >= x_min - min_gap / 2) return(NULL)
    y  <- x - thres
    if (any(y <= 0)) return(NULL)
    s  <- log(mean(y)) - mean(log(y))
    if (!is.finite(s) || s <= 0) return(NULL)
    sh <- (3 - s + sqrt((s - 3)^2 + 24 * s)) / (12 * s)
    # Guard against the degenerate shape -> 0 solution (occurs as thres -> x_min).
    # shape < 0.05 means the distribution is so J-shaped that it puts almost all
    # mass at the threshold; such fits are practically meaningless.
    if (!is.finite(sh) || sh < 0.05) return(NULL)
    for (iter in seq_len(50L)) {
      g  <- log(sh) - digamma(sh) - s
      dg <- 1 / sh - trigamma(sh)
      if (!is.finite(g) || abs(dg) < 1e-14) break
      sh <- sh - g / dg
      if (!is.finite(sh) || sh < 0.05) return(NULL)
      if (abs(g) < 1e-10) break
    }
    sc <- mean(y) / sh
    if (!is.finite(sc) || sc <= 0) return(NULL)
    ll <- tryCatch(sum(dgamma3(x, shape = sh, scale = sc, thres = thres,
                               log = TRUE)),
                   error = function(e) -1e300)
    if (!is.finite(ll)) return(NULL)
    list(shape = sh, scale = sc, ll = ll)
  }

  # Grid search
  thres_cands <- x_min - c(0.001, 0.01, 0.05, 0.1, 0.2, 0.5, 1.0) * sx
  ll_vals <- vapply(thres_cands, function(th) {
    r <- tryCatch(gamma_mle(th), error = function(e) NULL)
    if (is.null(r)) -1e300 else r$ll
  }, numeric(1L))

  if (all(ll_vals <= -1e299)) return(NULL)
  th_best <- thres_cands[which.max(ll_vals)]

  # Upper bound keeps thres at least min_gap below x_min, avoiding the
  # degenerate zone where shape -> 0 and the profile likelihood is unbounded.
  opt1 <- tryCatch(
    stats::optimize(
      function(th) { r <- gamma_mle(th); if (is.null(r)) -1e300 else r$ll },
      interval = c(th_best - 2 * sx, x_min - min_gap),
      maximum  = TRUE, tol = 1e-8
    ),
    error = function(e) NULL
  )
  if (!is.null(opt1)) th_best <- opt1$maximum

  r_fin <- gamma_mle(th_best)
  if (is.null(r_fin)) return(NULL)

  params <- c(shape = r_fin$shape, scale = r_fin$scale, thres = th_best)
  k      <- 3L
  list(dist_key = "gamma3", method = "MLE", params = params,
       loglik = r_fin$ll, aic = compute_aic(r_fin$ll, k),
       bic = compute_bic(r_fin$ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Log-Normal 3P MLE (profile likelihood over threshold) --------------------
#' For fixed thres, Y = x - thres, then meanlog = mean(log Y), sdlog = sd(log Y).
#' @keywords internal
.mle_lnorm3 <- function(x) {
  n     <- length(x)
  x_min <- min(x)
  sx    <- stats::sd(x)

  # Log-normal 3P has the same unbounded-likelihood degenerate MLE as Gamma 3P:
  # as thres -> x_min, the term -log(y_min) dominates and LL -> +Inf.
  # Two guards:
  #   (a) min_gap_ln  -- keeps thres a meaningful distance below x_min
  #   (b) sdlog guard -- rejects any candidate where sdlog > 3, which corresponds
  #       to a log-normal spanning > 20x in the multiplicative scale. Values
  #       above this are either physically implausible or reflect the degenerate
  #       boundary (sdlog -> Inf as thres -> x_min).
  #   (c) median sanity check -- the fitted median must lie within the data range;
  #       if exp(ml) + thres is far outside [min(x), max(x)], the fit has
  #       collapsed onto the threshold.
  min_gap_ln  <- max(1e-6, 0.01 * sx)   # >=1% of sd below x_min
  x_range     <- diff(range(x))

  profile_ll <- function(thres) {
    if (thres >= x_min - min_gap_ln / 2) return(-1e300)
    y <- x - thres
    if (any(y <= 0)) return(-1e300)
    ly <- log(y)
    ml <- mean(ly)
    sl <- sqrt(mean((ly - ml)^2))
    # Reject degenerate (sl -> Inf) and collapsed (sl -> 0) fits
    if (!is.finite(sl) || sl <= 0 || sl > 3) return(-1e300)
    # Fitted median must be plausible relative to the data
    fitted_median <- thres + exp(ml)
    if (fitted_median < x_min - sx || fitted_median > max(x) + sx) return(-1e300)
    ll <- tryCatch(sum(dlnorm3(x, meanlog = ml, sdlog = sl, thres = thres,
                               log = TRUE)),
                   error = function(e) -1e300)
    if (!is.finite(ll)) -1e300 else ll
  }

  # Grid search
  thres_cands <- x_min - c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0) * sx
  ll_vals <- vapply(thres_cands, function(th)
    tryCatch(profile_ll(th), error = function(e) -1e300), numeric(1L))

  if (all(ll_vals <= -1e299)) return(NULL)
  th_best <- thres_cands[which.max(ll_vals)]

  opt1 <- tryCatch(
    stats::optimize(profile_ll,
                    interval = c(th_best - 2 * sx, x_min - min_gap_ln),
                    maximum  = TRUE, tol = 1e-8),
    error = function(e) NULL
  )
  if (!is.null(opt1)) th_best <- opt1$maximum

  # Final parameters
  y_fin  <- x - th_best
  ly_fin <- log(y_fin)
  ml_hat <- mean(ly_fin)
  sl_hat <- sqrt(mean((ly_fin - ml_hat)^2))
  if (!is.finite(sl_hat) || sl_hat <= 0 || sl_hat > 3) return(NULL)
  fitted_median <- th_best + exp(ml_hat)
  if (fitted_median < x_min - sx || fitted_median > max(x) + sx) return(NULL)
  ll     <- sum(dlnorm3(x, meanlog = ml_hat, sdlog = sl_hat, thres = th_best,
                        log = TRUE))
  if (!is.finite(ll)) return(NULL)

  params <- c(meanlog = ml_hat, sdlog = sl_hat, thres = th_best)
  k      <- 3L
  list(dist_key = "lnorm3", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Exponential MLE (analytical) ---------------------------------------------

#' MLE for Exponential: rate = 1 / mean(x)   (exact, no optimizer)
#'
#' Bypasses fitdistrplus entirely to avoid try()-generated console noise
#' when fitdistrplus's internal finite-difference gradient computation fails
#' for small rate values (large mean).
#' @keywords internal
.mle_exponential <- function(x) {
  if (any(x <= 0)) return(NULL)
  n    <- length(x)
  rate <- 1 / mean(x)
  if (!is.finite(rate) || rate <= 0) return(NULL)
  ll <- n * log(rate) - rate * sum(x)
  if (!is.finite(ll)) return(NULL)
  list(dist_key = "exponential", method = "MLE",
       params   = c(rate = rate),
       loglik   = ll,
       aic      = compute_aic(ll, 1L),
       bic      = compute_bic(ll, 1L, n),
       n_params = 1L, n = n,
       converged = TRUE, fit_obj = NULL)
}


# -- Lindley MLE (closed-form score equation) ----------------------------------

#' MLE for Lindley: solve quadratic score equation
#' Score: 2/theta - 1/(1+theta) = mean(x)  =>  theta^2*xbar + theta*(xbar-1) - 2 = 0
#' @keywords internal
.mle_lindley <- function(x) {
  n    <- length(x)
  xbar <- mean(x)
  disc <- (xbar - 1)^2 + 8 * xbar
  if (disc < 0 || xbar <= 0) return(NULL)
  theta <- (-(xbar - 1) + sqrt(disc)) / (2 * xbar)
  if (!is.finite(theta) || theta <= 0) return(NULL)
  params <- c(theta = theta)
  ll     <- sum(dlindley(x, theta, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 1L
  list(dist_key = "lindley", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Exponentiated Exponential MLE (closed-form MLE) --------------------------

#' MLE for Exponentiated Exponential via profile likelihood.
#' @noRd
.mle_expexp <- function(x) {
  # Profile MLE for expexp(alpha, lambda):
  # Score equation for alpha (given lambda) has closed form:
  #   alpha_hat(lambda) = -n / sum(log(1 - exp(-lambda*x)))
  # Substitute into loglik and optimise over lambda only (1-D problem).
  # This is far more stable than joint 2-D L-BFGS-B.
  n  <- length(x)
  sx <- sum(x)
  neg_profile_ll <- function(loglam) {
    lam  <- exp(loglam)
    u    <- 1.0 - exp(-lam * x)
    u    <- pmax(u, 1e-300)               # guard log(0) for near-zero x
    slgu <- sum(log(u))
    if (!is.finite(slgu) || slgu >= 0) return(1e300)  # alpha would be <= 0
    alpha_hat <- -n / slgu
    ll <- n * log(alpha_hat) + n * loglam - lam * sx + (alpha_hat - 1L) * slgu
    if (!is.finite(ll)) return(1e300)
    -ll
  }
  # Warm start: use method-of-moments hint from registry entry
  entry  <- get_dist_entry("expexp")
  sv0    <- entry$start_fun(x)
  lam0   <- max(sv0$lambda, 1e-6)
  opt <- tryCatch(
    stats::optimise(neg_profile_ll, interval = c(log(1e-6), log(1e4))),
    error = function(e) NULL
  )
  if (is.null(opt) || opt$objective >= 1e299) {
    opt <- tryCatch(
      stats::optimise(neg_profile_ll,
                      interval = c(log(lam0 / 100), log(lam0 * 100))),
      error = function(e) NULL
    )
  }
  if (is.null(opt) || opt$objective >= 1e299) return(NULL)
  lam_hat   <- exp(opt$minimum)
  u_hat     <- pmax(1.0 - exp(-lam_hat * x), 1e-300)
  alpha_hat <- -n / sum(log(u_hat))
  if (!is.finite(alpha_hat) || alpha_hat <= 0) return(NULL)
  params <- c(alpha = alpha_hat, lambda = lam_hat)
  ll     <- sum(dexpexp(x, alpha_hat, lam_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "expexp", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = opt)
}


# -- Nakagami MLE -------------------------------------------------------------

#' MLE for Nakagami: closed-form estimators
#' Omega_hat = mean(x^2);  m_hat from moment equation solved numerically.
#' Actually lmomco-free profile MLE: use optim on (m, Omega) jointly.
#' @keywords internal
.mle_nakagami <- function(x) {
  n     <- length(x)
  Omega <- mean(x^2)
  # Profile: for fixed Omega, MLE of m from d/dm log-lik = 0
  # d/dm [n*m*log(m) - n*lgamma(m) - n*m*log(Omega) + (2m-1)*sum(log(x)) - m/Omega*sum(x^2)] = 0
  # => n*log(m) + n - n*digamma(m) - n*log(Omega) + 2*sum(log(x)) - n = 0
  # => log(m) - digamma(m) = log(Omega) - 2*mean(log(x))
  rhs <- log(Omega) - 2 * mean(log(x))
  obj <- function(m) (log(m) - digamma(m) - rhs)^2
  opt <- tryCatch(
    stats::optimise(obj, interval = c(0.5, 1e4))$minimum,
    error = function(e) NULL
  )
  if (is.null(opt)) return(NULL)
  m_hat <- max(opt, 0.5)
  params <- c(m = m_hat, Omega = Omega)
  ll     <- sum(dnakagami(x, m_hat, Omega, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "nakagami", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Truncated Normal MLE ------------------------------------------------------
# Fix a = min(x) and b = max(x); only estimate mean and sd.
# Freely optimising a and b causes discontinuities: any point outside [a,b]
# gets density 0 -> loglik = -Inf -> AIC = Inf.  Fixing the bounds to data
# extremes is the standard practice and keeps the problem smooth.

#' MLE for Truncated Normal with fixed bounds at data min/max
#' @keywords internal
.mle_truncnorm <- function(x) {
  n     <- length(x)
  # Nudge bounds just beyond the data range so that min(x) and max(x) are
  # never exactly at the truncation boundary.  If bounds equal data extremes,
  # ptruncnorm returns exactly 0 or 1 at those points, log(0) = -Inf blows
  # up the Anderson-Darling statistic and returns p-value = 0 spuriously.
  rng   <- max(x) - min(x)
  eps   <- 1e-6 * rng
  a_fix <- min(x) - eps
  b_fix <- max(x) + eps

  mu0  <- mean(x)
  sig0 <- stats::sd(x)
  if (!is.finite(sig0) || sig0 <= 0) return(NULL)

  # Minimum allowed sigma: 1% of sample SD.  Some versions of the truncnorm
  # package do NOT expose a log= argument for dtruncnorm, so we always compute
  # log(dtruncnorm(...)) rather than dtruncnorm(..., log = TRUE).  Without the
  # sigma floor, Nelder-Mead maximises sum(density) rather than sum(log density),
  # which is unbounded as sigma -> 0, producing AIC ~ -1e6 from a degenerate fit.
  sig_min <- 0.01 * sig0

  nll <- function(pars) {
    mu    <- pars[1]
    sigma <- exp(pars[2])   # log-parameterised to keep sigma > 0
    if (sigma < sig_min) return(1e300)
    dens  <- tryCatch(
      dtruncnorm(x, mean = mu, sd = sigma, a = a_fix, b = b_fix),
      error = function(e) rep(0, n)
    )
    lls <- log(dens)
    if (any(!is.finite(lls))) return(1e300)
    -sum(lls)
  }

  opt <- tryCatch(
    stats::optim(c(mu0, log(sig0)), nll, method = "Nelder-Mead",
                 control = list(maxit = 1000, reltol = 1e-8)),
    error = function(e) NULL
  )
  if (is.null(opt)) return(NULL)

  mu_hat    <- opt$par[1]
  sigma_hat <- exp(opt$par[2])

  # Sanity check: reject degenerate solutions where sigma collapsed near zero.
  if (sigma_hat < sig_min) return(NULL)

  params    <- c(mean = mu_hat, sd = sigma_hat, a = a_fix, b = b_fix)
  ll        <- -opt$value
  if (!is.finite(ll) || ll > 0) return(NULL)

  k <- 4L   # a and b are estimated from data (analytically at min/max), so all
            # four parameters count toward AIC; prevents inflated LL from tight
            # bounds beating unbounded Normal on genuinely normal data
  list(dist_key = "truncnorm", method = "MLE", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = (opt$convergence == 0), fit_obj = NULL)
}


# -- GB2 dedicated MLE ---------------------------------------------------------
#
# KEY DESIGN: GB2 has 4 positive parameters (p, b, a, q).  Generic L-BFGS-B
# on the raw scale frequently gets stuck.  We use a two-phase strategy:
#
# Phase 1 -- coarse Nelder-Mead over a 3x2x3 grid on the log scale.
#   Nelder-Mead needs no gradients, escapes saddle points, and is cheap
#   per evaluation (log-density for n points).  16 runs x 800 iters.
#
# Phase 2 -- BFGS refinement starting from the 3 best Phase-1 results.
#   BFGS is fast near the optimum and gives precise parameter estimates.
#
# Log-moment-based starting point added so the grid is anchored to the data:
#   E[log X] ~ log(b) + (psi(p) - psi(q)) / a  ->  b_start ~ exp(mean(log x))
#   Var[log X] ~ (psi'(p) + psi'(q)) / a^2       ->  a_start ~ sqrt(2psi'(2) / Var[log x])
#
#' @keywords internal
.mle_gb2 <- function(x, entry) {

  n     <- length(x)
  d_fun <- .lookup_dpqr("d", "gb2")
  if (is.null(d_fun)) {
    message("  [FitVerse] No density found for 'gb2' -- skipping MLE.")
    return(NULL)
  }

  # Negative log-likelihood in log-scale reparametrisation
  # theta = (log p, log b, log a, log q) -- all unconstrained, all >0 by exp()
  neg_ll <- function(theta) {
    p <- exp(theta[1]); b <- exp(theta[2])
    a <- exp(theta[3]); q <- exp(theta[4])
    ld <- tryCatch(
      d_fun(x, shape1 = p, scale = b, shape2 = a, shape3 = q, log = TRUE),
      error = function(e) rep(NA_real_, n)
    )
    if (any(!is.finite(ld))) return(1e300)
    -sum(ld)
  }

  # -- Data-adaptive starting values (log-moment method) -----------------------
  ly   <- log(x)
  lmu  <- mean(ly)                         # ~ log(b) when p ~ q
  lvar <- max(stats::var(ly), 1e-6)
  # psi'(2) ~ 0.6449; if p~q~2, a^2 ~ 2psi'(2)/Var[log x]
  a_lm <- sqrt(2 * 0.6449 / lvar)         # log-moment estimate of a
  a_lm <- max(min(a_lm, 10), 0.3)         # clamp to sensible range

  lb_lm <- lmu                             # log(b) ~ mean(log x)
  la_lm <- log(a_lm)

  # Phase-1 grid: (log_p) x (log_b) x (log_a) x (log_q)
  # Use 3 values for p and q (1,2,4), 2 for a (lm estimate and 2), anchored b
  log_pq_grid <- c(0, log(2), log(4))   # p or q = 1, 2, 4
  log_a_grid  <- c(la_lm, log(2))

  starts <- list()
  for (lp in log_pq_grid)
    for (lq in log_pq_grid)
      for (la in log_a_grid)
        starts[[length(starts) + 1]] <- c(lp, lb_lm, la, lq)

  # Phase 1: Nelder-Mead on every grid point (coarse exploration)
  phase1 <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    phase1[[i]] <- tryCatch(
      stats::optim(starts[[i]], neg_ll, method = "Nelder-Mead",
                   control = list(maxit = 500, reltol = 1e-8)),
      error = function(e) NULL
    )
  }

  # Keep finite results, sort by neg_ll value, take top-3
  ok      <- vapply(phase1, function(o) !is.null(o) && is.finite(o$value), logical(1))
  phase1  <- phase1[ok]
  if (length(phase1) == 0) return(NULL)
  vals    <- vapply(phase1, `[[`, numeric(1), "value")
  top3    <- phase1[order(vals)[seq_len(min(3, length(phase1)))]]

  # Phase 2: BFGS refinement from each of the top-3 seeds
  best <- NULL
  for (seed in top3) {
    opt <- tryCatch(
      stats::optim(seed$par, neg_ll, method = "BFGS",
                   control = list(maxit = 1000, reltol = 1e-12)),
      error = function(e) NULL
    )
    if (!is.null(opt) && is.finite(opt$value) &&
        (is.null(best) || opt$value < best$value))
      best <- opt
  }

  if (is.null(best) || !is.finite(best$value)) return(NULL)

  p_hat <- exp(best$par[1]); b_hat <- exp(best$par[2])
  a_hat <- exp(best$par[3]); q_hat <- exp(best$par[4])
  params <- c(shape1 = p_hat, scale = b_hat, shape2 = a_hat, shape3 = q_hat)
  ll     <- -best$value
  if (!is.finite(ll)) return(NULL)

  list(
    dist_key  = "gb2",
    method    = "MLE",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 4L),
    bic       = compute_bic(ll, 4L, n),
    n_params  = 4L,
    n         = n,
    converged = (best$convergence == 0),
    fit_obj   = best
  )
}


# -- NIG MLE via GeneralizedHyperbolic package ---------------------------------
#
# KEY DESIGN: NIG has the constraint |beta| < alpha which is a diagonal
# (non-box) constraint.  L-BFGS-B can only enforce box constraints, so the
# generic .mle_custom_optim route hits a hard wall at |beta| = alpha (density
# returns -Inf there) and the optimizer either gets stuck or fails.
#
# Fix: reparametrize to make every parameter unconstrained:
#   theta1 = mu             (free)
#   theta2 = log(delta)     (so delta = exp(theta2) > 0)
#   theta3 = log(alpha)     (so alpha = exp(theta3) > 0)
#   theta4 = atanh(beta/alpha)  (so beta = alpha*tanh(theta4), |beta| < alpha)
#
# L-BFGS-B sees a smooth, everywhere-finite objective and converges reliably.
#
#' @keywords internal
.mle_nig_reparam <- function(x, entry) {
  n  <- length(x)
  m  <- mean(x)
  v  <- max(stats::var(x), 1e-9)
  s4 <- mean((x - m)^4)
  ek <- max(s4 / v^2 - 3, 0.2)
  d0 <- max(sqrt(3 * v / ek), 1e-4)
  a0 <- max(sqrt(3 / (v * ek)), 1e-4)

  # Starting values in the reparametrized space
  sv <- c(mu       = m,
          ld       = log(d0),   # log(delta)
          la       = log(a0),   # log(alpha)
          eta      = 0)         # atanh(beta/alpha) = 0 => beta = 0

  neg_ll <- function(par) {
    mu    <- par[1]
    delta <- exp(par[2])
    alpha <- exp(par[3])
    beta  <- alpha * tanh(par[4])   # guarantees |beta| < alpha
    ld    <- tryCatch(
      dnig(x, mu = mu, delta = delta, alpha = alpha, beta = beta, log = TRUE),
      error = function(e) rep(-Inf, length(x))
    )
    if (any(!is.finite(ld))) return(1e300)
    s <- -sum(ld)
    if (!is.finite(s)) 1e300 else s
  }

  opt <- tryCatch(
    stats::optim(sv, neg_ll, method = "L-BFGS-B",
                 control = list(maxit = 2000, factr = 1e7)),
    error = function(e) NULL
  )
  # Nelder-Mead fallback if L-BFGS-B fails
  if (is.null(opt) || opt$value >= 1e299) {
    opt <- tryCatch(
      stats::optim(sv, neg_ll, method = "Nelder-Mead",
                   control = list(maxit = 2000, reltol = 1e-8)),
      error = function(e) NULL
    )
  }
  if (is.null(opt) || opt$value >= 1e299) return(NULL)

  # unname() strips the optimizer variable names (mu, ld, la, eta) so that
  # the named c() call below produces clean names (mu, delta, alpha, beta).
  par       <- unname(opt$par)
  mu_hat    <- par[1]
  delta_hat <- exp(par[2])
  alpha_hat <- exp(par[3])
  beta_hat  <- alpha_hat * tanh(par[4])

  params <- c(mu = mu_hat, delta = delta_hat, alpha = alpha_hat, beta = beta_hat)
  ll     <- -opt$value
  if (!is.finite(ll) || ll < -1e290) return(NULL)
  # Reject degenerate fits where delta has collapsed to near-zero
  if (delta_hat < 1e-3 * max(stats::sd(x), 1e-9)) return(NULL)

  k <- 4L
  list(dist_key  = "nig", method = "MLE", params = params,
       loglik    = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params  = k, n = n, converged = (opt$convergence == 0), fit_obj = opt)
}

#' MLE for NIG: uses nigFit() if GeneralizedHyperbolic is available,
#' otherwise falls back to reparametrized pure-R optimizer.
#' @keywords internal
.mle_nig <- function(x, entry) {
  n <- length(x)
  # Try dedicated fitter from GeneralizedHyperbolic
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    fit <- tryCatch(
      suppressMessages(
        do.call(getExportedValue("GeneralizedHyperbolic", "nigFit"),
                list(x, method = "BFGS", silent = TRUE,
                     control = list(maxit = 500)))
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      co <- tryCatch(coef(fit), error = function(e) NULL)
      if (!is.null(co) && length(co) >= 4) {
        if (is.null(names(co)))
          names(co) <- c("mu", "delta", "alpha", "beta")
        if (!all(c("mu","delta","alpha","beta") %in% names(co)))
          names(co) <- c("mu", "delta", "alpha", "beta")
        params <- c(mu    = unname(co["mu"]),    delta = unname(co["delta"]),
                    alpha = unname(co["alpha"]), beta  = unname(co["beta"]))
        ll <- tryCatch(
          sum(dnig(x, mu=params["mu"], delta=params["delta"],
                   alpha=params["alpha"], beta=params["beta"], log=TRUE)),
          error = function(e) NA_real_
        )
        # Reject degenerate fits: if delta << sd(x) the NIG has collapsed to a
        # spike, density >> 1 at data points, and ll is spuriously large positive.
        delta_min <- 1e-3 * max(stats::sd(x), 1e-9)
        if (is.finite(ll) && ll < -1e290) ll <- NA_real_   # sentinel guard
        if (is.finite(ll) && params["delta"] >= delta_min) {
          k <- 4L
          return(list(dist_key="nig", method="MLE", params=params,
                      loglik=ll, aic=compute_aic(ll,k), bic=compute_bic(ll,k,n),
                      n_params=k, n=n, converged=TRUE, fit_obj=fit))
        }
      }
    }
  }
  # Reparametrized pure-R fallback (handles |beta|<alpha constraint correctly)
  .mle_nig_reparam(x, entry)
}


# -- Generalised Hyperbolic MLE -----------------------------------------------
#
# Same reparametrisation strategy as NIG; also adds lambda as a free parameter.
# Reparametrized variables:
#   (mu, log(delta), log(alpha), atanh(beta/alpha), lambda)
#
#' @keywords internal
.mle_ghyp_reparam <- function(x, entry) {
  n  <- length(x)
  m  <- mean(x)
  v  <- max(stats::var(x), 1e-9)
  s4 <- mean((x - m)^4)
  ek <- max(s4 / v^2 - 3, 0.2)
  d0 <- max(sqrt(3 * v / ek), 1e-4)
  a0 <- max(sqrt(3 / (v * ek)), 1e-4)

  sv <- c(mu  = m,
          ld  = log(d0),
          la  = log(a0),
          eta = 0,         # atanh(beta/alpha)
          lam = -0.5)      # start at NIG; optimizer can move lambda freely

  neg_ll <- function(par) {
    mu     <- par[1]
    delta  <- exp(par[2])
    alpha  <- exp(par[3])
    beta   <- alpha * tanh(par[4])
    lambda <- par[5]
    ld     <- tryCatch(
      dghyp(x, lambda = lambda, alpha = alpha, beta = beta,
            delta = delta, mu = mu, log = TRUE),
      error = function(e) rep(-Inf, length(x))
    )
    if (any(!is.finite(ld))) return(1e300)
    s <- -sum(ld)
    if (!is.finite(s)) 1e300 else s
  }

  opt <- tryCatch(
    stats::optim(sv, neg_ll, method = "L-BFGS-B",
                 control = list(maxit = 2000, factr = 1e7)),
    error = function(e) NULL
  )
  if (is.null(opt) || opt$value >= 1e299) {
    opt <- tryCatch(
      stats::optim(sv, neg_ll, method = "Nelder-Mead",
                   control = list(maxit = 2000, reltol = 1e-8)),
      error = function(e) NULL
    )
  }
  if (is.null(opt) || opt$value >= 1e299) return(NULL)

  # unname() strips optimizer variable names (mu, ld, la, eta, lam) so that
  # the named c() call produces clean names (lambda, alpha, beta, delta, mu).
  par        <- unname(opt$par)
  mu_hat     <- par[1]
  delta_hat  <- exp(par[2])
  alpha_hat  <- exp(par[3])
  beta_hat   <- alpha_hat * tanh(par[4])
  lambda_hat <- par[5]

  params <- c(lambda = lambda_hat, alpha = alpha_hat, beta = beta_hat,
              delta  = delta_hat,  mu    = mu_hat)
  ll     <- -opt$value
  if (!is.finite(ll)) return(NULL)

  # Degenerate guard: |beta|/alpha > 0.98 -> CDF integration will hang in GoF
  if (alpha_hat > 0 && abs(beta_hat) / alpha_hat > 0.98) return(NULL)

  k <- 5L
  list(dist_key  = "ghyp", method = "MLE", params = params,
       loglik    = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params  = k, n = n, converged = (opt$convergence == 0), fit_obj = opt)
}

#' MLE for GHyp: uses ghypFit() if GeneralizedHyperbolic is available,
#' otherwise falls back to reparametrized pure-R optimizer.
#' @keywords internal
.mle_ghyp <- function(x, entry) {
  n <- length(x)
  if (requireNamespace("GeneralizedHyperbolic", quietly = TRUE)) {
    fit <- tryCatch(
      suppressMessages(
        do.call(getExportedValue("GeneralizedHyperbolic", "ghypFit"),
                list(x, method = "BFGS", silent = TRUE,
                     control = list(maxit = 500)))
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      co <- tryCatch(coef(fit), error = function(e) NULL)
      if (!is.null(co) && length(co) >= 5) {
        # coef.ghypFit returns named vector with names: mu delta alpha beta lambda
        params <- tryCatch(
          c(lambda = unname(co["lambda"]), alpha = unname(co["alpha"]),
            beta   = unname(co["beta"]),   delta = unname(co["delta"]),
            mu     = unname(co["mu"])),
          error = function(e) NULL
        )
        if (!is.null(params) && all(is.finite(params))) {
          # Degenerate guard: when |beta| is very close to alpha, the GHyp
          # distribution becomes one-sided and its CDF requires essentially
          # infinite numerical integration steps.  The GoF test then hangs.
          # Reject such fits to keep the table and GoF tests well-behaved.
          alpha_v <- params["alpha"]; beta_v <- params["beta"]
          if (is.finite(alpha_v) && alpha_v > 0 &&
              abs(beta_v) / alpha_v > 0.98) return(NULL)

          ll <- tryCatch(
            sum(dghyp(x, lambda=params["lambda"], alpha=params["alpha"],
                      beta=params["beta"], delta=params["delta"],
                      mu=params["mu"], log=TRUE)),
            error = function(e) NA_real_
          )
          if (is.finite(ll)) {
            k <- 5L
            return(list(dist_key="ghyp", method="MLE", params=params,
                        loglik=ll, aic=compute_aic(ll,k), bic=compute_bic(ll,k,n),
                        n_params=k, n=n, converged=TRUE, fit_obj=fit))
          }
        }
      }
    }
  }
  # Reparametrized pure-R fallback
  .mle_ghyp_reparam(x, entry)
}


# -- Batch MLE: fit all requested distributions --------------------------------

#' Fit a list of distributions by MLE and collect results
#'
#' @param x      Numeric vector of clean data.
#' @param keys   Character vector of distribution keys to try.
#' @return Named list of fit-result lists (NULL entries for failures).
#' @keywords internal
fit_mle_all <- function(x, keys) {
  .warn_unsupported_method(keys, "supports_mle", "MLE")
  results <- lapply(keys, function(k) fit_mle(x, k))
  names(results) <- keys
  # Drop NULL entries (failed fits)
  Filter(Negate(is.null), results)
}

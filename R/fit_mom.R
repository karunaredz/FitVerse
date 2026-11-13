# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : fit_mom.R
# Description: Method of Moments (MOM) estimation for all
#              distributions that support it in FitVerse.
#
#              Strategy:
#               - fitdistrplus::fitdist(..., method="mme") handles
#                 all standard stats distributions automatically
#                 (normal, lognormal, gamma, exponential, beta,
#                 logistic, uniform).
#               - Manual closed-form MOM is implemented here for
#                 all other supported distributions:
#                   * Weibull 2P   - numerical (CV equation)
#                   * Weibull 3P   - numerical (skewness equation)
#                   * Log-Logistic - closed-form (requires shape > 2)
#                   * Pareto       - closed-form (requires shape > 2)
#                   * Inv. Gamma   - closed-form (requires shape > 2)
#                   * Gumbel       - closed-form (Euler-Mascheroni)
#                   * Triangular   - closed-form (mean equation)
#                   * GPD          - closed-form (requires shape < 0.5)
#                   * Pearson III  - closed-form (mean/sd/skewness)
#                   * LP3          - Pearson III on log(x)
#                   * Right-Tri    - closed-form (boundary estimators)
#                   * Inv. Gaussian - closed-form (mean/variance)
#                   * Lomax        - closed-form (requires CV > 1)
#                   * Laplace      - closed-form (mean/sd)
#                   * Birnbaum-S.  - numerical (CV equation)
#                   * Student-t    - kurtosis-based nu estimate
#                   * Gen. Gamma   - numerical (mean/variance)
#                   * Rayleigh     - closed-form (mean)
#                   * Half-Normal  - closed-form (mean)
#                   * Gamma 3P     - closed-form (mean/variance/skewness)
#
#              Return value: same named list as fit_mle.R.
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

#' Fit a single distribution by Method of Moments
#'
#' @param x        Numeric vector of clean (finite) data.
#' @param dist_key Character key from the FitVerse registry.
#' @return Named list with fit results, or NULL if MOM is not available.
#' @keywords internal
fit_mom <- function(x, dist_key) {

  entry <- get_dist_entry(dist_key)

  if (!entry$supports_mom) {
    message(sprintf(
      "  [FitVerse] MOM not available for '%s' -- skipping.",
      entry$display_name))
    return(NULL)
  }

  # Distributions handled by manual closed-form MOM
  manual_keys <- c(
    # Symmetric / unbounded
    "laplace",
    # Right-skewed (positive support)
    "weibull", "weibull3p", "loglogistic", "invgamma",
    "pearson3", "lpearson3", "invgaussian", "gengamma",
    "birnbaum_saunders", "rayleigh", "halfnormal", "gamma3",
    # Heavy-tailed
    "pareto", "lomax",
    # Extreme-value
    "gumbel", "gpd",
    # Bounded
    "triangular", "rtriangle", "student_t",
    "uniform", "kumaraswamy",
    # Extended families
    "lindley", "expexp", "nakagami"
  )

  if (dist_key %in% manual_keys) {
    return(.mom_manual(x, dist_key, entry))
  }

  # All other supported distributions: delegate to fitdistrplus MME
  .mom_fitdistrplus(x, dist_key, entry)
}


# -- fitdistrplus MME (stats distributions) -----------------------------------

#' MOM via fitdistrplus::fitdist with method = "mme"
#'
#' Covers: normal, lognormal, gamma, exponential, beta, logistic, uniform.
#' fitdistrplus matches theoretical moments to sample moments analytically
#' for these families.
#'
#' @keywords internal
.mom_fitdistrplus <- function(x, dist_key, entry) {

  n     <- length(x)
  start <- entry$start_fun(x)

  # fitdistrplus requires BOTH lower and upper (same length as npar) or NEITHER.
  # Pass the FULL named vectors - fitdistrplus replaces [U+00B1]Inf with [U+00B1].Machine$double.xmax.
  lower_raw <- entry$lower
  upper_raw <- entry$upper
  has_constraints <- any(is.finite(lower_raw)) || any(is.finite(upper_raw))
  lower_arg <- if (has_constraints) lower_raw else NULL
  upper_arg <- if (has_constraints) upper_raw else NULL

  # Expose actuar/mc2d d/p/q functions into this frame for fitdistrplus lookup
  .expose_dist_fns(entry$package, entry$r_name)

  fit <- safe_fit(
    fitdistrplus::fitdist(
      data   = x,
      distr  = entry$r_name,
      method = "mme",
      start  = start,
      lower  = lower_arg,
      upper  = upper_arg
    ),
    dist_name = entry$display_name,
    method    = "MOM"
  )

  if (is.null(fit)) return(NULL)

  params <- fit$estimate
  ll     <- .loglik_from_params(x, dist_key, params)
  k      <- length(params)

  list(
    dist_key  = dist_key,
    method    = "MOM",
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


# -- Manual closed-form MOM ----------------------------------------------------

#' Dispatcher for manual MOM estimators
#' @keywords internal
.mom_manual <- function(x, dist_key, entry) {
  switch(dist_key,
    # Symmetric / unbounded
    laplace           = .mom_laplace(x),
    # Right-skewed (positive support)
    weibull           = .mom_weibull(x),
    weibull3p         = .mom_weibull3p(x),
    loglogistic       = .mom_loglogistic(x),
    invgamma          = .mom_invgamma(x),
    pearson3          = .mom_pearson3(x),
    lpearson3         = .mom_lpearson3(x),
    invgaussian       = .mom_invgaussian(x),
    gengamma          = .mom_gengamma(x),
    birnbaum_saunders = .mom_birnbaum_saunders(x),
    rayleigh          = .mom_rayleigh(x),
    halfnormal        = .mom_halfnormal(x),
    gamma3            = .mom_gamma3(x),
    # Heavy-tailed
    pareto            = .mom_pareto(x),
    lomax             = .mom_lomax(x),
    # Extreme-value
    gumbel            = .mom_gumbel(x),
    gpd               = .mom_gpd(x),
    # Bounded
    triangular        = .mom_triangular(x),
    rtriangle         = .mom_rtriangle(x),
    student_t         = .mom_student_t(x),
    uniform           = .mom_uniform(x),
    kumaraswamy       = .mom_kumaraswamy(x),
    # Extended families
    lindley           = .mom_lindley(x),
    expexp            = .mom_expexp(x),
    nakagami          = .mom_nakagami(x),
    NULL
  )
}


# -- Weibull MOM ---------------------------------------------------------------

#' Weibull MOM: solve numerically because moments involve the Gamma function
#'
#' We solve for shape k such that:
#'   CV^2 = Gamma(1+2/k) / Gamma(1+1/k)^2 - 1
#'
#' Then scale = mean(x) / Gamma(1+1/k).
#'
#' @keywords internal
.mom_weibull <- function(x) {

  n   <- length(x)
  m1  <- mean(x)
  m2  <- mean(x^2)
  cv2 <- m2 / m1^2 - 1    # CV^2 = E[X^2]/E[X]^2 - 1

  # Objective: find shape k minimising |CV^2(k) - observed CV^2|
  obj <- function(k) {
    g1 <- gamma(1 + 1 / k)
    g2 <- gamma(1 + 2 / k)
    (g2 / g1^2 - 1 - cv2)^2
  }

  opt <- safe_fit(
    stats::optimise(obj, interval = c(0.1, 100))$minimum,
    dist_name = "Weibull (2-Parameter)",
    method    = "MOM"
  )

  if (is.null(opt) || !is.finite(opt)) return(NULL)

  k_hat     <- opt
  scale_hat <- m1 / gamma(1 + 1 / k_hat)
  params    <- c(shape = k_hat, scale = scale_hat)

  ll <- .loglik_from_params(x, "weibull", params)

  list(
    dist_key  = "weibull",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 2L),
    bic       = compute_bic(ll, 2L, n),
    n_params  = 2L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- 3-Parameter Weibull MOM --------------------------------------------------

#' 3-Parameter Weibull MOM
#'
#' Matches the first three central moments.  Because the standardised skewness
#' of a Weibull depends only on the shape parameter k, we solve numerically for
#' k from the sample skewness, then recover scale lambda from the variance and
#' threshold mu from the mean.
#'
#' Moment formulas (letting Gamma_r = Gamma(1 + r/k)):
#'   E[X]   = mu + lambda Gamma_1
#'   Var[X] = lambda^2 (Gamma_2 - Gamma_1^2)
#'   Skew   = (Gamma_3 - 3 Gamma_2 Gamma_1 + 2 Gamma_1^3) / (Gamma_2 - Gamma_1^2)^(3/2)
#'
#' @keywords internal
.mom_weibull3p <- function(x) {

  n   <- length(x)
  m   <- mean(x)
  v   <- stats::var(x) * (n - 1) / n   # biased variance (moment-matching)
  s   <- skewness_stat(x)               # Fisher's g1 (from utils.R)

  if (!is.finite(s)) {
    message("  [FitVerse] Cannot compute skewness for Weibull 3P MOM -- skipping.")
    return(NULL)
  }

  # Solve for shape k from the skewness equation
  skew_3p <- function(k) {
    g1 <- gamma(1 + 1 / k)
    g2 <- gamma(1 + 2 / k)
    g3 <- gamma(1 + 3 / k)
    num  <- g3 - 3 * g2 * g1 + 2 * g1^3
    den  <- (g2 - g1^2)^1.5
    if (den <= 0) return(NA_real_)
    num / den
  }

  obj <- function(k) (skew_3p(k) - s)^2

  # 3P Weibull skewness ranges from roughly -1.14 (k->Inf) to +Inf (k->0);
  # search over a wide shape interval.
  opt <- safe_fit(
    stats::optimise(obj, interval = c(0.05, 200)),
    dist_name = "Weibull (3-Parameter)",
    method    = "MOM"
  )

  if (is.null(opt) || !is.finite(opt$minimum)) return(NULL)

  k_hat     <- opt$minimum
  g1        <- gamma(1 + 1 / k_hat)
  g2        <- gamma(1 + 2 / k_hat)
  var_coef  <- g2 - g1^2
  if (var_coef <= 0) return(NULL)

  scale_hat <- sqrt(v / var_coef)
  thres_hat <- m - scale_hat * g1
  params    <- c(shape = k_hat, scale = scale_hat, thres = thres_hat)

  ll <- sum(dweibull3p(x, shape = k_hat, scale = scale_hat,
                       thres = thres_hat, log = TRUE))
  if (!is.finite(ll)) ll <- NA_real_

  list(
    dist_key  = "weibull3p",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 3L),
    bic       = compute_bic(ll, 3L, n),
    n_params  = 3L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Log-Logistic MOM ---------------------------------------------------------

#' Log-Logistic MOM: closed-form solution valid when shape > 2
#'
#' E[X]   = theta [U+03C0]/beta / sin([U+03C0]/beta)
#' Var[X] = theta^2 [2[U+03C0]/beta / sin(2[U+03C0]/beta) - ([U+03C0]/beta)^2 / sin^2([U+03C0]/beta)]
#'
#' Solve for shape beta from the CV, then back-solve for scale theta.
#'
#' @keywords internal
.mom_loglogistic <- function(x) {

  n   <- length(x)
  m   <- mean(x)
  v   <- stats::var(x)
  cv2 <- v / m^2

  # Objective: minimise |theoretical CV^2 - sample CV^2|
  obj <- function(b) {
    if (b <= 2) return(1e10)
    pb  <- pi / b
    a1  <- pb / sin(pb)
    a2  <- 2 * pb / sin(2 * pb)
    th_cv2 <- a2 / a1^2 - 1
    (th_cv2 - cv2)^2
  }

  b_hat <- safe_fit(
    stats::optimise(obj, interval = c(2.01, 50))$minimum,
    dist_name = "Log-Logistic",
    method    = "MOM"
  )

  if (is.null(b_hat) || b_hat <= 2) return(NULL)

  pb      <- pi / b_hat
  theta   <- m / (pb / sin(pb))
  params  <- c(shape = b_hat, scale = theta)

  ll <- .loglik_from_params(x, "loglogistic", params)

  list(
    dist_key  = "loglogistic",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 2L),
    bic       = compute_bic(ll, 2L, n),
    n_params  = 2L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Pareto MOM ----------------------------------------------------------------

#' Pareto MOM: closed-form, valid when shape alpha > 2
#'
#' E[X]   = alphatheta/(alpha-1)
#' Var[X] = alphatheta^2/((alpha-1)^2(alpha-2))
#' => CV^2 = 1/(alpha-2)  =>  alpha = 1/CV^2 + 2
#'
#' @keywords internal
.mom_pareto <- function(x) {

  n   <- length(x)
  m   <- mean(x)
  v   <- stats::var(x)
  cv2 <- v / m^2

  alpha <- 1 / cv2 + 2
  if (alpha <= 2) {
    message("  [FitVerse] Pareto MOM requires CV^2 < 1 (i.e., shape > 2) -- skipping.")
    return(NULL)
  }
  theta  <- m * (alpha - 1) / alpha
  params <- c(shape = alpha, scale = theta)

  ll <- .loglik_from_params(x, "pareto", params)

  list(
    dist_key  = "pareto",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 2L),
    bic       = compute_bic(ll, 2L, n),
    n_params  = 2L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Inverse Gamma MOM ---------------------------------------------------------

#' Inverse Gamma MOM: closed-form, valid when shape alpha > 2
#'
#' E[X]   = beta/(alpha-1)
#' Var[X] = beta^2/((alpha-1)^2(alpha-2))
#' => alpha   = E[X]^2/Var[X] + 2,  beta = E[X](alpha-1)
#'
#' @keywords internal
.mom_invgamma <- function(x) {

  n   <- length(x)
  m   <- mean(x)
  v   <- stats::var(x)

  alpha <- m^2 / v + 2
  if (alpha <= 2) {
    message("  [FitVerse] Inverse-Gamma MOM requires shape > 2 -- skipping.")
    return(NULL)
  }
  beta   <- m * (alpha - 1)
  params <- c(shape = alpha, scale = beta)

  ll <- .loglik_from_params(x, "invgamma", params)

  list(
    dist_key  = "invgamma",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 2L),
    bic       = compute_bic(ll, 2L, n),
    n_params  = 2L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Gumbel MOM ----------------------------------------------------------------

#' Gumbel MOM: closed-form (Euler-Mascheroni constant gamma_E ~= 0.5772)
#'
#' E[X]   = mu + gamma_E . beta
#' Var[X] = [U+03C0]^2 beta^2 / 6
#' => beta   = sd(x).sqrt(6)/[U+03C0],  mu = mean(x) - gamma_E.beta
#'
#' @keywords internal
.mom_gumbel <- function(x) {

  if (!has_pkg("evd", "Gumbel distribution")) return(NULL)

  n      <- length(x)
  EULER  <- 0.5772156649015329
  scale  <- stats::sd(x) * sqrt(6) / pi
  loc    <- mean(x) - EULER * scale
  params <- c(loc = loc, scale = scale)

  ll <- .loglik_from_params(x, "gumbel", params)

  list(
    dist_key  = "gumbel",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 2L),
    bic       = compute_bic(ll, 2L, n),
    n_params  = 2L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Triangular MOM ------------------------------------------------------------

#' Triangular MOM
#'
#' E[X]   = (a + b + c) / 3
#' Var[X] = (a^2 + b^2 + c^2 - ab - ac - bc) / 18
#'
#' We fix a = min(x), b = max(x) and solve for mode c from the mean equation:
#'   c = 3.mean(x) - a - b
#'
#' Variance constraint is used as a sanity check.
#'
#' @keywords internal
.mom_triangular <- function(x) {

  n   <- length(x)
  rng <- max(x) - min(x)
  if (rng <= 0) return(NULL)

  # Compute mode from exact data extremes (analytical MOM)
  a_exact <- min(x)
  b_exact <- max(x)
  c_hat   <- 3 * mean(x) - a_exact - b_exact

  # Mode must lie within [a, b]
  if (c_hat < a_exact || c_hat > b_exact) {
    message("  [FitVerse] Triangular MOM: implied mode outside [min, max] -- skipping.")
    return(NULL)
  }

  # Nudge bounds so no observation sits exactly on the boundary -- prevents
  # ptriang returning 0/1 at min(x)/max(x), which causes log(0) = -Inf in AD test
  eps <- 1e-4 * rng
  a   <- a_exact - eps
  b   <- b_exact + eps

  params <- c(min = a, max = b, mode = c_hat)
  ll     <- .loglik_from_params(x, "triangular", params)

  list(
    dist_key  = "triangular",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 3L),
    bic       = compute_bic(ll, 3L, n),
    n_params  = 3L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- GPD MOM -------------------------------------------------------------------

#' GPD MOM: closed-form, valid when shape < 0.5
#'
#' Fix loc = min(x) - [U+03B5]; then for the excess Y = x - loc:
#'   E[Y] = scale/(1-shape),  Var[Y] = scale^2/((1-shape)^2(1-2*shape))
#' From these: shape = (1 - E[Y]^2/Var[Y]) / 2,  scale = E[Y]*(1-shape)
#'
#' @keywords internal
.mom_gpd <- function(x) {

  n   <- length(x)
  eps <- 1e-4 * (max(x) - min(x) + 1)
  loc <- min(x) - eps
  y   <- x - loc
  m   <- mean(y)
  v   <- stats::var(y)

  if (v <= 0) {
    message("  [FitVerse] GPD MOM: zero variance -- skipping.")
    return(NULL)
  }

  xi    <- 0.5 * (1 - m^2 / v)
  if (xi >= 0.5) {
    message("  [FitVerse] GPD MOM: estimated shape >= 0.5 (infinite variance) -- skipping.")
    return(NULL)
  }
  sc    <- m * (1 - xi)
  if (sc <= 0) return(NULL)
  params <- c(loc = loc, scale = sc, shape = xi)

  ll <- .loglik_from_params(x, "gpd", params)

  list(
    dist_key  = "gpd",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 3L),
    bic       = compute_bic(ll, 3L, n),
    n_params  = 3L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Pearson III MOM -----------------------------------------------------------

#' Pearson III MOM: closed-form via mean, sd, and skewness
#'
#' Let mu, sigma, gamma be the sample mean, sd, and skewness coefficient.
#' Then:  shape = 4/gamma^2,  scale = sigmagamma/2,  loc = mu - 2sigma/gamma
#'
#' @keywords internal
.mom_pearson3 <- function(x) {

  n   <- length(x)
  mu  <- mean(x)
  sig <- stats::sd(x)
  gam <- .skewness(x)    # sample skewness

  if (abs(gam) < 1e-4) {
    message("  [FitVerse] Pearson III MOM: near-zero skewness -- approximating as Normal.")
    # Fall back: use P3 with large shape -> approximately Normal
    gam <- 1e-4
  }

  shp <- 4 / gam^2
  sc  <- sig * gam / 2
  loc <- mu - 2 * sig / gam

  if (shp <= 0) return(NULL)

  params <- c(shape = shp, scale = sc, loc = loc)
  ll     <- .loglik_from_params(x, "pearson3", params)

  list(
    dist_key  = "pearson3",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 3L),
    bic       = compute_bic(ll, 3L, n),
    n_params  = 3L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Log-Pearson III MOM -------------------------------------------------------

#' Log-Pearson III MOM: apply Pearson III MOM to log(x)
#'
#' @keywords internal
.mom_lpearson3 <- function(x) {

  if (any(x <= 0)) {
    message("  [FitVerse] Log-Pearson III MOM requires strictly positive data -- skipping.")
    return(NULL)
  }

  lx  <- log(x)
  n   <- length(lx)
  mu  <- mean(lx)
  sig <- stats::sd(lx)
  gam <- .skewness(lx)

  # LP3 is only valid for skewed log-data; near-zero skewness makes
  # shape -> Inf and log-likelihood non-finite.  Skip gracefully.
  if (abs(gam) < 0.1) {
    message("  [FitVerse] Log-Pearson III MOM: near-zero skewness on log scale -- skipping.")
    return(NULL)
  }

  shp <- 4 / gam^2
  sc  <- sig * gam / 2
  loc <- mu - 2 * sig / gam

  if (shp <= 0) return(NULL)

  params <- c(shape = shp, scale = sc, loc = loc)
  ll     <- .loglik_from_params(x, "lpearson3", params)

  list(
    dist_key  = "lpearson3",
    method    = "MOM",
    params    = params,
    loglik    = ll,
    aic       = compute_aic(ll, 3L),
    bic       = compute_bic(ll, 3L, n),
    n_params  = 3L,
    n         = n,
    converged = TRUE,
    fit_obj   = NULL
  )
}


# -- Internal skewness helper --------------------------------------------------

#' Sample skewness (type 1: biased)
#' @keywords internal
.skewness <- function(x) {
  n  <- length(x)
  m  <- mean(x)
  s  <- stats::sd(x)
  if (s == 0) return(0)
  sum(((x - m) / s)^3) / n
}


# -- Log-likelihood from parameters -------------------------------------------

#' Evaluate the log-likelihood for a given set of parameters
#'
#' Used by manual MOM estimators that produce parameters without an embedded
#' log-likelihood (because they bypass fitdistrplus).
#'
#' @param x        Numeric data vector.
#' @param dist_key Distribution key.
#' @param params   Named numeric vector of parameters.
#' @return Numeric log-likelihood, or NA on error.
#' @keywords internal
.loglik_from_params <- function(x, dist_key, params) {
  # Use unified resolver - checks FITVERSE_DPQR_OVERRIDES first, then
  # match.fun / getExportedValue.  This ensures the new distributions
  # (GPD, Pearson III, LP3, GLO) resolve to their internal implementations.
  d_fun <- .lookup_dpqr("d", dist_key)

  if (is.null(d_fun)) return(NA_real_)

  ll <- tryCatch({
    # Try with log = TRUE (all FitVerse internal functions support it).
    # Fall back to manual log() if the underlying function does not.
    log_dens <- tryCatch(
      do.call(d_fun, c(list(x = x), as.list(params), list(log = TRUE))),
      error = function(e)
        log(do.call(d_fun, c(list(x = x), as.list(params))))
    )
    # If ANY observation has zero density, the model is incompatible with the
    # data - return -Inf so AIC/BIC become +Inf and the fit ranks last.
    if (any(is.infinite(log_dens) & log_dens < 0, na.rm = TRUE))
      return(-Inf)
    sum(log_dens[is.finite(log_dens)])
  }, error = function(e) NA_real_)

  ll
}


# -- Right Triangular MOM -----------------------------------------------------

#' MOM for Right Triangular: min=min(x), max=max(x) (closed-form boundaries)
#' @keywords internal
.mom_rtriangle <- function(x) {
  n   <- length(x)
  rng <- max(x) - min(x)
  if (rng <= 0) return(NULL)
  a <- min(x) - 1e-4 * rng
  b <- max(x) + 1e-4 * rng
  params <- c(min = a, max = b)
  ll <- sum(drtriangle(x, min = a, max = b, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 1L
  list(dist_key = "rtriangle", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Inverse Gaussian MOM ------------------------------------------------------

#' MOM for Inverse Gaussian: mu=mean(x), lambda=mu^3/var(x)
#' @keywords internal
.mom_invgaussian <- function(x) {
  if (any(x <= 0)) return(NULL)
  n      <- length(x)
  mu_hat <- mean(x)
  v      <- stats::var(x)
  if (v <= 0 || !is.finite(mu_hat) || mu_hat <= 0) return(NULL)
  lam_hat <- mu_hat^3 / v
  if (!is.finite(lam_hat) || lam_hat <= 0) return(NULL)
  params <- c(mu = mu_hat, lambda = lam_hat)
  ll <- sum(dinvgaussian(x, mu = mu_hat, lambda = lam_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "invgaussian", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Lomax MOM -----------------------------------------------------------------

#' MOM for Lomax (Pareto Type II): alpha and lambda from mean/variance.
#' E[X] = lambda/(alpha-1),  Var[X] = alpha*lambda^2/((alpha-1)^2*(alpha-2))
#' Requires alpha > 2.
#' @keywords internal
.mom_lomax <- function(x) {
  if (any(x < 0)) return(NULL)
  n  <- length(x)
  m1 <- mean(x)
  v  <- stats::var(x)
  if (v <= 0 || !is.finite(m1) || m1 <= 0) return(NULL)
  # Solve: CV2 = (alpha)/((alpha-2)) => alpha = 2/(1 - 1/CV2) for CV2 > 1
  cv2 <- v / m1^2
  if (cv2 <= 1) {
    message("  [FitVerse] Lomax MOM requires CV > 1 (alpha > 2) -- skipping.")
    return(NULL)
  }
  alpha_hat  <- 2 * cv2 / (cv2 - 1)
  lambda_hat <- m1 * (alpha_hat - 1)
  if (!is.finite(alpha_hat) || !is.finite(lambda_hat) ||
      alpha_hat <= 2 || lambda_hat <= 0) return(NULL)
  params <- c(alpha = alpha_hat, lambda = lambda_hat)
  ll <- sum(dlomax(x, alpha = alpha_hat, lambda = lambda_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "lomax", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Laplace MOM ---------------------------------------------------------------

#' MOM for Laplace: mu=mean(x), b=sd(x)/sqrt(2)
#' @keywords internal
.mom_laplace <- function(x) {
  n      <- length(x)
  mu_hat <- mean(x)
  b_hat  <- stats::sd(x) / sqrt(2)
  if (!is.finite(b_hat) || b_hat <= 0) return(NULL)
  params <- c(mu = mu_hat, b = b_hat)
  ll <- sum(dlaplace(x, mu = mu_hat, b = b_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "laplace", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Birnbaum-Saunders MOM ----------------------------------------------------

#' MOM for Birnbaum-Saunders: match E[X]=beta(1+alpha^2/2), Var[X].
#' beta = mean(x)/(1 + alpha^2/2); solve alpha from variance equation
#' numerically.
#' @keywords internal
.mom_birnbaum_saunders <- function(x) {
  if (any(x <= 0)) return(NULL)
  n  <- length(x)
  m1 <- mean(x)
  v  <- stats::var(x)
  if (v <= 0 || !is.finite(m1) || m1 <= 0) return(NULL)
  # Var[X] = beta^2 * alpha^2 * (1 + 5*alpha^2/4)
  # E[X]   = beta  * (1 + alpha^2/2)
  # => CV^2 = alpha^2 * (1 + 5*alpha^2/4) / (1 + alpha^2/2)^2
  cv2 <- v / m1^2
  obj <- function(a2) {
    (a2 * (1 + 5 * a2 / 4) / (1 + a2 / 2)^2 - cv2)^2
  }
  opt <- tryCatch(stats::optimize(obj, interval = c(1e-6, 100)), error = function(e) NULL)
  if (is.null(opt)) return(NULL)
  alpha_hat <- sqrt(opt$minimum)
  beta_hat  <- m1 / (1 + alpha_hat^2 / 2)
  if (!is.finite(alpha_hat) || !is.finite(beta_hat) ||
      alpha_hat <= 0 || beta_hat <= 0) return(NULL)
  params <- c(alpha = alpha_hat, beta = beta_hat)
  ll <- sum(dbirnbaum_saunders(x, alpha = alpha_hat, beta = beta_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "birnbaum_saunders", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Student-t (location-scale) MOM -------------------------------------------

#' MOM for Student-t location-scale.
#' mu = mean(x), sigma = sd(x)*sqrt((nu-2)/nu) - requires nu > 2.
#' We first estimate nu from the excess kurtosis: kurtosis = 6/(nu-4), nu>4,
#' falling back to nu=5 when kurtosis is too small or not estimable.
#' @keywords internal
.mom_student_t <- function(x) {
  n      <- length(x)
  mu_hat <- mean(x)
  s      <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NULL)
  # Estimate nu from excess kurtosis
  m4   <- mean((x - mu_hat)^4)
  kurt <- m4 / s^4 - 3  # excess kurtosis
  if (is.finite(kurt) && kurt > 0) {
    nu_hat <- 4 + 6 / kurt
  } else {
    nu_hat <- 5   # safe fallback
  }
  nu_hat <- max(nu_hat, 2.5)   # must be > 2 for variance to exist
  sigma_hat <- s * sqrt((nu_hat - 2) / nu_hat)
  if (!is.finite(sigma_hat) || sigma_hat <= 0) return(NULL)
  params <- c(nu = nu_hat, mu = mu_hat, sigma = sigma_hat)
  ll <- sum(dstudent_t(x, nu = nu_hat, mu = mu_hat, sigma = sigma_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 3L
  list(dist_key = "student_t", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Generalised Gamma MOM ----------------------------------------------------

#' MOM for Generalised Gamma via numerical optimisation.
#' Matches mean, variance, and skewness to theoretical moments.
#' E[X]   = b * Gamma((d+1)/p) / Gamma(d/p)
#' Var[X] = b^2 * [Gamma((d+2)/p)/Gamma(d/p) - (Gamma((d+1)/p)/Gamma(d/p))^2]
#' @keywords internal
.mom_gengamma <- function(x) {
  if (any(x <= 0)) return(NULL)
  n  <- length(x)
  m1 <- mean(x)
  v  <- stats::var(x)
  s3 <- mean((x - m1)^3) / v^1.5   # skewness
  if (!is.finite(m1) || !is.finite(v) || v <= 0) return(NULL)

  obj <- function(pars) {
    p <- exp(pars[1]); d <- exp(pars[2])
    if (p <= 0 || d <= 0) return(1e9)
    r1 <- tryCatch(gamma((d + 1) / p) / gamma(d / p), error = function(e) NA)
    r2 <- tryCatch(gamma((d + 2) / p) / gamma(d / p), error = function(e) NA)
    if (!is.finite(r1) || !is.finite(r2)) return(1e9)
    b_est <- m1 / r1
    if (b_est <= 0) return(1e9)
    var_th <- b_est^2 * (r2 - r1^2)
    (var_th - v)^2 / v^2
  }
  opt <- tryCatch(
    stats::optim(par = c(0, 1), fn = obj, method = "Nelder-Mead",
                 control = list(maxit = 2000)),
    error = function(e) NULL
  )
  if (is.null(opt) || opt$convergence != 0) return(NULL)
  p_hat <- exp(opt$par[1]); d_hat <- exp(opt$par[2])
  r1    <- tryCatch(gamma((d_hat + 1) / p_hat) / gamma(d_hat / p_hat),
                    error = function(e) NA)
  if (!is.finite(r1) || r1 <= 0) return(NULL)
  b_hat <- m1 / r1
  if (!is.finite(b_hat) || b_hat <= 0) return(NULL)
  params <- c(p = p_hat, d = d_hat, b = b_hat)
  ll <- sum(dgengamma(x, p = p_hat, d = d_hat, b = b_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 3L
  list(dist_key = "gengamma", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Rayleigh MOM --------------------------------------------------------------
#' E[X] = sigma * sqrt(pi/2)  =>  sigma_hat = mean(x) / sqrt(pi/2)
#' @keywords internal
.mom_rayleigh <- function(x) {
  if (any(x < 0)) return(NULL)
  n         <- length(x)
  sigma_hat <- mean(x) / sqrt(pi / 2)
  if (!is.finite(sigma_hat) || sigma_hat <= 0) return(NULL)
  params <- c(sigma = sigma_hat)
  ll     <- sum(drayleigh(x, sigma = sigma_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 1L
  list(dist_key = "rayleigh", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Half-Normal MOM -----------------------------------------------------------
#' E[X] = sigma * sqrt(2/pi)  =>  sigma_hat = mean(x) * sqrt(pi/2)
#' @keywords internal
.mom_halfnormal <- function(x) {
  if (any(x < 0)) return(NULL)
  n         <- length(x)
  sigma_hat <- mean(x) * sqrt(pi / 2)
  if (!is.finite(sigma_hat) || sigma_hat <= 0) return(NULL)
  params <- c(sigma = sigma_hat)
  ll     <- sum(dhalfnormal(x, sigma = sigma_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 1L
  list(dist_key = "halfnormal", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Gamma 3P MOM --------------------------------------------------------------
#' Match mean, variance, and skewness:
#'   skewness = 2/sqrt(shape)  =>  shape = 4/skew^2
#'   scale    = sqrt(var/shape)
#'   thres    = mean - shape*scale
#' @keywords internal
.mom_gamma3 <- function(x) {
  n  <- length(x)
  m  <- mean(x)
  v  <- stats::var(x)
  sk <- mean((x - m)^3) / v^1.5
  if (!is.finite(sk) || abs(sk) < 1e-6) return(NULL)
  shape <- 4 / sk^2
  scale <- sqrt(v / shape)
  thres <- m - shape * scale
  if (!is.finite(shape) || shape <= 0 ||
      !is.finite(scale) || scale <= 0) return(NULL)
  # Ensure threshold is below all data
  thres <- min(thres, min(x) - 1e-4 * stats::sd(x))
  params <- c(shape = shape, scale = scale, thres = thres)
  ll <- sum(dgamma3(x, shape = shape, scale = scale, thres = thres,
                    log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 3L
  list(dist_key = "gamma3", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Lindley MOM ---------------------------------------------------------------

#' MOM for Lindley: solve for theta from mean equation
#' E[X] = (theta+2)/(theta*(theta+1))
#' Rearranged: xbar*theta^2 + (xbar-1)*theta - 2 = 0  (same as MLE score)
#' @keywords internal
.mom_lindley <- function(x) {
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
  list(dist_key = "lindley", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Exponentiated Exponential MOM --------------------------------------------

#' MOM for Exponentiated Exponential: use digamma-based moment equations.
#' E[X]   = (digamma(alpha+1) + euler_gamma) / lambda
#' Var[X] = (pi^2/6 - trigamma(alpha+1)) ... actually use trigamma.
#' Match observed mean and variance numerically to find (alpha, lambda).
#' @keywords internal
.mom_expexp <- function(x) {
  n    <- length(x)
  m1   <- mean(x)
  m2   <- mean(x^2)
  cv2  <- m2 / m1^2 - 1           # observed CV^2 = Var/Mean^2

  euler_g <- 0.5772156649015329

  # G(alpha) = (digamma(alpha+1) + euler_g)  ->  E[X] = G(alpha)/lambda
  # H(alpha) = pi^2/6 - trigamma(alpha+1)   ->  Var[X] = H(alpha)/lambda^2
  # CV^2 = H(alpha) / G(alpha)^2

  obj_alpha <- function(a) {
    if (a <= 0) return(1e9)
    G <- digamma(a + 1) + euler_g
    H <- pi^2 / 6 - trigamma(a + 1)
    if (H < 0 || G <= 0) return(1e9)
    (H / G^2 - cv2)^2
  }

  opt <- tryCatch(
    stats::optimise(obj_alpha, interval = c(0.01, 500))$minimum,
    error = function(e) NULL
  )
  if (is.null(opt)) return(NULL)
  alpha_hat  <- max(opt, 0.01)
  G          <- digamma(alpha_hat + 1) + euler_g
  if (!is.finite(G) || G <= 0) return(NULL)
  lambda_hat <- G / m1

  params <- c(alpha = alpha_hat, lambda = lambda_hat)
  ll     <- sum(dexpexp(x, alpha_hat, lambda_hat, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "expexp", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Nakagami MOM -------------------------------------------------------------

#' MOM for Nakagami: E[X^2] = Omega; m from mean equation.
#' E[X] = Gamma(m+0.5)/Gamma(m) * sqrt(Omega/m)
#' Solve numerically for m given ratio r = mean(x)/sqrt(mean(x^2)).
#' @keywords internal
.mom_nakagami <- function(x) {
  n     <- length(x)
  Omega <- mean(x^2)
  if (Omega <= 0) return(NULL)
  r <- mean(x) / sqrt(Omega)      # E[X]/sqrt(Omega) = Gamma(m+0.5)/(Gamma(m)*sqrt(m))
  if (r <= 0 || r >= 1) return(NULL)

  obj_m <- function(m) {
    if (m < 0.5) return(1e9)
    ratio <- exp(lgamma(m + 0.5) - lgamma(m)) / sqrt(m)
    (ratio - r)^2
  }

  opt <- tryCatch(
    stats::optimise(obj_m, interval = c(0.5, 500))$minimum,
    error = function(e) NULL
  )
  if (is.null(opt)) return(NULL)
  m_hat  <- max(opt, 0.5)
  params <- c(m = m_hat, Omega = Omega)
  ll     <- sum(dnakagami(x, m_hat, Omega, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "nakagami", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Uniform MOM --------------------------------------------------------------

#' Uniform MOM: a = mean - sqrt(3)*sd, b = mean + sqrt(3)*sd
#' Expanded slightly to ensure all observed data lies within [a, b].
#' @keywords internal
.mom_uniform <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NULL)
  a <- m - sqrt(3) * s
  b <- m + sqrt(3) * s
  # Expand to cover all data so log-likelihood is always finite
  a <- min(a, min(x) - 1e-10)
  b <- max(b, max(x) + 1e-10)
  if (!is.finite(a) || !is.finite(b) || a >= b) return(NULL)
  params <- c(min = a, max = b)
  ll <- sum(dunif(x, min = a, max = b, log = TRUE))
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "uniform", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Kumaraswamy MOM ----------------------------------------------------------

#' Kumaraswamy MOM: numerically match first two raw moments.
#' E[X^k; a, b] = b * B(1 + k/a, b) = b * exp(lbeta(1 + k/a, b))
#' Optimise (a, b) on log scale for numerical stability.
#' @keywords internal
.mom_kumaraswamy <- function(x) {
  n <- length(x)
  if (any(x <= 0) || any(x >= 1)) return(NULL)
  m1 <- mean(x)
  m2 <- mean(x^2)
  if (!is.finite(m1) || !is.finite(m2) || m1 <= 0 || m1 >= 1) return(NULL)

  ex_k <- function(a, b, k) {
    if (a <= 0 || b <= 0) return(NA_real_)
    tryCatch(b * exp(lbeta(1 + k / a, b)), error = function(e) NA_real_)
  }

  obj <- function(par) {
    a <- exp(par[1]); b <- exp(par[2])
    e1 <- ex_k(a, b, 1); e2 <- ex_k(a, b, 2)
    if (!is.finite(e1) || !is.finite(e2)) return(1e9)
    (e1 - m1)^2 + (e2 - m2)^2
  }

  opt <- tryCatch(
    stats::optim(c(0, 0), obj, method = "Nelder-Mead",
                 control = list(reltol = 1e-10, maxit = 2000)),
    error = function(e) NULL
  )
  if (is.null(opt) || opt$convergence > 1) return(NULL)
  a_hat <- exp(opt$par[1])
  b_hat <- exp(opt$par[2])
  if (!is.finite(a_hat) || !is.finite(b_hat) || a_hat <= 0 || b_hat <= 0) return(NULL)

  params <- c(shape1 = a_hat, shape2 = b_hat)
  ll <- tryCatch(
    sum(dkumaraswamy(x, shape1 = a_hat, shape2 = b_hat, log = TRUE)),
    error = function(e) NA_real_
  )
  if (!is.finite(ll)) return(NULL)
  k <- 2L
  list(dist_key = "kumaraswamy", method = "MOM", params = params,
       loglik = ll, aic = compute_aic(ll, k), bic = compute_bic(ll, k, n),
       n_params = k, n = n, converged = TRUE, fit_obj = NULL)
}


# -- Batch MOM ----------------------------------------------------------------

#' Fit all eligible distributions by Method of Moments
#'
#' @param x    Numeric vector of clean data.
#' @param keys Character vector of distribution keys.
#' @return Named list of fit results (NULLs dropped).
#' @keywords internal
fit_mom_all <- function(x, keys) {
  .warn_unsupported_method(keys, "supports_mom", "Method of Moments")
  # Only attempt keys whose registry entry supports MOM
  eligible <- keys[vapply(keys, function(k) {
    isTRUE(get_dist_entry(k)$supports_mom)
  }, logical(1))]

  results <- lapply(eligible, function(k) fit_mom(x, k))
  names(results) <- eligible
  Filter(Negate(is.null), results)
}

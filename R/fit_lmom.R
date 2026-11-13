# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : fit_lmom.R
# Description: L-Moments (LMOM) estimation for distributions
#              supported by the lmomco package.
#
#              L-moments are linear combinations of order statistics.
#              They are more robust than classical moments for
#              heavy-tailed and skewed distributions (Hosking, 1990).
#
#              Supported distributions and lmomco function mapping:
#                normal       -> parnor   (mu, sigma -> mean, sd)
#                lognormal    -> parln3   (zeta~=0, mu, sigma -> meanlog, sdlog)
#                gamma        -> pargam   (alpha, beta -> shape, scale)
#                exponential  -> parexp   (xi~=0, beta -> rate=1/beta)
#                weibull      -> parwei   (zeta~=0, beta, delta -> shape, scale)
#                weibull3p    -> parwei   (zeta, beta, delta -> thres, scale, shape)
#                logistic     -> parglo   (xi, alpha, kappa~=0 -> location, scale)
#                gumbel       -> pargum   (xi, alpha -> loc, scale)
#                gev          -> pargev   (xi, alpha, kappa -> loc, scale, shape)
#                gpd          -> pargpa   (xi, alpha, kappa -> loc, scale, shape)
#                pearson3     -> parpe3   (mu, sigma, gamma -> shape, scale, loc)
#                lpearson3    -> parpe3 on log(x)
#                glo          -> parglo   (xi, alpha, kappa -> loc, scale, shape)
#
#              Note: Inverse Gaussian (lmomco::parwig) is NOT included because
#              parwig is not a public export of the lmomco namespace.
#
#              IMPORTANT parameter-sign convention:
#              lmomco uses kappa for GEV/GPD shape with OPPOSITE sign to evd.
#              We convert: evd_shape = -lmomco_kappa.
#
#              Return value: same named list as fit_mle.R.
#
# Reference:
#   Hosking, J.R.M. (1990). L-moments: analysis and estimation of
#   distributions using linear combinations of order statistics.
#   Journal of the Royal Statistical Society B, 52(1), 105-124.
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

#' Fit a single distribution by L-Moments
#'
#' @param x        Numeric vector of clean (finite) data.
#' @param dist_key Character key from the FitVerse registry.
#' @return Named list with fit results, or NULL if unsupported/failed.
#' @keywords internal
fit_lmom <- function(x, dist_key) {

  entry <- get_dist_entry(dist_key)

  if (!entry$supports_lmom) {
    message(sprintf(
      "  [FitVerse] L-Moments not available for '%s' -- skipping.",
      entry$display_name))
    return(NULL)
  }

  if (!has_pkg("lmomco", "L-moment estimation")) return(NULL)

  n <- length(x)

  # Compute sample L-moments (unbiased estimators)
  lmom <- tryCatch(
    lmomco::lmoms(x, nmom = 5),
    error = function(e) {
      message(sprintf("  [FitVerse] L-moment computation failed for '%s': %s",
                      entry$display_name, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(lmom)) return(NULL)

  # Fit parameters using the appropriate lmomco function
  result <- switch(dist_key,
    # Symmetric / unbounded
    normal      = .lmom_normal(x, lmom, n),
    logistic    = .lmom_logistic(x, lmom, n),
    # Right-skewed (positive support)
    lognormal   = .lmom_lognormal(x, lmom, n),
    gamma       = .lmom_gamma(x, lmom, n),
    exponential = .lmom_exponential(x, lmom, n),
    weibull     = .lmom_weibull(x, lmom, n),
    weibull3p   = .lmom_weibull3p(x, lmom, n),
    pearson3    = .lmom_pearson3(x, lmom, n),
    lpearson3   = .lmom_lpearson3(x, n),     # re-computes lmoms on log(x)
    # Extreme-value
    gumbel      = .lmom_gumbel(x, lmom, n),
    gev         = .lmom_gev(x, lmom, n),
    gpd         = .lmom_gpd(x, lmom, n),
    glo         = .lmom_glo(x, lmom, n),
    # Extended families
    kappa4      = .lmom_kappa4(x, lmom, n),
    wakeby      = .lmom_wakeby(x, lmom, n),
    {
      message(sprintf("  [FitVerse] No lmomco handler for '%s'.", dist_key))
      NULL
    }
  )

  result
}


# -- Individual L-moment fitters -----------------------------------------------

#' L-Moments: Normal
#' parnor returns: para = c(mu, sigma)  -> R names: mean, sd
#' @keywords internal
.lmom_normal <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parnor(lmom), "Normal", "LMOM")
  if (is.null(par_obj)) return(NULL)
  params <- c(mean = unname(par_obj$para["mu"]),
              sd   = unname(par_obj$para["sigma"]))
  .build_lmom_result(x, "normal", params, n)
}


#' L-Moments: Log-Normal (3-parameter, with location fixed near 0)
#' parln3 returns: para = c(zeta, mu, sigma)
#' We use the 2-parameter form by ignoring zeta (lmomco sets it close to 0).
#' @keywords internal
.lmom_lognormal <- function(x, lmom, n) {
  # lmomco's parln3 needs L-skewness > 0; shift check
  if (lmom$ratios[3] <= 0) {
    message("  [FitVerse] Log-Normal LMOM requires positive L-skewness -- skipping.")
    return(NULL)
  }
  mu <- sigma <- NA_real_
  par_obj <- safe_fit(lmomco::parln3(lmom), "Log-Normal", "LMOM")
  if (!is.null(par_obj)) {
    mu    <- unname(par_obj$para["mu"])
    sigma <- unname(par_obj$para["sigma"])
  }
  # Fallback: if parln3 gives non-finite params, use log-space sample moments
  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) {
    lx    <- log(x[x > 0])
    mu    <- mean(lx)
    sigma <- stats::sd(lx)
  }
  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) return(NULL)
  params <- c(meanlog = mu, sdlog = sigma)
  .build_lmom_result(x, "lognormal", params, n)
}


#' L-Moments: Gamma
#' pargam returns: para = c(alpha, beta) where beta is the SCALE (not rate)
#' R's dgamma uses shape and rate; we convert: rate = 1/beta
#' @keywords internal
.lmom_gamma <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::pargam(lmom), "Gamma", "LMOM")
  if (is.null(par_obj)) return(NULL)
  alpha <- unname(par_obj$para["alpha"])
  beta  <- unname(par_obj$para["beta"])   # beta is scale in lmomco
  params <- c(shape = alpha, rate = 1 / beta)
  .build_lmom_result(x, "gamma", params, n)
}


#' L-Moments: Exponential
#' parexp returns: para = c(xi, alpha) where xi is location, alpha is scale
#' R's dexp uses rate = 1/alpha (valid when xi ~= 0)
#' @keywords internal
.lmom_exponential <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parexp(lmom), "Exponential", "LMOM")
  if (is.null(par_obj)) return(NULL)
  alpha <- unname(par_obj$para["alpha"])   # lmomco uses "alpha" for scale (not "beta")
  if (!is.finite(alpha) || alpha <= 0) return(NULL)
  params <- c(rate = 1 / alpha)
  .build_lmom_result(x, "exponential", params, n)
}


#' L-Moments: Weibull
#' parwei returns: para = c(zeta, beta, delta)
#' where zeta = location (~=0), beta = scale, delta = shape
#' R's dweibull: shape = delta, scale = beta
#' @keywords internal
.lmom_weibull <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parwei(lmom), "Weibull", "LMOM")
  if (is.null(par_obj)) return(NULL)
  delta  <- unname(par_obj$para["delta"])
  beta   <- unname(par_obj$para["beta"])
  if (!is.finite(delta) || !is.finite(beta) || delta <= 0 || beta <= 0)
    return(NULL)
  params <- c(shape = delta, scale = beta)
  .build_lmom_result(x, "weibull", params, n)
}


#' L-Moments: 3-Parameter Weibull
#' parwei returns: para = c(zeta, beta, delta)
#' where zeta = threshold, beta = scale, delta = shape
#' We pass all three through, unlike the 2P handler which drops zeta.
#' @keywords internal
.lmom_weibull3p <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parwei(lmom), "Weibull (3-Parameter)", "LMOM")
  if (is.null(par_obj)) return(NULL)
  delta <- unname(par_obj$para["delta"])
  beta  <- unname(par_obj$para["beta"])
  zeta  <- unname(par_obj$para["zeta"])
  if (!is.finite(delta) || !is.finite(beta) || !is.finite(zeta) ||
      delta <= 0 || beta <= 0)
    return(NULL)
  params <- c(shape = delta, scale = beta, thres = zeta)
  .build_lmom_result(x, "weibull3p", params, n)
}


#' L-Moments: Logistic (via Generalised Logistic with kappa fixed to 0)
#' parglo returns: para = c(xi, alpha, kappa)
#' Standard logistic: kappa = 0, location = xi, scale = alpha
#' @keywords internal
.lmom_logistic <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parglo(lmom), "Logistic", "LMOM")
  if (is.null(par_obj)) return(NULL)
  params <- c(location = unname(par_obj$para["xi"]),
              scale    = unname(par_obj$para["alpha"]))
  .build_lmom_result(x, "logistic", params, n)
}


#' L-Moments: Gumbel
#' pargum returns: para = c(xi, alpha)  -> loc = xi, scale = alpha
#' @keywords internal
.lmom_gumbel <- function(x, lmom, n) {
  if (!has_pkg("evd", "Gumbel distribution")) return(NULL)
  par_obj <- safe_fit(lmomco::pargum(lmom), "Gumbel", "LMOM")
  if (is.null(par_obj)) return(NULL)
  params <- c(loc   = unname(par_obj$para["xi"]),
              scale = unname(par_obj$para["alpha"]))
  .build_lmom_result(x, "gumbel", params, n)
}


#' L-Moments: Generalised Extreme Value (GEV)
#' pargev returns: para = c(xi, alpha, kappa)
#' IMPORTANT: lmomco kappa has OPPOSITE sign to evd shape.
#'   evd shape = -lmomco_kappa
#' @keywords internal
.lmom_gev <- function(x, lmom, n) {
  if (!has_pkg("evd", "GEV distribution")) return(NULL)
  par_obj <- safe_fit(lmomco::pargev(lmom), "GEV", "LMOM")
  if (is.null(par_obj)) return(NULL)
  # Convert sign convention
  params <- c(loc   = unname(par_obj$para["xi"]),
              scale = unname(par_obj$para["alpha"]),
              shape = -unname(par_obj$para["kappa"]))  # sign flip!
  .build_lmom_result(x, "gev", params, n)
}


#' L-Moments: Generalised Pareto Distribution (GPD)
#' pargpa returns: para = c(xi, alpha, kappa)
#' IMPORTANT: lmomco kappa has OPPOSITE sign to our shape (same convention as GEV).
#'   our shape = -lmomco_kappa
#' @keywords internal
.lmom_gpd <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::pargpa(lmom), "GPD", "LMOM")
  if (is.null(par_obj)) return(NULL)
  params <- c(loc   = unname(par_obj$para["xi"]),
              scale = unname(par_obj$para["alpha"]),
              shape = -unname(par_obj$para["kappa"]))   # sign flip!
  if (params["scale"] <= 0) return(NULL)
  .build_lmom_result(x, "gpd", params, n)
}


#' L-Moments: Pearson Type III
#' parpe3 returns: para = c(mu, sigma, gamma) - mean, std dev, skewness
#' Convert to our (shape, scale, loc) parameterisation.
#' @keywords internal
.lmom_pearson3 <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parpe3(lmom), "Pearson III", "LMOM")
  if (is.null(par_obj)) return(NULL)
  mu  <- unname(par_obj$para["mu"])
  sig <- unname(par_obj$para["sigma"])
  gam <- unname(par_obj$para["gamma"])
  if (abs(gam) < 1e-8 || sig <= 0) {
    message("  [FitVerse] Pearson III LMOM: near-zero skewness or sd -- skipping.")
    return(NULL)
  }
  shp <- 4 / gam^2
  sc  <- sig * gam / 2
  loc <- mu - 2 * sig / gam
  params <- c(shape = shp, scale = sc, loc = loc)
  .build_lmom_result(x, "pearson3", params, n)
}


#' L-Moments: Log-Pearson Type III
#' Apply Pearson III L-moment fitting to log(x), then convert.
#' @keywords internal
.lmom_lpearson3 <- function(x, n) {
  if (any(x <= 0)) {
    message("  [FitVerse] Log-Pearson III LMOM requires strictly positive data -- skipping.")
    return(NULL)
  }
  lx   <- log(x)
  lmom_lx <- tryCatch(lmomco::lmoms(lx, nmom = 5), error = function(e) NULL)
  if (is.null(lmom_lx)) return(NULL)
  # If L-skewness of log(x) is near zero the PE3 fit degenerates --
  # parameters become enormous and the log-likelihood is non-finite.
  # LP3 is only meaningful for skewed log-data; bail out early.
  if (abs(lmom_lx$ratios[3]) < 0.05) {
    message("  [FitVerse] Log-Pearson III LMOM: near-zero L-skewness on log scale -- skipping.")
    return(NULL)
  }
  par_obj <- safe_fit(lmomco::parpe3(lmom_lx), "Log-Pearson III", "LMOM")
  if (is.null(par_obj)) return(NULL)
  mu  <- unname(par_obj$para["mu"])
  sig <- unname(par_obj$para["sigma"])
  gam <- unname(par_obj$para["gamma"])
  if (abs(gam) < 0.05 || sig <= 0) return(NULL)
  shp <- 4 / gam^2
  sc  <- sig * gam / 2
  loc <- mu - 2 * sig / gam
  params <- c(shape = shp, scale = sc, loc = loc)
  .build_lmom_result(x, "lpearson3", params, n)
}


#' L-Moments: Generalised Logistic (GLO) - full shape parameter
#' parglo returns: para = c(xi, alpha, kappa)
#' No sign flip for GLO: lmomco kappa = our shape.
#' (The existing .lmom_logistic drops kappa to get standard logistic;
#'  .lmom_glo keeps it for the full GLO.)
#' @keywords internal
.lmom_glo <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parglo(lmom), "Generalised Logistic", "LMOM")
  if (is.null(par_obj)) return(NULL)
  params <- c(loc   = unname(par_obj$para["xi"]),
              scale = unname(par_obj$para["alpha"]),
              shape = unname(par_obj$para["kappa"]))
  if (params["scale"] <= 0) return(NULL)
  .build_lmom_result(x, "glo", params, n)
}


# -- Inverse Gaussian L-Moments -----------------------------------------------

#' L-Moments: Inverse Gaussian (Wald)
#'
#' lmomco::parwig fits the Wald distribution.
#' Returns: para = c(mu, lambda)
#' @keywords internal
.lmom_invgaussian <- function(x, lmom, n) {
  if (any(x <= 0)) {
    message("  [FitVerse] Inverse Gaussian LMOM requires x > 0 -- skipping.")
    return(NULL)
  }
  par_fn <- tryCatch(get("parwig", envir = asNamespace("lmomco")), error = function(e) NULL)
  if (is.null(par_fn)) {
    message("  [FitVerse] Inverse Gaussian LMOM: lmomco::parwig not available - skipping.")
    return(NULL)
  }
  par_obj <- safe_fit(par_fn(lmom), "Inverse Gaussian", "LMOM")
  if (is.null(par_obj)) return(NULL)
  mu_val  <- unname(par_obj$para["mu"])
  lam_val <- unname(par_obj$para["lambda"])
  if (!is.finite(mu_val) || !is.finite(lam_val) ||
      mu_val <= 0 || lam_val <= 0) return(NULL)
  params <- c(mu = mu_val, lambda = lam_val)
  .build_lmom_result(x, "invgaussian", params, n)
}


# -- Shared result builder -----------------------------------------------------

#' Build a standardised L-moment fit result list
#'
#' Computes the log-likelihood of the estimated parameters on the data,
#' then packages everything into the standard FitVerse result format.
#'
#' @keywords internal
.build_lmom_result <- function(x, dist_key, params, n) {

  # Sanity-check: all parameters must be finite
  if (any(!is.finite(params))) {
    message(sprintf("  [FitVerse] L-moment fit for '%s' produced non-finite parameters -- skipping.",
                    dist_key))
    return(NULL)
  }

  ll <- .loglik_from_params(x, dist_key, params)
  k  <- length(params)

  list(
    dist_key  = dist_key,
    method    = "LMOM",
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


# -- Kappa-4 L-Moments --------------------------------------------------------

#' L-Moments: Kappa-4 (Hosking 1994)
#' parkap returns: para = c(xi, alpha, kappa, h)
#' These map directly to our registry params: xi, alpha, kappa, h.
#' Requires nmom >= 4 (already computed above with nmom=5).
#' @keywords internal
.lmom_kappa4 <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parkap(lmom), "Kappa-4", "LMOM")
  if (is.null(par_obj)) return(NULL)
  xi    <- unname(par_obj$para["xi"])
  alpha <- unname(par_obj$para["alpha"])
  kappa <- unname(par_obj$para["kappa"])
  h     <- unname(par_obj$para["h"])
  if (!all(is.finite(c(xi, alpha, kappa, h))) || alpha <= 0)
    return(NULL)

  # Guard: kappa4 can have bounded support. If the L-moment estimate places the
  # support boundary inside the data range, pdfkap returns 0 for boundary obs
  # and loglik collapses to -Inf.  Expand alpha incrementally until both data
  # extremes have positive density.
  extremes  <- c(min(x), max(x))
  test_para <- lmomco::vec2par(c(xi, alpha, kappa, h), type = "kap")
  for (iter in seq_len(20L)) {
    d_ext <- tryCatch(lmomco::pdfkap(extremes, test_para),
                      error = function(e) c(0, 0))
    if (all(is.finite(d_ext) & d_ext > 0)) break
    alpha     <- alpha * 1.05
    test_para <- lmomco::vec2par(c(xi, alpha, kappa, h), type = "kap")
  }

  params <- c(xi = xi, alpha = alpha, kappa = kappa, h = h)
  .build_lmom_result(x, "kappa4", params, n)
}


# -- Wakeby L-Moments ---------------------------------------------------------

#' L-Moments: Wakeby (five-parameter quantile distribution)
#' parwak returns: para = c(xi, alpha, beta, gamma, delta)
#' These map directly to our registry params: xi, alpha, beta, gamma, delta.
#' Requires nmom >= 5 (already computed above with nmom=5).
#' @keywords internal
.lmom_wakeby <- function(x, lmom, n) {
  par_obj <- safe_fit(lmomco::parwak(lmom), "Wakeby", "LMOM")
  if (is.null(par_obj)) return(NULL)
  xi    <- unname(par_obj$para["xi"])
  alpha <- unname(par_obj$para["alpha"])
  beta  <- unname(par_obj$para["beta"])
  gamma <- unname(par_obj$para["gamma"])
  delta <- unname(par_obj$para["delta"])
  if (!all(is.finite(c(xi, alpha, beta, gamma, delta)))) return(NULL)
  if (alpha < 0 || gamma < 0) return(NULL)
  params <- c(xi = xi, alpha = alpha, beta = beta, gamma = gamma, delta = delta)
  .build_lmom_result(x, "wakeby", params, n)
}


# -- Batch LMOM ---------------------------------------------------------------

#' Fit all eligible distributions by L-Moments
#'
#' @param x    Numeric vector of clean data.
#' @param keys Character vector of distribution keys.
#' @return Named list of fit results (NULLs dropped).
#' @keywords internal
fit_lmom_all <- function(x, keys) {
  .warn_unsupported_method(keys, "supports_lmom", "L-Moments")

  eligible <- keys[vapply(keys, function(k) {
    isTRUE(get_dist_entry(k)$supports_lmom)
  }, logical(1))]

  results <- lapply(eligible, function(k) {
    tryCatch(fit_lmom(x, k), error = function(e) {
      message(sprintf("  [FitVerse] LMOM fit for '%s' failed: %s", k, conditionMessage(e)))
      NULL
    })
  })
  names(results) <- eligible
  Filter(Negate(is.null), results)
}


# -- Method-compatibility warnings ---------------------------------------------

#' Warn about distributions excluded because they explicitly block a fitting method.
#'
#' Uses \code{isFALSE()} so it only fires for registry entries where the
#' support flag is \emph{explicitly} \code{FALSE} -- distributions that simply
#' lack an LMOM fitter are silently skipped, as usual.  Up to three excluded
#' distributions get individual messages; more than three get a single summary.
#'
#' @param keys          Character vector of distribution keys to check.
#' @param support_field Registry field name (e.g. \code{"supports_mle"}).
#' @param method_label  Human-readable method name for the message.
#' @keywords internal
.warn_unsupported_method <- function(keys, support_field, method_label) {
  excluded <- Filter(function(k) {
    e <- get_dist_entry(k)
    !is.null(e) && isFALSE(e[[support_field]])
  }, keys)

  if (length(excluded) == 0L) return(invisible(NULL))

  if (length(excluded) <= 3L) {
    for (k in excluded) {
      e    <- get_dist_entry(k)
      alts <- c(
        if (isTRUE(e$supports_mle))  "MLE",
        if (isTRUE(e$supports_lmom)) "'L-Moments'",
        if (isTRUE(e$supports_mom))  "'Method of Moments'"
      )
      if (length(alts) == 0L) next
      message(sprintf(
        "  [FitVerse] '%s' does not support %s -- use %s (or 'All Methods') to include it.",
        e$display_name, method_label, paste(alts, collapse = " or ")
      ))
    }
  } else {
    message(sprintf(
      "  [FitVerse] %d distribution(s) explicitly do not support %s and were excluded. Use 'All Methods' to fit all distributions.",
      length(excluded), method_label
    ))
  }

  invisible(excluded)
}

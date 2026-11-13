# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : gof_tests.R
# Description: Goodness-of-Fit (GoF) tests for fitted distributions.
#
#              Four tests are computed for every fitted model:
#
#              1. Kolmogorov-Smirnov (KS) test
#                 stats::ks.test(x, pF, ...)
#                 Tests whether the empirical CDF deviates significantly
#                 from the fitted theoretical CDF.
#                 Statistic D: maximum absolute CDF difference.
#
#              2. Anderson-Darling (AD) test
#                 goftest::ad.test(x, pF, ...)
#                 Gives more weight to the tails than the KS test.
#                 Recommended for detecting tail departures.
#
#              3. Cramer-von Mises (CvM) test
#                 goftest::cvm.test(x, pF, ...)
#                 Integrates the squared CDF difference; less sensitive
#                 to tail behaviour than AD but more powerful than KS
#                 in the body of the distribution.
#
#              4. Chi-Squared (CS) test
#                 Pearson's chi-squared test on k equiprobable bins.
#                 Degrees of freedom adjusted for estimated parameters:
#                 df = k - 1 - n_params.
#                 Handles grouped / binned data naturally.
#                 Recommended for discrete-like or heavily grouped data.
#
#              Each test returns its statistic and p-value.
#              Large p-value (> 0.05) -> no significant departure from
#              the fitted distribution.
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


# -- Main GoF function ---------------------------------------------------------

#' Compute four goodness-of-fit tests for a fitted distribution
#'
#' @param x         Numeric vector of clean data.
#' @param dist_key  Distribution key from the FitVerse registry.
#' @param params    Named numeric vector of fitted parameter values.
#' @return A data frame with columns:
#'   Test, Statistic, P_value, Stars
#'   (or NA rows if a test cannot be computed).
#' @keywords internal
compute_gof <- function(x, dist_key, params) {

  entry <- get_dist_entry(dist_key)

  # Use the unified resolver so that internal overrides (LP3, P3, GPD, GEV,
  # Gumbel, GLO, Rayleigh, HalfNormal, ExpWeibull, Gamma3P, LNorm3P, etc.)
  # are found before falling back to match.fun / getExportedValue.
  p_fun <- .lookup_dpqr("p", dist_key)

  if (is.null(p_fun)) {
    return(.gof_na_frame())
  }

  # Wrap the CDF so we can call it as a single-argument function
  # (ks.test and goftest require this form)
  pcdf <- function(q) {
    args <- c(list(q), as.list(params))
    do.call(p_fun, args)
  }

  # Look up quantile function for fast equiprobable-bin breaks in chi-squared.
  # Using q<dist>() directly (~1 vectorised call) is 50-100x faster than
  # uniroot-based CDF inversion (~50-200 CDF evaluations per breakpoint).
  q_fun <- tryCatch(.lookup_dpqr("q", dist_key), error = function(e) NULL)

  n_params <- length(params)

  ks_row  <- .run_ks(x, pcdf)
  ad_row  <- .run_ad(x, pcdf)
  cvm_row <- .run_cvm(x, pcdf)
  cs_row  <- .run_chisq(x, pcdf, n_params, q_fun = q_fun, params = params)

  rbind(ks_row, ad_row, cvm_row, cs_row)
}


# -- Individual test runners ---------------------------------------------------

#' Kolmogorov-Smirnov test (base R)
#' @keywords internal
.run_ks <- function(x, pcdf) {
  res <- tryCatch(
    stats::ks.test(x, pcdf),
    error = function(e) {
      message("[FitVerse KS error] ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res)) return(.gof_row("KS", NA, NA))
  .gof_row("Kolmogorov-Smirnov", res$statistic, res$p.value)
}


#' Anderson-Darling test (goftest package)
#' @keywords internal
.run_ad <- function(x, pcdf) {
  if (!requireNamespace("goftest", quietly = TRUE))
    return(.gof_row("Anderson-Darling", NA, NA))
  # goftest::ad.test requires CDF values strictly in [0,1].  Some internal CDF
  # wrappers can return values marginally outside this range (e.g. on pseudo-
  # observations from grouped data).  Clamp defensively before passing in.
  pcdf_safe <- function(q) pmin(pmax(pcdf(q), 0), 1)
  res <- tryCatch(
    goftest::ad.test(x, null = pcdf_safe),
    error = function(e) {
      message("[FitVerse AD error] ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res)) return(.gof_row("Anderson-Darling", NA, NA))
  .gof_row("Anderson-Darling", res$statistic, res$p.value)
}


#' Cramer-von Mises test (goftest package)
#' @keywords internal
.run_cvm <- function(x, pcdf) {
  if (!requireNamespace("goftest", quietly = TRUE))
    return(.gof_row("Cramer-von Mises", NA, NA))
  pcdf_safe <- function(q) pmin(pmax(pcdf(q), 0), 1)
  res <- tryCatch(
    goftest::cvm.test(x, null = pcdf_safe),
    error = function(e) NULL
  )
  if (is.null(res)) return(.gof_row("Cramer-von Mises", NA, NA))
  .gof_row("Cramer-von Mises", res$statistic, res$p.value)
}


#' Chi-squared goodness-of-fit test (Pearson, equiprobable bins)
#'
#' Uses k equiprobable bins defined by the theoretical quantile function
#' (where available) or equal-width bins as a fallback.  Degrees of freedom
#' are adjusted for the number of estimated parameters:
#'   df = k - 1 - n_params  (minimum 1).
#' Bins with expected count < 1 are merged with their neighbours before
#' the test statistic is computed.
#'
#' @param x        Numeric vector of observed data.
#' @param pcdf     Single-argument CDF function: pcdf(q) -> P(X <= q).
#' @param n_params Integer: number of estimated parameters (for df correction).
#' @keywords internal
.run_chisq <- function(x, pcdf, n_params = 0L, q_fun = NULL, params = NULL) {
  n <- length(x)
  if (n < 5L) return(.gof_row("Chi-Squared", NA, NA))

  # Choose number of bins: Rice rule, bounded [5, 30]
  # Cap at 30 (not 50) to keep chi-squared fast while retaining power.
  k <- max(5L, min(30L, ceiling(2 * n^(1/3))))

  # Define equiprobable breakpoints.
  # Interior probs avoid the exact 0/1 boundary; the outermost breaks are
  # set to -Inf / +Inf after inversion so all data is captured.
  probs        <- seq(0, 1, length.out = k + 1L)
  probs[1L]    <- 1e-8
  probs[k + 1L] <- 1 - 1e-8

  # Strategy 1 (fast): use the quantile function q<dist>() directly.
  # One vectorised call replaces k separate uniroot inversions, which each
  # require 50-200 CDF evaluations -- a 50-100x speed gain.
  breaks <- NULL
  if (!is.null(q_fun) && !is.null(params)) {
    breaks <- tryCatch(
      do.call(q_fun, c(list(p = probs), as.list(params))),
      error = function(e) NULL
    )
    if (!is.null(breaks) && (anyNA(breaks) || is.unsorted(breaks)))
      breaks <- NULL   # fall through to strategy 2
  }

  # Strategy 2 (fallback): numerical CDF inversion via uniroot.
  if (is.null(breaks)) {
    x_range <- range(x, na.rm = TRUE)
    pad     <- diff(x_range) * 0.5
    lo_bnd  <- x_range[1L] - pad
    hi_bnd  <- x_range[2L] + pad

    breaks <- tryCatch({
      vapply(probs, function(p) {
        tryCatch(
          stats::uniroot(
            function(q) pcdf(q) - p,
            interval  = c(lo_bnd, hi_bnd),
            extendInt = "yes",
            tol       = .Machine$double.eps^0.5
          )$root,
          error = function(e) NA_real_
        )
      }, numeric(1L))
    }, error = function(e) rep(NA_real_, k + 1L))
  }

  # Strategy 3 (last resort): equal-width bins -- no CDF inversion needed.
  if (anyNA(breaks) || is.unsorted(breaks)) {
    x_range <- range(x, na.rm = TRUE)
    breaks  <- seq(x_range[1L] - 1e-10, x_range[2L] + 1e-10,
                   length.out = k + 1L)
  }

  breaks[1L]      <- -Inf
  breaks[k + 1L]  <-  Inf

  # Observed and expected counts per bin.
  # Each element of `expected` is n * P(X in bin_i) = n * (F(upper_i) - F(lower_i)).
  # NOTE: do NOT wrap the sapply in diff() -- the sapply already produces k bin
  # probabilities directly; diff() would compute differences *between* consecutive
  # bin probs (all ~0 for equiprobable bins), making the statistic enormous.
  observed <- as.integer(table(cut(x, breaks = breaks, include.lowest = TRUE)))
  expected <- n * vapply(seq_len(k), function(i)
    pcdf(breaks[i + 1L]) - pcdf(breaks[i]), numeric(1L))
  expected <- pmax(expected, 0)  # guard against tiny negatives from rounding

  # Merge bins with expected count < 1 into neighbours
  # (left-to-right pass)
  while (any(expected < 1) && length(expected) > 2L) {
    bad <- which(expected < 1)[1L]
    if (bad == length(expected)) {
      # Merge with left neighbour
      observed[bad - 1L] <- observed[bad - 1L] + observed[bad]
      expected[bad - 1L] <- expected[bad - 1L] + expected[bad]
    } else {
      # Merge with right neighbour
      observed[bad + 1L] <- observed[bad + 1L] + observed[bad]
      expected[bad + 1L] <- expected[bad + 1L] + expected[bad]
    }
    observed <- observed[-bad]
    expected <- expected[-bad]
  }

  k_eff <- length(observed)
  if (k_eff < 2L) return(.gof_row("Chi-Squared", NA, NA))

  df <- max(1L, k_eff - 1L - as.integer(n_params))

  stat <- sum((observed - expected)^2 / pmax(expected, 1e-10))
  pval <- stats::pchisq(stat, df = df, lower.tail = FALSE)

  .gof_row("Chi-Squared", stat, pval)
}


# -- Helpers -------------------------------------------------------------------

#' Build a single GoF result row
#' @keywords internal
.gof_row <- function(test_name, statistic, p_value) {
  data.frame(
    Test      = test_name,
    Statistic = as.numeric(statistic),
    P_value   = as.numeric(p_value),
    Stars     = pval_stars(p_value),
    stringsAsFactors = FALSE
  )
}


#' Return an all-NA GoF data frame (four rows, one per test)
#' @keywords internal
.gof_na_frame <- function() {
  rbind(
    .gof_row("Kolmogorov-Smirnov", NA, NA),
    .gof_row("Anderson-Darling",   NA, NA),
    .gof_row("Cramer-von Mises",   NA, NA),
    .gof_row("Chi-Squared",        NA, NA)
  )
}


# -- Attach GoF results to a fit-result list -----------------------------------

#' Run GoF tests for a single FitVerse fit-result list and attach results
#'
#' @param fit_result A fit-result list (from fit_mle / fit_mom / fit_lmom).
#' @param x          Numeric data vector.
#' @return The same list with an added \code{gof} element.
#' @keywords internal
attach_gof <- function(fit_result, x) {
  if (is.null(fit_result)) return(NULL)
  # 10-second elapsed time limit guards against CDF functions that hang
  # (e.g. GHyp/NIG near parameter-space boundaries with infinite integration).
  fit_result$gof <- tryCatch({
    setTimeLimit(cpu = Inf, elapsed = 10, transient = TRUE)
    # Suppress the benign "Inf replaced by maximum positive value" warning that
    # some external quantile functions (e.g. actuar's q<dist> via numerical CDF
    # inversion) emit when optimize() encounters Inf at interval boundaries.
    # The substitution of .Machine$double.xmax is the correct behaviour; the
    # warning is noise that floods the console during 52-distribution runs.
    withCallingHandlers(
      compute_gof(x, fit_result$dist_key, fit_result$params),
      warning = function(w) {
        if (grepl("Inf replaced by maximum positive value",
                  conditionMessage(w), fixed = TRUE))
          invokeRestart("muffleWarning")
      }
    )
  }, error = function(e) {
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    .gof_na_frame()
  })
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  fit_result
}


#' Extract the KS p-value from a fit result (for ranking purposes)
#' @keywords internal
get_ks_pvalue <- function(fit_result) {
  if (is.null(fit_result$gof)) return(NA_real_)
  ks_row <- fit_result$gof[fit_result$gof$Test == "Kolmogorov-Smirnov", , drop = FALSE]
  if (nrow(ks_row) == 0) return(NA_real_)
  ks_row$P_value[1]
}


#' Extract the AD p-value from a fit result
#' @keywords internal
get_ad_pvalue <- function(fit_result) {
  if (is.null(fit_result$gof)) return(NA_real_)
  ad_row <- fit_result$gof[fit_result$gof$Test == "Anderson-Darling", , drop = FALSE]
  if (nrow(ad_row) == 0) return(NA_real_)
  ad_row$P_value[1]
}


#' Extract the Chi-Squared p-value from a fit result
#' @keywords internal
get_chisq_pvalue <- function(fit_result) {
  if (is.null(fit_result$gof)) return(NA_real_)
  cs_row <- fit_result$gof[fit_result$gof$Test == "Chi-Squared", , drop = FALSE]
  if (nrow(cs_row) == 0) return(NA_real_)
  cs_row$P_value[1]
}

# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : bootstrap_ci.R
# Description: Non-parametric bootstrap confidence intervals for
#              parameter estimates and return levels.
#
#              bootstrap_ci() resamples the data with replacement
#              B times, re-fits the best-fit distribution each time
#              using the same method (MLE / MOM / LMOM), and
#              computes percentile-bootstrap CIs.
#
#              Optionally supplies progress updates via a callback
#              function (useful for Shiny progress bars).
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


#' Bootstrap confidence intervals for a fitted distribution
#'
#' Resamples the cleaned data \code{B} times with replacement, re-fits the
#' same distribution and estimation method as the best-fit model, and returns
#' percentile-bootstrap confidence intervals for the parameter estimates and
#' (optionally) user-specified return levels.
#'
#' @param fit            A \code{fitverse} object returned by
#'                       \code{\link{fitverse}()}.
#' @param B              Integer: number of bootstrap replicates.
#'                       Default \code{999}.
#' @param conf           Numeric: confidence level, e.g. \code{0.95}.
#'                       Default \code{0.95}.
#' @param return_periods Numeric vector of return periods for which to
#'                       compute bootstrap CIs.  \code{NULL} (default)
#'                       skips return-level CIs.
#' @param progress       Optional function \code{function(p)} called with a
#'                       numeric progress value in \code{[0, 1]}.  Useful for
#'                       Shiny progress bars.  Default \code{NULL}.
#' @param seed           Integer: random seed for reproducibility.
#'                       \code{NULL} (default) does not set a seed.
#' @param verbose        Logical: print progress messages? Default \code{TRUE}.
#'
#' @return A list of class \code{"fitverse_bootstrap"} with:
#'   \describe{
#'     \item{\code{params_ci}}{Data frame: one row per parameter with columns
#'       \code{param}, \code{estimate}, \code{lower}, \code{upper}.}
#'     \item{\code{return_level_ci}}{Data frame with return-level CIs, or
#'       \code{NULL} if \code{return_periods} was \code{NULL}.}
#'     \item{\code{boot_params}}{Matrix of bootstrap parameter estimates
#'       (\code{B} rows x number of parameters).}
#'     \item{\code{B}}{Number of successful replicates.}
#'     \item{\code{conf}}{Confidence level used.}
#'     \item{\code{dist_key}}{Distribution key of the best-fit model.}
#'     \item{\code{method}}{Estimation method of the best-fit model.}
#'   }
#'
#' @export
#' @examples
#' set.seed(42)
#' x  <- rgamma(200, shape = 2, rate = 0.5)
#' fv <- fitverse(x, dists = "gamma", method = "MLE",
#'                plot = FALSE, verbose = FALSE)
#' ci <- bootstrap_ci(fv, B = 499, verbose = FALSE)
#' print(ci)
bootstrap_ci <- function(fit,
                         B              = 999L,
                         conf           = 0.95,
                         return_periods = NULL,
                         progress       = NULL,
                         seed           = NULL,
                         verbose        = TRUE) {

  stopifnot(inherits(fit, "fitverse"))
  B    <- as.integer(B)
  if (B < 10L) stop("B must be at least 10.", call. = FALSE)
  if (conf <= 0 || conf >= 1) stop("conf must be in (0, 1).", call. = FALSE)
  if (!is.null(progress) && !is.function(progress))
    stop("'progress' must be a function or NULL.", call. = FALSE)

  if (!is.null(seed)) set.seed(seed)

  best    <- fit$best_fit
  dist_key <- best$dist_key
  method   <- best$method
  x        <- fit$data
  n        <- length(x)
  alpha    <- 1 - conf

  if (verbose)
    message(sprintf(
      "[FitVerse] Bootstrap CI: %d replicates, %s (%s), n = %d",
      B, get_dist_entry(dist_key)$display_name, method, n))

  .prog <- function(p) if (is.function(progress)) progress(p)
  .prog(0)

  # -- Bootstrap loop --------------------------------------------------------
  fit_fn <- switch(method,
    MLE  = function(xs) fit_mle(xs,  dist_key),
    MOM  = function(xs) fit_mom(xs,  dist_key),
    LMOM = function(xs) fit_lmom(xs, dist_key),
    stop(sprintf("Unknown method '%s'.", method), call. = FALSE)
  )

  boot_params <- vector("list", B)
  n_ok        <- 0L

  for (b in seq_len(B)) {
    xs  <- x[sample.int(n, n, replace = TRUE)]
    f_b <- tryCatch(fit_fn(xs), error = function(e) NULL,
                               warning = function(w) suppressWarnings(fit_fn(xs)))
    if (!is.null(f_b) && isTRUE(f_b$converged)) {
      n_ok             <- n_ok + 1L
      boot_params[[b]] <- f_b$params
    }
    if (b %% max(1L, B %/% 20L) == 0L) .prog(b / B * 0.95)
  }

  # Keep only successful replicates
  boot_params <- Filter(Negate(is.null), boot_params)
  n_ok        <- length(boot_params)

  if (n_ok < 10L)
    stop(sprintf("Too few successful bootstrap replicates (%d / %d).
                  Check that the distribution can be fitted to resampled data.",
                 n_ok, B), call. = FALSE)

  if (n_ok < B && verbose)
    message(sprintf("  [Warning] %d / %d replicates converged.",
                    n_ok, B))

  # -- Assemble parameter matrix ---------------------------------------------
  param_names <- names(best$params)
  mat <- do.call(rbind, lapply(boot_params, function(p) {
    out         <- setNames(rep(NA_real_, length(param_names)), param_names)
    out[names(p)] <- p
    out
  }))

  # -- Percentile CI for parameters ------------------------------------------
  lo <- alpha / 2
  hi <- 1 - alpha / 2

  params_ci <- data.frame(
    param    = param_names,
    estimate = best$params,
    lower    = apply(mat, 2, function(v) stats::quantile(v, lo, na.rm = TRUE)),
    upper    = apply(mat, 2, function(v) stats::quantile(v, hi, na.rm = TRUE)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # -- Return-level CIs ------------------------------------------------------
  rl_ci <- NULL
  if (!is.null(return_periods) && length(return_periods) > 0) {

    entry    <- get_dist_entry(dist_key)
    q_name   <- paste0("q", entry$r_name)
    q_fun    <- tryCatch(match.fun(q_name), error = function(e) NULL)
    if (is.null(q_fun) && !is.null(entry$package) &&
        requireNamespace(entry$package, quietly = TRUE))
      q_fun <- tryCatch(getExportedValue(entry$package, q_name),
                        error = function(e) NULL)

    if (!is.null(q_fun)) {
      probs      <- 1 - 1 / return_periods
      boot_rl    <- lapply(boot_params, function(p) {
        tryCatch(do.call(q_fun, c(list(p = probs), as.list(p))),
                 error = function(e) rep(NA_real_, length(probs)))
      })
      rl_mat <- do.call(rbind, boot_rl)

      rl_ci <- data.frame(
        T               = return_periods,
        return_level    = do.call(q_fun, c(list(p = probs),
                                           as.list(best$params))),
        lower           = apply(rl_mat, 2,
                                function(v) stats::quantile(v, lo, na.rm = TRUE)),
        upper           = apply(rl_mat, 2,
                                function(v) stats::quantile(v, hi, na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
    }
  }

  .prog(1)

  structure(
    list(
      params_ci      = params_ci,
      return_level_ci = rl_ci,
      boot_params    = mat,
      B              = n_ok,
      conf           = conf,
      dist_key       = dist_key,
      method         = method
    ),
    class = "fitverse_bootstrap"
  )
}


# -- S3 print method -----------------------------------------------------------

#' @export
#' @method print fitverse_bootstrap
#' @noRd
print.fitverse_bootstrap <- function(x, ...) {

  entry <- get_dist_entry(x$dist_key)
  ci_pct <- sprintf("%.0f%%", x$conf * 100)

  cat("\n")
  cat("-----------------------------------------------------\n")
  cat(sprintf("  Bootstrap CIs  [%s conf.]  B = %d\n", ci_pct, x$B))
  cat(sprintf("  Distribution : %s  (%s)\n",
              entry$display_name, x$method))
  cat("-----------------------------------------------------\n")
  cat("  Parameters:\n\n")
  print(x$params_ci, row.names = FALSE, digits = 5)

  if (!is.null(x$return_level_ci)) {
    cat("\n  Return levels:\n\n")
    print(x$return_level_ci, row.names = FALSE, digits = 5)
  }

  cat("\n")
  invisible(x)
}

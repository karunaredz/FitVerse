# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : s3_methods.R
# Description: S3 generic methods for the 'fitverse' class:
#                print.fitverse   - concise one-line summary
#                summary.fitverse - full diagnostic report
#                coef.fitverse    - extract best-fit parameters
#                logLik.fitverse  - extract log-likelihood
#                AIC.fitverse     - extract AIC
#                BIC.fitverse     - extract BIC
#
#              These make fitverse objects play nicely with
#              standard R workflows and model-comparison tools.
#
# Authors:
#   Karuna G. Reddy <karuna.reddy@auckland.ac.nz>
#   University of Auckland, New Zealand
#
#   M. G. M. Khan <mgm.khan@usp.ac.fj>
#   University of the South Pacific, Fiji
#
# Package : FitVerse
# Version : 1.0-3
# Date    : 2026-11-13
# License : GPL (>= 3)
# ============================================================


# -- print.fitverse ------------------------------------------------------------

#' @export
#' @method print fitverse
#' @noRd
print.fitverse <- function(x, ...) {
  fit   <- x$best_fit
  entry <- get_dist_entry(fit$dist_key)

  cat("\n")
  cat("-----------------------------------------------------\n")
  cat("  FitVerse - Best-Fit Distribution\n")
  cat("-----------------------------------------------------\n")
  cat(sprintf("  Distribution : %s\n",  entry$display_name))
  cat(sprintf("  Family       : %s\n",  entry$family))
  cat(sprintf("  Method       : %s\n",  fit$method))
  cat(sprintf("  n            : %d\n",  x$n))
  cat("\n")
  cat("  Estimated parameters:\n")
  for (i in seq_along(fit$params)) {
    cat(sprintf("    %-12s = %10.6f\n", names(fit$params)[i], fit$params[i]))
  }
  cat("\n")
  cat(sprintf("  Log-likelihood : %10.4f\n", fit$loglik))
  cat(sprintf("  AIC            : %10.4f\n", fit$aic))
  cat(sprintf("  BIC            : %10.4f\n", fit$bic))

  # GoF summary
  if (!is.null(fit$gof)) {
    cat("\n  Goodness-of-fit:\n")
    for (i in seq_len(nrow(fit$gof))) {
      row <- fit$gof[i, ]
      cat(sprintf("    %-25s  stat = %7.4f  p = %.4f  %s\n",
                  row$Test, row$Statistic, row$P_value, row$Stars))
    }
    cat("  Signif. codes: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10\n")
  }

  cat("-----------------------------------------------------\n")
  cat(sprintf("  (Based on %d models fitted; ranked by %s)\n",
              nrow(x$ranking), x$criterion))
  cat("\n")

  invisible(x)
}


# -- fitverse_summary class ----------------------------------------------------

#' @export
#' @method summary fitverse
#' @noRd
summary.fitverse <- function(object, top_n = 10L, ...) {
  tbl <- head(object$ranking, top_n)
  out <- structure(
    list(
      object       = object,
      top_n        = top_n,
      data_summary = object$data_summary,
      ranking      = tbl
    ),
    class = "fitverse_summary"
  )
  out
}


#' @export
#' @method print fitverse_summary
#' @noRd
print.fitverse_summary <- function(x, ...) {
  object <- x$object

  cat("\n")
  cat("===============================================================\n")
  cat("  FitVerse: Distribution Fitting Summary\n")
  cat("===============================================================\n")

  # Data summary section
  if (!is.null(x$data_summary)) {
    ds <- x$data_summary
    cat(sprintf("  Sample size  : %d\n",           ds$n))
    cat(sprintf("  Mean         : %.4f\n",          ds$mean))
    cat(sprintf("  Std Dev      : %.4f\n",          ds$sd))
    cat(sprintf("  Skewness     : %.4f\n",          ds$skewness))
    cat(sprintf("  Kurtosis     : %.4f  (excess)\n",ds$kurtosis))
    cat(sprintf("  Range        : [%.4f, %.4f]\n",  ds$min, ds$max))
  } else {
    xvec <- object$data
    cat(sprintf("  Sample size  : %d\n",   object$n))
    cat(sprintf("  Mean         : %.4f\n", mean(xvec)))
    cat(sprintf("  Std Dev      : %.4f\n", stats::sd(xvec)))
    cat(sprintf("  Range        : [%.4f, %.4f]\n", min(xvec), max(xvec)))
  }
  cat(sprintf("  Methods used : %s\n", paste(object$method, collapse = ", ")))
  cat(sprintf("  Criterion    : %s\n",  object$criterion))

  # Best-fit section
  cat("\n")
  print(object)

  # Ranking table
  tbl       <- x$ranking
  show_cols <- c("Rank", "Distribution", "Method", "AIC", "Delta_AIC",
                 "BIC", "KS_p", "AD_p")
  show_cols <- show_cols[show_cols %in% names(tbl)]
  cat("\n  Full ranking table (top", nrow(tbl), "models):\n\n")
  print(tbl[, show_cols], row.names = FALSE)

  cat("\n")
  cat("===============================================================\n")

  invisible(x)
}


# -- coef.fitverse ------------------------------------------------------------

#' @export
#' @method coef fitverse
#' @noRd
coef.fitverse <- function(object, ...) {
  object$best_fit$params
}


# -- logLik.fitverse -----------------------------------------------------------

#' @export
#' @method logLik fitverse
#' @noRd
logLik.fitverse <- function(object, ...) {
  fit <- object$best_fit
  ll  <- structure(fit$loglik,
                   df    = fit$n_params,
                   nobs  = fit$n,
                   class = "logLik")
  ll
}


# -- AIC.fitverse -------------------------------------------------------------

#' @export
#' @method AIC fitverse
#' @noRd
AIC.fitverse <- function(object, ..., k = 2) {
  fit <- object$best_fit
  k * fit$n_params - 2 * fit$loglik
}


# -- BIC.fitverse -------------------------------------------------------------

#' @export
#' @method BIC fitverse
#' @noRd
BIC.fitverse <- function(object, ...) {
  object$best_fit$bic
}


# -- as.list.fitverse ----------------------------------------------------------

#' @export
#' @method as.list fitverse
#' @noRd
as.list.fitverse <- function(x, ...) {

  bf    <- x$best_fit
  entry <- get_dist_entry(bf$dist_key)

  # Serialise GoF table
  gof_list <- if (!is.null(bf$gof)) {
    lapply(seq_len(nrow(bf$gof)), function(i) as.list(bf$gof[i, ]))
  } else NULL

  # Serialise ranking table (drop dist_key internal column for output)
  rank_df <- x$ranking
  rank_out <- lapply(seq_len(nrow(rank_df)), function(i) as.list(rank_df[i, ]))

  list(
    best_fit = list(
      dist_key     = bf$dist_key,
      display_name = entry$display_name,
      family       = entry$family,
      method       = bf$method,
      params       = as.list(bf$params),
      loglik       = bf$loglik,
      aic          = bf$aic,
      bic          = bf$bic,
      n_params     = bf$n_params,
      converged    = bf$converged,
      gof          = gof_list
    ),
    ranking      = rank_out,
    data_summary = x$data_summary,
    n            = x$n,
    xlab         = x$xlab %||% "x",
    criterion    = x$criterion,
    method       = x$method
  )
}


# -- to_json -------------------------------------------------------------------

#' @export
#' @noRd
to_json <- function(x, pretty = TRUE, ...) {
  UseMethod("to_json")
}

#' @export
#' @method to_json fitverse
#' @noRd
to_json.fitverse <- function(x, pretty = TRUE, ...) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required for to_json(). Install it with install.packages('jsonlite').",
         call. = FALSE)
  jsonlite::toJSON(as.list(x), pretty = pretty, auto_unbox = TRUE, na = "null")
}


# -- ranking() accessor --------------------------------------------------------

#' @export
#' @noRd
ranking <- function(object, ...) UseMethod("ranking")

#' @export
#' @method ranking fitverse
#' @noRd
ranking.fitverse <- function(object, ...) {
  object$ranking
}


# -- all_fits() accessor -------------------------------------------------------

#' @export
#' @noRd
all_fits <- function(object, ...) UseMethod("all_fits")

#' @export
#' @method all_fits fitverse
#' @noRd
all_fits.fitverse <- function(object, ...) {
  object$fits
}


# -- NULL coalescing helper ----------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

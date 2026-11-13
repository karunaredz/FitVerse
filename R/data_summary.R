# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : data_summary.R
# Description: Computes a rich set of descriptive statistics for
#              a numeric data vector, stored as part of every
#              fitverse object and used in plots, reports, and
#              the web interface.
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


#' Compute descriptive statistics for a numeric data vector
#'
#' Returns a named list of summary statistics that are stored in every
#' \code{fitverse} object and displayed in plots, console output, and
#' auto-generated reports.
#'
#' @param x     Numeric vector of clean (finite) observations.
#' @param xlab  Character label for the variable (e.g. \code{"Income (NZD)"}).
#'              Defaults to \code{"x"}.
#' @param n_raw Integer: number of observations in the original (uncleaned)
#'              vector, used to report how many non-finite values were removed.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{xlab}}{Variable label.}
#'     \item{\code{n}}{Number of finite observations used.}
#'     \item{\code{n_missing}}{Number of non-finite values removed.}
#'     \item{\code{mean}}{Arithmetic mean.}
#'     \item{\code{sd}}{Standard deviation (n-1 denominator).}
#'     \item{\code{cv}}{Coefficient of variation (sd / mean); NA if mean <= 0.}
#'     \item{\code{median}}{Sample median.}
#'     \item{\code{iqr}}{Interquartile range (Q3 - Q1).}
#'     \item{\code{skewness}}{Fisher's moment skewness coefficient.}
#'     \item{\code{kurtosis}}{Excess kurtosis (normal = 0).}
#'     \item{\code{min}}{Minimum.}
#'     \item{\code{max}}{Maximum.}
#'     \item{\code{range}}{max - min.}
#'     \item{\code{q25}}{First quartile.}
#'     \item{\code{q75}}{Third quartile.}
#'   }
#'
#' @export
#' @examples
#' x <- rgamma(200, shape = 2, rate = 0.5)
#' compute_data_summary(x, xlab = "Waiting time (s)")
compute_data_summary <- function(x, xlab = "x", n_raw = NULL) {

  stopifnot(is.numeric(x), length(x) >= 1)

  n         <- length(x)
  n_missing <- if (!is.null(n_raw)) as.integer(n_raw - n) else 0L

  q_vals <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE)
  m      <- mean(x)
  s      <- stats::sd(x)

  list(
    xlab      = as.character(xlab),
    n         = n,
    n_missing = n_missing,
    mean      = m,
    sd        = s,
    cv        = if (m > 0) s / m else NA_real_,
    median    = stats::median(x),
    iqr       = stats::IQR(x),
    skewness  = skewness_stat(x),
    kurtosis  = kurtosis_stat(x),
    min       = min(x),
    max       = max(x),
    range     = max(x) - min(x),
    q25       = q_vals[1],
    q75       = q_vals[2]
  )
}


#' @noRd
print_data_summary <- function(ds) {

  cat(sprintf("  Variable   : %s\n", ds$xlab))
  cat(sprintf("  n          : %d", ds$n))
  if (ds$n_missing > 0)
    cat(sprintf("  (%d non-finite removed)", ds$n_missing))
  cat("\n")
  cat(sprintf("  Range      : [%.4g, %.4g]\n", ds$min, ds$max))
  cat(sprintf("  Mean       : %.4g   SD : %.4g   CV : %s\n",
              ds$mean, ds$sd,
              if (is.na(ds$cv)) "--" else sprintf("%.3f", ds$cv)))
  cat(sprintf("  Median     : %.4g   IQR: %.4g\n", ds$median, ds$iqr))
  cat(sprintf("  Skewness   : %.4g   Excess kurtosis: %.4g\n",
              ds$skewness, ds$kurtosis))
  invisible(ds)
}

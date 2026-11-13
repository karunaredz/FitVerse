# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : utils.R
# Description: Internal utility functions used throughout the
#              FitVerse package. Not exported to the user.
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


# -- Data validation ---------------------------------------------------------

#' Validate that input is a suitable numeric vector
#'
#' Stops with an informative message if the data are not numeric, have fewer
#' than 4 finite observations, or violate a stated support constraint.
#'
#' @param x           Numeric vector supplied by the user.
#' @param dist_name   Distribution name, used only in error messages.
#' @param require_pos If TRUE, all values must be strictly positive.
#' @param require_01  If TRUE, all values must lie in the open interval (0,1).
#' @return Invisibly TRUE; stops on failure.
#' @keywords internal
check_data <- function(x, dist_name = NULL, require_pos = FALSE,
                       require_01 = FALSE) {
  if (!is.numeric(x))
    stop("Data must be a numeric vector.", call. = FALSE)

  x_fin <- x[is.finite(x)]

  if (length(x_fin) < 4)
    stop("At least 4 finite observations are required.", call. = FALSE)

  if (require_pos && any(x_fin <= 0))
    stop(sprintf(
      paste0("Distribution '%s' requires strictly positive data (x > 0).\n",
             "  Minimum value observed: %g"),
      dist_name, min(x_fin)), call. = FALSE)

  if (require_01 && (any(x_fin <= 0) || any(x_fin >= 1)))
    stop(sprintf(
      paste0("Distribution '%s' requires data in the open interval (0, 1).\n",
             "  Data range: [%g, %g]"),
      dist_name, min(x_fin), max(x_fin)), call. = FALSE)

  invisible(TRUE)
}


#' Remove non-finite values from a vector
#'
#' Issues a message if any values are removed so the user is aware.
#'
#' @param x Numeric vector (possibly containing NA / Inf / NaN).
#' @return Numeric vector containing only finite values.
#' @keywords internal
clean_data <- function(x) {
  clean    <- x[is.finite(x)]
  n_removed <- length(x) - length(clean)
  if (n_removed > 0)
    message(sprintf("  [note] %d non-finite value(s) (NA/Inf/NaN) removed before fitting.",
                n_removed))
  clean
}


# -- Descriptive statistics ---------------------------------------------------

#' Sample skewness (Fisher's moment coefficient)
#' @param x Numeric vector (finite values only).
#' @return Numeric scalar.
#' @keywords internal
skewness_stat <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  (sum((x - m)^3) / n) / s^3
}


#' Sample excess kurtosis
#' @param x Numeric vector (finite values only).
#' @return Numeric scalar.
#' @keywords internal
kurtosis_stat <- function(x) {
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  (sum((x - m)^4) / n) / s^4 - 3
}


# -- Information criteria -----------------------------------------------------

#' Compute AIC from log-likelihood and parameter count
#' @param loglik Numeric log-likelihood value.
#' @param k      Integer number of free parameters.
#' @return AIC value, or NA if log-likelihood is not finite.
#' @keywords internal
compute_aic <- function(loglik, k) {
  if (!is.finite(loglik)) return(NA_real_)
  2 * k - 2 * loglik
}


#' Compute BIC from log-likelihood, parameter count, and sample size
#' @param loglik Numeric log-likelihood value.
#' @param k      Integer number of free parameters.
#' @param n      Sample size.
#' @return BIC value, or NA if log-likelihood is not finite.
#' @keywords internal
compute_bic <- function(loglik, k, n) {
  if (!is.finite(loglik)) return(NA_real_)
  k * log(n) - 2 * loglik
}


# -- Formatting helpers --------------------------------------------------------

#' Format a named numeric vector as "name (symbol) = val, ..."
#'
#' @param params Named numeric vector of parameter estimates.
#' @param digits Number of decimal places.
#' @param labels Optional character vector of Greek/symbol labels in the same
#'   order as \code{params} (e.g. \code{c("k", "lambda")} for Weibull).
#'   When supplied each term is rendered as \code{name (symbol) = value}.
#' @return Single character string.
#' @keywords internal
format_params <- function(params, digits = 4, labels = NULL) {
  vals  <- round(params, digits)
  nms   <- names(params)
  if (!is.null(labels) && length(labels) == length(params)) {
    terms <- paste0(nms, " (", labels, ") = ", vals)
  } else {
    terms <- paste0(nms, " = ", vals)
  }
  paste(terms, collapse = ", ")
}


#' Right-pad a string to a fixed width (for aligned console output)
#' @param s     Character string.
#' @param width Target width in characters.
#' @return String padded with trailing spaces.
#' @keywords internal
pad_right <- function(s, width = 30) {
  formatC(s, width = -width, flag = "-")
}


#' Render a star-rating for a p-value (used in GoF table output)
#' @param p Numeric p-value.
#' @return Character string "***", "**", "*", "." or " ".
#' @keywords internal
pval_stars <- function(p) {
  if (is.na(p))    return("   ")
  if (p < 0.001)   return("***")
  if (p < 0.01)    return("** ")
  if (p < 0.05)    return("*  ")
  if (p < 0.1)     return(".  ")
  return("   ")
}


# -- Package availability ------------------------------------------------------

#' Check whether an optional package is installed
#'
#' Emits an informative \code{message()} (not an error) if the package is
#' missing, so that the caller can skip optional functionality gracefully.
#'
#' @param pkg     Package name as a character string.
#' @param feature Short description of what requires the package (for message).
#' @return Logical TRUE if available, FALSE otherwise.
#' @keywords internal
has_pkg <- function(pkg, feature = NULL) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  if (!ok && !is.null(feature))
    cat(sprintf("  [note] Package '%s' needed for %s -- install.packages('%s')\n",
                pkg, feature, pkg))
  ok
}


# -- Safe fitting wrapper ------------------------------------------------------

#' Attempt a fitting call and return NULL (with a message) on failure
#'
#' Wraps any fitting function in tryCatch so that a single failed distribution
#' does not abort the entire \code{fitverse()} run.
#'
#' @param expr An unevaluated expression (use \code{quote()} or \code{\{\}}).
#' @param dist_name Distribution name, for the failure message.
#' @param method    Method name, for the failure message.
#' @return The result of evaluating \code{expr}, or NULL on error/warning-as-error.
#' @keywords internal
safe_fit <- function(expr, dist_name, method) {
  # IMPORTANT: use withCallingHandlers for warnings (non-terminating - execution
  # continues and the return value is still captured) wrapped inside tryCatch
  # for errors (terminating).  The old pattern of catching warnings in tryCatch
  # interrupted execution and prevented the error handler from firing correctly.
  tryCatch(
    withCallingHandlers(
      eval(expr),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) {
      raw <- conditionMessage(e)
      friendly <- if (grepl("error code [67]|did not converge|optim",
                            raw, ignore.case = TRUE)) {
        "optimizer did not converge for this dataset"
      } else if (grepl("non-finite|infinite|NaN", raw, ignore.case = TRUE)) {
        "log-likelihood produced non-finite values"
      } else if (grepl("initial.*value|start.*param|bound", raw,
                        ignore.case = TRUE)) {
        "initial parameter values are out of range"
      } else if (grepl("singular|hessian|matrix", raw, ignore.case = TRUE)) {
        "singular Hessian -- distribution may be unidentifiable here"
      } else {
        # Truncate very long technical messages
        if (nchar(raw) > 70) paste0(substr(raw, 1, 67), "...") else raw
      }
      cat(sprintf("  [skipped] %-18s -- %s\n",
                  sprintf("%s (%s)", dist_name, method), friendly))
      NULL
    }
  )
}


# -- Distribution function exposure -------------------------------------------

#' Expose d/p/q functions from actuar or mc2d into a caller's environment
#'
#' fitdistrplus finds density functions via get() on the search path.
#' When actuar/mc2d are in Imports (not Depends), their functions are invisible
#' to fitdistrplus. Calling this inside a fitting function assigns the d/p/q
#' functions into that function's own frame, making them visible to fitdistrplus
#' via parent.frame() traversal.
#'
#' @param pkg    Package name, e.g. "actuar", "mc2d" (or "stats" -> no-op).
#' @param r_name Distribution prefix, e.g. "llogis", "pareto", "triang".
#' @param envir  Environment to assign into (default: caller's own frame).
#' @keywords internal
.expose_dist_fns <- function(pkg, r_name, envir = parent.frame()) {
  if (is.null(pkg) || pkg %in% c("stats", "base", "")) return(invisible(NULL))
  if (!requireNamespace(pkg, quietly = TRUE))          return(invisible(NULL))
  for (prefix in c("d", "p", "q")) {
    fn_name <- paste0(prefix, r_name)
    fn_obj  <- tryCatch(getExportedValue(pkg, fn_name), error = function(e) NULL)
    if (!is.null(fn_obj)) assign(fn_name, fn_obj, envir = envir)
  }
  invisible(NULL)
}


# -- Colour palette ------------------------------------------------------------
# Note: dweibull3p / pweibull3p / qweibull3p / rweibull3p are defined in
# probability_tools.R so they exist before .FITVERSE_DPQR_OVERRIDES is built.

#' FitVerse colour palette for multi-distribution comparison plots
#' @param n Number of colours required.
#' @return Character vector of hex colour codes.
#' @keywords internal
fv_palette <- function(n = 10) {
  base_cols <- c(
    "#2E86AB",   # steel blue    - best fit
    "#E84855",   # crimson       - 2nd
    "#3BB273",   # emerald       - 3rd
    "#F4A261",   # sandy orange  - 4th
    "#9B5DE5",   # purple        - 5th
    "#F15BB5",   # pink          - 6th
    "#00BBF9",   # sky blue      - 7th
    "#FEE440",   # yellow        - 8th
    "#8338EC",   # violet        - 9th
    "#FB5607"    # orange-red    - 10th
  )
  if (n <= length(base_cols)) return(base_cols[seq_len(n)])
  # Recycle if more than 10 needed
  rep_len(base_cols, n)
}

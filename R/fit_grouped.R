# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : fit_grouped.R
# Description: Distribution fitting for grouped / interval data.
#
#              Many real-world datasets arrive not as raw
#              observations but as frequency tables:
#
#                Format A (midpoint):
#                  midpoint | count
#                  ---------|------
#                       50  |     3
#                       70  |    17
#                      ...
#
#                Format B (interval):
#                  lower | upper | count
#                  ------|-------|------
#                      0 |   100 |     3
#                    100 |   200 |    17
#                      ...
#
#              For MLE the grouped likelihood is used:
#
#                log L(theta) = sum_i  n_i * log[ F(b_i;theta) - F(a_i;theta) ]
#
#              where a_i / b_i are the interval boundaries and n_i
#              the count.  This is more accurate than the midpoint
#              expansion (which treats group marks as exact values)
#              especially for coarsely grouped data.
#
#              For L-Moments and MoM the data are expanded to
#              weighted pseudo-observations (each midpoint replicated
#              n_i times) and the standard fitters are called.
#
# Public API:
#   fitverse_grouped(midpoints, counts, ...)
#   fitverse_grouped(lower, upper, counts, ...)
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
# Date    : 2026-07-18
# License : GPL (>= 3)
# ============================================================


# -- Public entry point --------------------------------------------------------

#' Fit distributions to grouped / interval data
#'
#' Accepts a frequency table -- either as (midpoints, counts) or
#' (lower, upper, counts) -- and fits probability distributions using
#' the grouped likelihood for MLE and weighted pseudo-observations for
#' L-moments and method-of-moments.
#'
#' @param midpoints Numeric vector of class midpoints (class marks).
#'   Provide either \code{midpoints} \emph{or} both \code{lower} and
#'   \code{upper}; not both.
#' @param counts    Integer vector of observed counts for each class.
#'   Must be the same length as \code{midpoints} (or \code{lower}).
#' @param lower     Numeric vector of lower class boundaries.
#' @param upper     Numeric vector of upper class boundaries.
#'   The last interval may use \code{Inf}; it is handled correctly.
#' @param dists     Distributions to fit.  Same as \code{\link{fitverse}()}.
#'   Default \code{"auto"} uses all applicable distributions.
#' @param method    Estimation method: \code{"MLE"}, \code{"L-Moments"},
#'   \code{"MOM"}, or \code{"All"}.  Default \code{"All"}.
#' @param xlab      Variable label used in plots and reports.
#' @param top_n     Number of top distributions to display.  Default \code{5}.
#' @param verbose   Print progress messages?  Default \code{FALSE}.
#'
#' @return A \code{fitverse} object identical to the output of
#'   \code{\link{fitverse}()}, with an additional component
#'   \code{$grouped_data} containing the original frequency table.
#'
#' @export
#' @examples
#' \dontrun{
#' # Midpoint format
#' fitverse_grouped(
#'   midpoints = c(50, 70, 90, 110, 130, 150),
#'   counts    = c( 3, 17,  32,  14,   6,   5),
#'   xlab      = "Annual maximum rainfall (mm)"
#' )
#'
#' # Interval format
#' fitverse_grouped(
#'   lower  = c(  0, 100, 200, 350, 500, 700,  900, 1200),
#'   upper  = c(100, 200, 350, 500, 700, 900, 1200, 1600),
#'   counts = c(  3,  17,  21,  18,  26,  15,   16,    4),
#'   xlab   = "Failure time (hours)"
#' )
#' }
fitverse_grouped <- function(midpoints = NULL,
                              counts,
                              lower   = NULL,
                              upper   = NULL,
                              dists   = "auto",
                              method  = "All",
                              xlab    = "x",
                              top_n   = 5L,
                              verbose = FALSE) {

  # -- Input validation --------------------------------------------------------
  counts <- as.integer(round(counts))
  if (any(counts < 0L))
    stop("All counts must be non-negative.", call. = FALSE)
  if (sum(counts) < 10L)
    stop("Total count must be at least 10 for reliable fitting.", call. = FALSE)

  has_mids <- !is.null(midpoints)
  has_ints <- !is.null(lower) && !is.null(upper)

  if (has_mids && has_ints)
    stop("Supply either midpoints OR (lower, upper), not both.", call. = FALSE)
  if (!has_mids && !has_ints)
    stop("Supply either midpoints or both lower and upper boundaries.", call. = FALSE)

  # -- Derive boundaries and midpoints -----------------------------------------
  if (has_mids) {
    midpoints <- as.numeric(midpoints)
    if (length(midpoints) != length(counts))
      stop("midpoints and counts must have the same length.", call. = FALSE)

    # Reconstruct interval boundaries from midpoints (assume equal-width classes)
    half_w <- if (length(midpoints) > 1L)
      min(diff(midpoints)) / 2
    else
      midpoints[1L] * 0.5
    lower_bd <- midpoints - half_w
    upper_bd <- midpoints + half_w
    lower_bd[1L] <- max(0, lower_bd[1L])  # clamp first lower to 0 if positive
  } else {
    lower <- as.numeric(lower)
    upper <- as.numeric(upper)
    if (length(lower) != length(counts) || length(upper) != length(counts))
      stop("lower, upper, and counts must have the same length.", call. = FALSE)
    if (any(lower >= upper, na.rm = TRUE))
      stop("Each lower boundary must be strictly less than its upper boundary.",
           call. = FALSE)
    midpoints <- (lower + pmin(upper, 1e9)) / 2  # midpoint (cap Inf for mean)
    lower_bd  <- lower
    upper_bd  <- upper
  }

  # Remove zero-count classes
  keep      <- counts > 0L
  midpoints <- midpoints[keep]
  lower_bd  <- lower_bd[keep]
  upper_bd  <- upper_bd[keep]
  counts    <- counts[keep]

  # Store original table for the output object
  grouped_tbl <- data.frame(
    lower    = lower_bd,
    upper    = upper_bd,
    midpoint = midpoints,
    count    = counts,
    stringsAsFactors = FALSE
  )

  # -- Expand to pseudo-observations for L-moments / MoM -----------------------
  # For MLE, this expansion is also used to get MLE starting values.
  x_pseudo <- rep(midpoints, times = counts)
  n_total  <- sum(counts)

  if (verbose)
    message(sprintf("[FitVerse] Grouped data: %d classes, n = %d",
                    length(counts), n_total))

  # -- Resolve distribution list ------------------------------------------------
  if (identical(dists, "auto")) {
    all_keys <- names(FITVERSE_DIST_REGISTRY)
    # Exclude purely discrete or bounded distributions unlikely to fit
    # grouped continuous data well
    exclude  <- c("uniform")
    dist_keys <- setdiff(all_keys, exclude)
  } else {
    dist_keys <- dists
  }

  # -- Phase 1: fit MLE via grouped likelihood ----------------------------------
  mle_results  <- list()
  lmom_results <- list()
  mom_results  <- list()

  method_upper <- toupper(trimws(method))
  do_mle  <- method_upper %in% c("MLE",  "ALL")
  do_lmom <- method_upper %in% c("L-MOMENTS", "LMOM", "L-MOM", "ALL")
  do_mom  <- method_upper %in% c("MOM",  "MOMENT", "MOMENTS",
                                  "METHOD OF MOMENTS", "ALL")

  if (do_mle) {
    if (verbose) message("[FitVerse] Running grouped MLE...")
    for (dk in dist_keys) {
      res <- tryCatch(
        .grouped_mle(lower_bd, upper_bd, counts, dk, x_pseudo, verbose),
        error = function(e) NULL
      )
      if (!is.null(res))
        mle_results[[paste0(dk, "_MLE")]] <- res
    }
  }

  if (do_lmom) {
    if (verbose) message("[FitVerse] Running L-Moments on pseudo-observations...")
    for (dk in dist_keys) {
      res <- tryCatch(fit_lmom(x_pseudo, dk), error = function(e) NULL)
      if (!is.null(res))
        lmom_results[[paste0(dk, "_L-Moments")]] <- res
    }
  }

  if (do_mom) {
    if (verbose) message("[FitVerse] Running MoM on pseudo-observations...")
    for (dk in dist_keys) {
      res <- tryCatch(fit_mom(x_pseudo, dk), error = function(e) NULL)
      if (!is.null(res))
        mom_results[[paste0(dk, "_MOM")]] <- res
    }
  }

  all_fits <- c(mle_results, lmom_results, mom_results)
  all_fits <- Filter(Negate(is.null), all_fits)

  if (length(all_fits) == 0L)
    stop("No distributions could be fitted to this grouped data.", call. = FALSE)

  # -- Attach GoF (use pseudo-observations -- standard GoF on expanded data) ----
  all_fits <- lapply(all_fits, function(f) {
    tryCatch(attach_gof(f, x_pseudo), error = function(e) f)
  })

  # -- Rank and build fitverse object -----------------------------------------
  ranking  <- build_ranking_table(all_fits, criterion = "AIC")
  best_fit <- select_best_fit(ranking, all_fits)

  ds <- compute_data_summary(x_pseudo, xlab = xlab)

  fv <- structure(
    list(
      best_fit     = best_fit,
      all_fits     = all_fits,
      ranking      = ranking,
      data         = x_pseudo,
      n            = n_total,
      data_summary = ds,
      criterion    = "AIC",
      grouped_data = grouped_tbl,   # extra: original frequency table
      call         = match.call()
    ),
    class = "fitverse"
  )

  if (verbose) {
    message(sprintf(
      "[FitVerse] Best fit: %s  (%s)  AIC = %.2f",
      get_dist_entry(best_fit$dist_key)$display_name,
      best_fit$method, best_fit$aic
    ))
  }

  fv
}


# -- Grouped MLE --------------------------------------------------------------

#' Fit a single distribution by grouped MLE
#'
#' Maximises sum_i  n_i * log[ F(upper_i) - F(lower_i) ]
#' using optim() with Nelder-Mead.  Starting values are taken from
#' the standard MLE fit on the pseudo-observations (midpoint x count).
#'
#' @param lower_bd,upper_bd Numeric vectors of class boundaries.
#' @param counts    Integer count vector.
#' @param dist_key  FitVerse distribution key.
#' @param x_pseudo  Expanded midpoint pseudo-observations (for starting values).
#' @param verbose   Print warnings?
#' @keywords internal
.grouped_mle <- function(lower_bd, upper_bd, counts, dist_key,
                          x_pseudo, verbose = FALSE) {

  entry <- tryCatch(get_dist_entry(dist_key), error = function(e) NULL)
  if (is.null(entry) || !isTRUE(entry$supports_mle)) return(NULL)

  p_fun <- tryCatch(.lookup_dpqr("p", dist_key), error = function(e) NULL)
  if (is.null(p_fun)) return(NULL)

  # -- Starting values: standard MLE on pseudo-observations ------------------
  start_fit <- tryCatch(fit_mle(x_pseudo, dist_key), error = function(e) NULL)
  if (is.null(start_fit)) return(NULL)
  start_params <- start_fit$params

  # -- Grouped log-likelihood -------------------------------------------------
  # Handle open-ended intervals: lower = 0 or -Inf -> F(lower) = 0
  #                               upper = Inf       -> F(upper) = 1
  grouped_ll <- function(par_vec) {
    if (any(!is.finite(par_vec))) return(-1e300)
    params_list <- as.list(par_vec)
    names(params_list) <- names(start_params)

    F_upper <- tryCatch(
      do.call(p_fun, c(list(q = upper_bd), params_list)),
      error = function(e) NULL
    )
    F_lower <- tryCatch(
      do.call(p_fun, c(list(q = lower_bd), params_list)),
      error = function(e) NULL
    )

    if (is.null(F_upper) || is.null(F_lower)) return(-1e300)
    if (!all(is.finite(F_upper)) || !all(is.finite(F_lower))) return(-1e300)

    # Detect CDF overflow: if values are substantially outside [0, 1] the
    # optimizer has landed on degenerate parameters.  Penalise heavily so
    # the optimizer moves away; a tolerance of 0.01 allows minor floating-
    # point imprecision without masking real overflow.
    if (any(F_upper > 1.01, na.rm = TRUE) ||
        any(F_lower < -0.01, na.rm = TRUE) ||
        any(F_lower > 1.01, na.rm = TRUE)) return(-1e300)

    # Clamp CDF values to [0, 1] for minor floating-point imprecision only
    F_upper <- pmin(pmax(F_upper, 0.0), 1.0)
    F_lower <- pmin(pmax(F_lower, 0.0), 1.0)

    # bin probability for each class: clamp to (0, 1]
    bin_p <- F_upper - F_lower
    bin_p <- pmin(pmax(bin_p, 1e-300), 1.0)

    if (any(!is.finite(bin_p))) return(-1e300)
    sum(counts * log(bin_p))
  }

  # -- Optimise --------------------------------------------------------------
  opt <- tryCatch(
    stats::optim(
      par     = as.numeric(start_params),
      fn      = grouped_ll,
      method  = "Nelder-Mead",
      control = list(fnscale = -1, maxit = 10000, reltol = 1e-9)
    ),
    error = function(e) NULL
  )

  if (is.null(opt) || opt$convergence > 1L) return(NULL)

  opt_params <- opt$par
  names(opt_params) <- names(start_params)

  # Validate parameters
  if (any(!is.finite(opt_params))) return(NULL)

  ll_val <- opt$value
  if (!is.finite(ll_val)) return(NULL)
  # Grouped log-likelihood = sum n_i*log(p_i) where p_i in (0,1] -> must be < 0.
  # LL = 0 means all bin_p were clamped to 1.0 (CDF overflow) -> degenerate.
  if (ll_val >= 0) return(NULL)
  # LL cannot exceed the saturated-model LL: sum(n_i * log(n_i/N)).
  # Any fitted model beating this is a sign of partial clamping / overflow.
  saturated_ll <- sum(counts * log(counts / sum(counts)))
  if (ll_val > saturated_ll + 0.5) return(NULL)

  n_p <- length(opt_params)
  aic <- -2 * ll_val + 2 * n_p
  bic <- -2 * ll_val + n_p * log(sum(counts))

  list(
    dist_key  = dist_key,
    method    = "MLE",
    params    = opt_params,
    loglik    = ll_val,
    aic       = aic,
    bic       = bic,
    n_params  = n_p,
    converged = (opt$convergence == 0L),
    grouped   = TRUE          # flag: fitted via grouped likelihood
  )
}


# -- Utility: read grouped CSV ------------------------------------------------

#' @export
#' @noRd
read_grouped_csv <- function(file, sep = ",") {
  # Strip comment lines and read
  lines <- readLines(file)
  lines <- lines[!grepl("^\\s*#", lines)]
  df    <- utils::read.csv(text = paste(lines, collapse = "\n"),
                            sep = sep, stringsAsFactors = FALSE,
                            strip.white = TRUE)

  cols  <- tolower(trimws(names(df)))

  if (length(cols) == 2L &&
      any(cols[1L] %in% c("midpoint", "mid", "class_mid", "class_mark",
                           "mark", "x", "value"))) {
    list(midpoints = as.numeric(df[[1L]]),
         counts    = as.integer(df[[2L]]))
  } else if (length(cols) >= 3L &&
             any(cols[1L] %in% c("lower", "low", "lo", "from", "a")) &&
             any(cols[2L] %in% c("upper", "up", "hi", "to", "b"))) {
    list(lower  = as.numeric(df[[1L]]),
         upper  = as.numeric(df[[2L]]),
         counts = as.integer(df[[3L]]))
  } else {
    # Auto-detect by column count
    if (ncol(df) == 2L) {
      list(midpoints = as.numeric(df[[1L]]),
           counts    = as.integer(df[[2L]]))
    } else if (ncol(df) >= 3L) {
      list(lower  = as.numeric(df[[1L]]),
           upper  = as.numeric(df[[2L]]),
           counts = as.integer(df[[3L]]))
    } else {
      stop("Cannot determine format: expected 2 or 3 columns.", call. = FALSE)
    }
  }
}

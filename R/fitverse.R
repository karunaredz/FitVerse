# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : fitverse.R
# Description: Main user-facing function fitverse().
#
#              fitverse() is the single entry-point for all
#              distribution-fitting workflows.  Given a numeric
#              data vector it:
#                1. Validates and cleans the data.
#                2. Identifies which distributions are support-compatible.
#                3. Fits each distribution with the requested method(s).
#                4. Computes goodness-of-fit tests (KS, AD, CvM).
#                5. Ranks all models by AIC (or BIC).
#                6. Selects the best-fitting model.
#                7. Optionally produces a publication-quality plot.
#                8. Returns a 'fitverse' S3 object.
#
#              Also exported: compare_dists() - fits all distributions
#              and returns just the ranked comparison table.
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


# -- fitverse() ---------------------------------------------------------------

#' Fit probability distributions to univariate continuous data
#'
#' The primary function of the FitVerse package.  Fits one or more parametric
#' distributions to a numeric data vector, selects the best fit by an
#' information criterion, runs goodness-of-fit tests, and produces a
#' publication-quality diagnostic plot.
#'
#' @param x           Numeric vector of observations (at least 4 finite values).
#' @param dists       Character vector of distribution keys to try, or
#'                    \code{"all"} (default) to try all support-compatible
#'                    distributions.  See \code{\link{fitverse_distributions}()}
#'                    for valid keys.
#' @param method      Character vector, one or more of \code{"MLE"},
#'                    \code{"MOM"}, \code{"LMOM"}.  Default: \code{"MLE"}.
#'                    All methods are run and the overall best is returned.
#' @param criterion   Character: \code{"AIC"} (default) or \code{"BIC"}.
#'                    Used for ranking and best-model selection.
#' @param xlab        Character: label for the data variable used in plot
#'                    axis titles and reports.  Default \code{"x"}.
#' @param plot        Logical: produce a diagnostic plot? Default \code{TRUE}.
#' @param interactive Logical: if \code{TRUE} and the \pkg{plotly} package is
#'                    installed, produces an interactive HTML plot instead of
#'                    a static ggplot2 figure. Default \code{FALSE}.
#' @param top_n       Integer: number of top distributions to overlay on the
#'                    comparison plot (used when \code{plot = TRUE}).
#'                    Default \code{3}.
#' @param progress    Optional function taking a single numeric argument in
#'                    \code{[0, 1]} that is called with progress updates.
#'                    Useful for Shiny progress bars.  Default \code{NULL}.
#' @param verbose     Logical: print progress messages? Default \code{TRUE}.
#' @param ...         Additional arguments passed to the plotting function.
#'
#' @return An S3 object of class \code{"fitverse"} containing:
#'   \describe{
#'     \item{\code{best_fit}}{Fit-result list for the best model.}
#'     \item{\code{all_fits}}{Named list of all fit-result lists.}
#'     \item{\code{ranking}}{Data frame of all models ranked by criterion.}
#'     \item{\code{data}}{The cleaned input data.}
#'     \item{\code{n}}{Sample size.}
#'     \item{\code{xlab}}{Variable label.}
#'     \item{\code{data_summary}}{Named list of descriptive statistics from
#'       \code{\link{compute_data_summary}()}.}
#'     \item{\code{criterion}}{Criterion used for ranking.}
#'     \item{\code{method}}{Method(s) used.}
#'     \item{\code{call}}{The matched call.}
#'   }
#'
#' @export
#' @examples
#' set.seed(42)
#' x <- rgamma(200, shape = 2, rate = 0.5)
#' fit <- fitverse(x, dists = c("gamma", "lognormal", "weibull"), method = "MLE")
#' print(fit)
#' summary(fit)
#'
#' # All distributions, multiple methods
#' fit2 <- fitverse(x, method = c("MLE", "MOM", "LMOM"), criterion = "BIC")
fitverse <- function(x,
                     dists       = "all",
                     method      = "MLE",
                     criterion   = "AIC",
                     xlab        = "x",
                     plot        = TRUE,
                     interactive = FALSE,
                     top_n       = 3L,
                     progress    = NULL,
                     verbose     = TRUE,
                     ...) {

  # -- 0. Record the call ----------------------------------------------------
  cl <- match.call()

  # -- 1. Validate and clean data --------------------------------------------
  if (!is.numeric(x))
    stop("'x' must be a numeric vector.", call. = FALSE)

  if (!is.null(progress) && !is.function(progress))
    stop("'progress' must be a function or NULL.", call. = FALSE)

  n_raw   <- length(x)
  x_clean <- clean_data(x)

  if (length(x_clean) < 4)
    stop("At least 4 finite observations are required.", call. = FALSE)

  n    <- length(x_clean)
  xlab <- as.character(xlab)[1]

  .prog <- function(p) if (is.function(progress)) progress(p)
  .prog(0)

  # -- 2. Normalise method argument -----------------------------------------
  method <- toupper(trimws(method))
  valid_methods <- c("MLE", "MOM", "LMOM")
  bad_methods   <- setdiff(method, valid_methods)
  if (length(bad_methods) > 0)
    stop(sprintf("Unknown method(s): %s.  Choose from: MLE, MOM, LMOM.",
                 paste(bad_methods, collapse = ", ")), call. = FALSE)

  criterion <- toupper(trimws(criterion))
  if (!criterion %in% c("AIC", "BIC"))
    stop("criterion must be 'AIC' or 'BIC'.", call. = FALSE)

  # -- 3. Resolve distribution list -----------------------------------------
  all_keys <- names(FITVERSE_DIST_REGISTRY)

  if (identical(dists, "all")) {
    keys <- all_keys
  } else {
    keys <- tolower(trimws(dists))
    unknown <- setdiff(keys, all_keys)
    if (length(unknown) > 0)
      stop(sprintf("Unknown distribution key(s): %s\nSee fitverse_distributions() for valid keys.",
                   paste(unknown, collapse = ", ")), call. = FALSE)
  }

  # Filter out distributions whose support is incompatible with the data
  keys <- filter_by_support(x_clean, keys)

  if (length(keys) == 0)
    stop("No distributions are compatible with the support of the supplied data.",
         call. = FALSE)

  if (verbose) {
    .fv_rule <- strrep("-", 60)
    cat(sprintf("\n%s\n", .fv_rule))
    cat(sprintf("  FitVerse  .  %d distribution(s)  .  %s  .  n = %d\n",
                length(keys), paste(method, collapse = " + "), n))
    cat(sprintf("%s\n", .fv_rule))
  }

  .prog(0.05)

  # -- 4. Fit all distributions with all requested methods -------------------
  all_fits   <- list()
  n_methods  <- length(method)

  # Suppress "Inf replaced by maximum positive value" warnings that some
  # external optimisers (e.g. actuar's quantile functions via numerical CDF
  # inversion) emit when optimize() encounters Inf at interval boundaries.
  # This is benign: optimize() correctly substitutes .Machine$double.xmax.
  # With 52 distributions the warning fires 50+ times and is pure noise.
  .muffle_inf_warn <- function(w) {
    if (grepl("Inf replaced by maximum positive value",
              conditionMessage(w), fixed = TRUE))
      invokeRestart("muffleWarning")
  }

  for (mi in seq_along(method)) {
    meth <- method[mi]
    if (verbose) cat(sprintf("  > Estimating by %s ...\n", meth))

    fits_for_method <- withCallingHandlers(
      switch(meth,
        MLE  = fit_mle_all(x_clean, keys),
        MOM  = fit_mom_all(x_clean, keys),
        LMOM = fit_lmom_all(x_clean, keys),
        list()
      ),
      warning = .muffle_inf_warn
    )

    # Key each result uniquely as "distkey_METHOD"
    if (length(fits_for_method) > 0) {
      keyed <- setNames(fits_for_method,
                        paste0(names(fits_for_method), "_", meth))
      all_fits <- c(all_fits, keyed)
    }

    .prog(0.05 + 0.65 * (mi / n_methods))
  }

  if (length(all_fits) == 0)
    stop("All distribution fits failed.  Check your data or try a different method.",
         call. = FALSE)

  # -- 5. Attach goodness-of-fit tests --------------------------------------
  if (verbose) cat("  > Goodness-of-fit tests ...\n")
  all_fits <- lapply(all_fits, function(f) attach_gof(f, x_clean))
  all_fits <- Filter(Negate(is.null), all_fits)
  .prog(0.80)

  # -- 6. Build ranking table ------------------------------------------------
  ranking <- build_ranking_table(all_fits, criterion = criterion)

  # -- 7. Select best model -------------------------------------------------
  best_fit <- select_best_fit(ranking, all_fits)

  # -- 7b. Compute descriptive statistics -----------------------------------
  data_sum <- compute_data_summary(x_clean, xlab = xlab, n_raw = n_raw)

  # -- 8. Assemble S3 object ------------------------------------------------
  result <- structure(
    list(
      best_fit     = best_fit,
      all_fits     = all_fits,
      ranking      = ranking,
      data         = x_clean,
      n            = n,
      xlab         = xlab,
      data_summary = data_sum,
      criterion    = criterion,
      method       = method,
      call         = cl
    ),
    class = "fitverse"
  )

  # -- 9. Print summary to console ------------------------------------------
  if (verbose) {
    entry    <- get_dist_entry(best_fit$dist_key)
    .fv_rule <- strrep("-", 60)

    # GoF p-values (may be NA if tests were not run)
    ks_p <- get_ks_pvalue(best_fit)
    ad_p <- get_ad_pvalue(best_fit)
    gof_line <- if (!is.na(ks_p))
      sprintf("  KS p-value  : %-10s  AD p-value : %s\n",
              sprintf("%.4f", ks_p), sprintf("%.4f", ad_p))
    else ""

    cat(sprintf("\n  OK  Best fit : %s  (%s)\n",
                entry$display_name, best_fit$method))
    cat(sprintf("  %s\n", .fv_rule))
    cat(sprintf("  Parameters  : %s\n",
                format_params(best_fit$params, digits = 4,
                              labels = entry$param_labels)))
    cat(sprintf("  AIC         : %-12.4f  BIC        : %.4f\n",
                best_fit$aic, best_fit$bic))
    cat(gof_line)
    cat(sprintf("  %s\n\n", .fv_rule))
  }

  # -- 10. Plot -------------------------------------------------------------
  if (plot) {
    plot.fitverse(result, interactive = interactive,
                  top_n = top_n, xlab = xlab, ...)
  }

  .prog(1)
  invisible(result)
}


# -- compare_dists() -----------------------------------------------------------

#' Compare all distributions on a dataset and return a ranked table
#'
#' A convenience wrapper around \code{\link{fitverse}()} that suppresses the
#' plot and returns only the ranking table - useful for a quick comparison.
#'
#' @inheritParams fitverse
#' @return A data frame ranking all fitted distributions by AIC (or BIC).
#' @export
#' @examples
#' set.seed(1)
#' x <- rlnorm(300, meanlog = 1, sdlog = 0.5)
#' compare_dists(x, method = "MLE")
compare_dists <- function(x,
                          dists     = "all",
                          method    = "MLE",
                          criterion = "AIC",
                          xlab      = "x",
                          verbose   = TRUE) {

  fit <- fitverse(x,
                  dists     = dists,
                  method    = method,
                  criterion = criterion,
                  xlab      = xlab,
                  plot      = FALSE,
                  verbose   = verbose)

  fit$ranking
}

# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : fitverse_batch.R
# Description: Batch distribution fitting across multiple columns
#              of a data frame.  Designed for the web upload
#              workflow: a user uploads a CSV with several numeric
#              columns and wants each fitted automatically.
#
#              fitverse_batch()
#                - accepts a data.frame or a named list of vectors
#                - fits each numeric column independently
#                - returns a named list of 'fitverse' objects plus
#                  a tidy summary table of all best-fit models
#                - supports a progress callback for Shiny
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


#' Fit distributions to multiple numeric columns in a data frame
#'
#' A convenience wrapper that calls \code{\link{fitverse}()} on every numeric
#' column (or vector in a named list), collects the results, and returns a
#' tidy summary table alongside the full list of \code{fitverse} objects.
#'
#' @param data      A \code{data.frame}, \code{matrix}, or named \code{list}
#'                  whose elements/columns are numeric vectors.
#' @param cols      Character vector of column names (or integer indices) to
#'                  fit.  \code{NULL} (default) selects all numeric columns.
#' @param dists     Passed to \code{\link{fitverse}()}.  Default \code{"all"}.
#' @param method    Passed to \code{\link{fitverse}()}.  Default \code{"MLE"}.
#' @param criterion Passed to \code{\link{fitverse}()}.  Default \code{"AIC"}.
#' @param top_n     Number of distributions overlaid on each plot.
#'                  Default \code{3}.
#' @param plot      Logical: produce a plot for each column? Default \code{FALSE}
#'                  (suppressed in batch mode to avoid flooding the device).
#' @param progress  Optional \code{function(p)} called with numeric progress
#'                  in \code{[0, 1]}.  Default \code{NULL}.
#' @param verbose   Logical: print per-column progress messages?
#'                  Default \code{TRUE}.
#'
#' @return A list of class \code{"fitverse_batch"} with:
#'   \describe{
#'     \item{\code{fits}}{Named list of \code{fitverse} objects, one per column.}
#'     \item{\code{summary}}{Data frame with one row per column, summarising
#'       the best-fit distribution, parameters, and key statistics.}
#'     \item{\code{failed}}{Character vector of column names that could not be
#'       fitted (empty if all succeeded).}
#'   }
#'
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   income  = rlnorm(300, meanlog = 8, sdlog = 0.8),
#'   waiting = rexp(300, rate = 0.1),
#'   damage  = rgamma(300, shape = 2, rate = 0.05)
#' )
#' batch <- fitverse_batch(df, method = "MLE", verbose = FALSE)
#' print(batch)
fitverse_batch <- function(data,
                            cols      = NULL,
                            dists     = "all",
                            method    = "MLE",
                            criterion = "AIC",
                            top_n     = 3L,
                            plot      = FALSE,
                            progress  = NULL,
                            verbose   = TRUE) {

  # -- Input coercion --------------------------------------------------------
  if (is.matrix(data)) data <- as.data.frame(data)

  if (is.data.frame(data)) {
    vec_list <- as.list(data)
  } else if (is.list(data)) {
    vec_list <- data
  } else {
    stop("'data' must be a data.frame, matrix, or named list.", call. = FALSE)
  }

  if (is.null(names(vec_list)))
    names(vec_list) <- paste0("col", seq_along(vec_list))

  # -- Column selection ------------------------------------------------------
  if (!is.null(cols)) {
    if (is.numeric(cols)) cols <- names(vec_list)[cols]
    bad <- setdiff(cols, names(vec_list))
    if (length(bad) > 0)
      stop(sprintf("Column(s) not found: %s", paste(bad, collapse = ", ")),
           call. = FALSE)
    vec_list <- vec_list[cols]
  }

  # Keep only numeric columns
  is_num <- vapply(vec_list, function(v) is.numeric(v), logical(1))
  if (!all(is_num)) {
    dropped <- names(vec_list)[!is_num]
    if (verbose)
      message(sprintf("[FitVerse] Skipping non-numeric column(s): %s",
                      paste(dropped, collapse = ", ")))
    vec_list <- vec_list[is_num]
  }

  if (length(vec_list) == 0)
    stop("No numeric columns to fit.", call. = FALSE)

  col_names <- names(vec_list)
  n_cols    <- length(col_names)

  .prog <- function(p) if (is.function(progress)) progress(p)
  .prog(0)

  # -- Fit loop --------------------------------------------------------------
  fits   <- vector("list", n_cols)
  failed <- character(0)

  for (i in seq_len(n_cols)) {
    nm <- col_names[i]
    x  <- vec_list[[i]]

    if (verbose)
      message(sprintf("[FitVerse] Batch: fitting column '%s' (%d / %d) ...",
                      nm, i, n_cols))

    fv <- tryCatch(
      fitverse(x,
               dists     = dists,
               method    = method,
               criterion = criterion,
               xlab      = nm,
               top_n     = top_n,
               plot      = plot,
               verbose   = FALSE),
      error   = function(e) {
        message(sprintf("  [Error] Column '%s': %s", nm, conditionMessage(e)))
        NULL
      }
    )

    if (is.null(fv)) {
      failed <- c(failed, nm)
    } else {
      fits[[i]] <- fv
    }

    .prog(i / n_cols)
  }

  names(fits) <- col_names
  fits        <- Filter(Negate(is.null), fits)

  # -- Tidy summary table ----------------------------------------------------
  summary_rows <- lapply(names(fits), function(nm) {
    fv    <- fits[[nm]]
    bf    <- fv$best_fit
    entry <- get_dist_entry(bf$dist_key)
    ds    <- fv$data_summary

    data.frame(
      Column       = nm,
      n            = fv$n,
      Mean         = round(ds$mean,     4),
      SD           = round(ds$sd,       4),
      Skewness     = round(ds$skewness, 4),
      Kurtosis     = round(ds$kurtosis, 4),
      Distribution = entry$display_name,
      Method       = bf$method,
      Params       = format_params(bf$params),
      AIC          = round(bf$aic, 2),
      BIC          = round(bf$bic, 2),
      stringsAsFactors = FALSE
    )
  })

  summary_tbl <- if (length(summary_rows) > 0)
    do.call(rbind, summary_rows)
  else
    data.frame()

  .prog(1)

  structure(
    list(
      fits    = fits,
      summary = summary_tbl,
      failed  = failed
    ),
    class = "fitverse_batch"
  )
}


# -- S3 methods ----------------------------------------------------------------

#' @export
#' @method print fitverse_batch
#' @noRd
print.fitverse_batch <- function(x, ...) {

  cat("\n")
  cat("===============================================================\n")
  cat("  FitVerse Batch -- Results\n")
  cat("===============================================================\n")
  cat(sprintf("  Columns fitted successfully : %d\n", length(x$fits)))
  if (length(x$failed) > 0)
    cat(sprintf("  Failed column(s)           : %s\n",
                paste(x$failed, collapse = ", ")))
  cat("\n")
  if (nrow(x$summary) > 0) {
    print(x$summary[, c("Column", "n", "Distribution", "Method",
                         "AIC", "BIC")],
          row.names = FALSE)
  }
  cat("\n")
  invisible(x)
}


#' @export
#' @method summary fitverse_batch
#' @noRd
summary.fitverse_batch <- function(object, ...) {
  cat("\n  Batch summary:\n\n")
  print(object$summary, row.names = FALSE)
  invisible(object$summary)
}

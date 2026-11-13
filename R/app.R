# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : app.R
# Description: Shiny app launcher. Defines FitVerseApp(), the
#              user-facing function that opens the interactive
#              web interface in the default browser.
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
# Date    : 2026-07-14
# ============================================================

#' Launch the FitVerse Interactive Shiny Application
#'
#' Opens the FitVerse web interface in the default browser.  The app lets
#' users upload data, fit distributions interactively, inspect diagnostic
#' plots, and download results -- without writing any R code.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}},
#'   for example \code{port} or \code{launch.browser}.
#'
#' @return Called for its side effect (launches a Shiny app); returns
#'   \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' FitVerseApp()
#' }
#'
#' @export
FitVerseApp <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required to run FitVerseApp(). ",
         "Install it with: install.packages(\"shiny\")", call. = FALSE)

  app_dir <- system.file("app", package = "FitVerse")
  if (!nzchar(app_dir) || !file.exists(app_dir))
    stop("Could not find the FitVerse app directory. ",
         "Try reinstalling the package.", call. = FALSE)

  shiny::runApp(app_dir, ...)
}

# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : zzz.R
# Description: Suppress R CMD check global-variable notes.
#              Variables used inside ggplot2::aes() and
#              data.table/dplyr pipelines are not visible to
#              R CMD check static analysis.  Listing them here
#              silences the "no visible binding" NOTE.
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

utils::globalVariables(c(
  "dist", "ecdf", "empirical", "label", "lower", "ltype",
  "mean_excess", "sigma_star", "ss_hi", "ss_lo", "surv",
  "t3", "t4", "theoretical", "threshold", "time",
  "upper", "value", "x", "xi", "xi_hi", "xi_lo",
  "y", "ymax", "ymin", "setNames"
))

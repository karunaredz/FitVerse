# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : model_selection.R
# Description: Collects all fit results, ranks them by GoF tier and AIC,
#              and identifies the best-fitting distribution.
#
#              Ranking rule (four-level sort):
#                1st - Convergence   (converged fits before non-converged)
#                2nd - GoF tier      (adequate > partial > inadequate)
#                3rd - AIC / BIC     (primary within-tier criterion)
#                4th - GoF score     (KS rank + AD rank; tiebreaker)
#
#              GoF tiers prevent fits with failing GoF tests from ranking
#              above fits that pass both tests, regardless of AIC.  Within
#              the adequate tier, AIC is used because it penalises parameter
#              count; flexible distributions (Kappa-4, GEV, etc.) that
#              achieve high GoF p-values by fitting noise are correctly
#              demoted by the AIC penalty.  GoF composite score (KS rank +
#              AD rank) serves as the final tiebreaker only.
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


# -- Build ranking table from a list of fit results ----------------------------

#' Summarise all fit results into a ranked comparison table
#'
#' @param fits      Named list of fit-result lists (from fit_mle/mom/lmom).
#' @param criterion Character, either "AIC" (default) or "BIC".
#' @return A data frame ranked by GoF tier then AIC/BIC (GoF composite score
#'   as final tiebreaker), with columns: Rank, Distribution, Method, N_params,
#'   LogLik, AIC, Delta_AIC, BIC, Delta_BIC, KS_p, AD_p, ChiSq_p,
#'   GoF_group, Converged, dist_key
#' @keywords internal
build_ranking_table <- function(fits, criterion = "AIC") {

  if (length(fits) == 0)
    stop("No successful fits to rank.", call. = FALSE)

  rows <- lapply(fits, function(f) {
    entry <- get_dist_entry(f$dist_key)
    data.frame(
      dist_key    = f$dist_key,
      Distribution = entry$display_name,
      Method      = f$method,
      N_params    = f$n_params,
      LogLik      = round(f$loglik, 4),
      AIC         = round(f$aic,    4),
      BIC         = round(f$bic,    4),
      KS_p        = round(get_ks_pvalue(f),    4),
      AD_p        = round(get_ad_pvalue(f),    4),
      ChiSq_p     = round(get_chisq_pvalue(f), 4),
      Converged   = f$converged,
      stringsAsFactors = FALSE
    )
  })

  tbl <- do.call(rbind, rows)
  rownames(tbl) <- NULL

  sort_col <- if (toupper(criterion) == "BIC") "BIC" else "AIC"

  # -- Step 1: sort by AIC/BIC for initial ordering -------------------------
  tbl <- tbl[order(tbl[[sort_col]], na.last = TRUE), ]

  # -- Step 2: GoF-adjusted re-ranking -------------------------------------
  # A model that passes at least one of KS / AD at alpha=0.05 is deemed
  # adequate.  A model must FAIL BOTH KS and AD to be penalised to the
  # "inadequate" group.  This avoids over-penalising distributions that
  # fail only one test due to random sampling variation - especially
  # important at large n where GoF tests have very high power and can
  # reject even the true generating distribution on a single test.
  #
  # GoF groups (three-tier, used as the primary sort key):
  #
  #   0 = Adequate / fully trusted
  #       - BOTH tests available AND at least one passes.
  #       - Note: distributions for which GoF tests cannot be computed
  #         (both KS and AD are NA, e.g. GB2, Lindley) are NOT placed here.
  #         They fall through to tier 2 and are ranked by AIC only.
  #         This prevents distributions without GoF tests from systematically
  #         beating distributions that were tested but rejected.
  #
  #   1 = Partial / one test missing
  #       - Exactly one test is available and it passes.
  #       - Common for LMOM/MOM fits where the AD test cannot be evaluated
  #         because the fitted CDF has a bounded upper tail.  The single
  #         passing test gives some confidence but the fit is less certain
  #         than a distribution verified by both tests.
  #       - Ranked BELOW group-0 fits but ABOVE group-2 failures.
  #
  #   2 = Inadequate / no info / ranked by AIC
  #       - KS fails AND AD either also fails or is unavailable; OR
  #       - Neither test could be computed at all (both KS and AD are NA).
  #         These distributions are ranked purely by AIC within this tier.
  #
  ks_known <- !is.na(tbl$KS_p)
  ad_known <- !is.na(tbl$AD_p)
  ks_pass  <- ks_known & tbl$KS_p >= 0.05
  ad_pass  <- ad_known & tbl$AD_p >= 0.05
  ks_fail  <- ks_known & tbl$KS_p <  0.05

  # Tier 0: BOTH tests available AND at least one passes (strongest evidence).
  # "No info" (both NA) is deliberately excluded: it falls to tier 2.
  cond_adequate <- ks_known & ad_known & (ks_pass | ad_pass)
  cond_partial  <- (ks_known & !ad_known & ks_pass) |         # only KS, passes
                   (!ks_known & ad_known & ad_pass)            # only AD, passes
  # everything else -> tier 2: KS fails (and/or AD fails), or both NA

  tbl$GoF_group <- ifelse(cond_adequate, 0L,
                   ifelse(cond_partial,  1L, 2L))

  # -- AIC guard: prevent GoF-tier inflation ---------------------------------
  # A flexible distribution can pass KS/AD by overfitting (e.g. Wakeby with
  # 5 parameters at large n) yet have substantially worse AIC than simpler
  # competitors in tier 2.  If the best tier-0 distribution is more than 10
  # AIC units worse than the overall best AIC, the tier advantage is not
  # warranted -- flatten all groups and rank by AIC alone.
  # (10 AIC units is the conventional "substantial" evidence threshold.)
  if (any(tbl$GoF_group == 0L, na.rm = TRUE)) {
    min_aic_t0  <- min(tbl$AIC[tbl$GoF_group == 0L], na.rm = TRUE)
    min_aic_all <- min(tbl$AIC, na.rm = TRUE)
    if (is.finite(min_aic_t0) && is.finite(min_aic_all) &&
        (min_aic_t0 - min_aic_all) > 10) {
      tbl$GoF_group[] <- 0L   # flatten: pure AIC ranking
    }
  }

  # -- Step 2.5: GoF composite rank score -----------------------------------
  # Primary ranking within each GoF tier uses a composite score from the ranks
  # of the KS and AD p-values (higher p-value = better rank = lower rank
  # number).  This is method-agnostic: MLE, MOM, and LMOM fits compete on
  # equal footing because GoF test results do not depend on the estimation
  # method, whereas AIC is only valid for MLE-estimated parameters.
  #
  # GoF composite score = KS_rank + AD_rank   (lower = better fit)
  #
  # NA p-values receive the neutral mid-rank (n+1)/2 so that inability to
  # compute a test neither rewards nor penalises the distribution.
  n_fits   <- nrow(tbl)
  ks_ranks <- rank(-tbl$KS_p, ties.method = "average", na.last = "keep")
  ks_ranks[is.na(ks_ranks)] <- (n_fits + 1L) / 2

  ad_ranks <- rank(-tbl$AD_p, ties.method = "average", na.last = "keep")
  ad_ranks[is.na(ad_ranks)] <- (n_fits + 1L) / 2

  tbl$GoF_score <- ks_ranks + ad_ranks   # lower = better

  # -- Step 3: Convergence guard --------------------------------------------
  # Non-converged fits are placed AFTER all converged fits, regardless of AIC.
  # A non-converged optimiser may report a spuriously low AIC that it cannot
  # actually achieve -- treating such fits as rank-1 is statistically invalid.
  #
  # Conv group codes:
  #   0 = converged (trustworthy)
  #   1 = did not converge (demoted)
  conv_group <- ifelse(!is.na(tbl$Converged) & tbl$Converged, 0L, 1L)

  # Four-key sort: convergence -> GoF tier -> AIC/BIC -> GoF composite score
  #
  # AIC/BIC is the PRIMARY within-tier criterion because it penalises parameter
  # count.  GoF composite score (KS rank + AD rank) is the FINAL tiebreaker.
  # This ordering is correct because:
  #   - The three-tier GoF grouping already demotes LMOM/MOM fits with AD=NA
  #     that fail KS (-> tier 1 or 2), so they cannot displace tier-0 MLE fits.
  #   - Pure GoF composite ranking without an AIC penalty rewards flexible
  #     distributions (e.g. Kappa-4, GEV) that achieve high KS/AD p-values by
  #     having extra parameters, incorrectly demoting the true parsimonious fit.
  tbl <- tbl[order(conv_group, tbl$GoF_group, tbl[[sort_col]],
                   tbl$GoF_score, na.last = TRUE), ]
  tbl$Rank <- seq_len(nrow(tbl))

  # -- Step 4: compute Delta_AIC/BIC from the FINAL rank-1 model -----------
  # Computed here (after GoF + convergence re-ranking) so that a demoted
  # model with a spuriously low AIC never inflates all other Delta values.
  tbl$Delta_AIC <- round(tbl$AIC - tbl$AIC[1], 4)
  tbl$Delta_BIC <- round(tbl$BIC - tbl$BIC[1], 4)

  # -- Step 5: one row per distribution -------------------------------------
  # When method = "ALL", MLE / MOM / LMOM fits for the same distribution all
  # appear in the table.  After the sort above the first occurrence of each
  # dist_key is already the best-ranked (lowest AIC) method; removing later
  # occurrences ensures every distribution occupies exactly one ranking slot.
  # This prevents a single distribution from claiming the top-3 positions and
  # ensures the density overlay shows three distinct distributions.
  # Users who want to compare estimation methods for a specific distribution
  # can re-run with that method explicitly selected (MLE / MOM / LMOM).
  if (anyDuplicated(tbl$dist_key)) {
    tbl       <- tbl[!duplicated(tbl$dist_key), ]
    tbl$Rank  <- seq_len(nrow(tbl))
  }

  # Reorder columns for display
  tbl[, c("Rank", "Distribution", "Method", "N_params",
          "LogLik", "AIC", "Delta_AIC", "BIC", "Delta_BIC",
          "KS_p", "AD_p", "ChiSq_p", "GoF_group", "Converged", "dist_key")]
}


# -- Select the best fit -------------------------------------------------------

#' Identify the best-fitting model from a ranked table
#'
#' Returns the Rank-1 model from the ranked table produced by
#' \code{build_ranking_table()}.  Rank 1 is determined by the four-key
#' sort: convergence, then GoF tier, then AIC/BIC, then GoF composite score.
#'
#' @param ranking  Data frame from build_ranking_table().
#' @param fits     Named list of fit-result lists.
#' @return A single fit-result list for the best model.
#' @keywords internal
select_best_fit <- function(ranking, fits) {

  # Always use AIC rank 1 - transparent, consistent, and matches the ranking
  # table the user sees.  The previous [U+0394]AIC < 2 / AD_p tiebreaker caused
  # confusing mismatches between the table (rank 1) and the plot title.
  best_key    <- ranking$dist_key[1]
  best_method <- ranking$Method[1]

  match_key <- paste0(best_key, "_", best_method)
  if (!is.null(fits[[match_key]])) return(fits[[match_key]])

  # Fallback: iterate in case key format differs
  for (f in fits) {
    if (f$dist_key == best_key && f$method == best_method) return(f)
  }

  fits[[1]]   # ultimate fallback
}


# -- Name-key builder for multi-method fit lists -------------------------------

#' Create a unique key for a fit result: "dist_key_METHOD"
#' @keywords internal
fit_list_key <- function(dist_key, method) {
  paste0(dist_key, "_", toupper(method))
}

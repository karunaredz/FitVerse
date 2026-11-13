# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : tests/testthat/test_fitverse.R
# Description: Unit tests for the FitVerse package.
#
#              Tests cover:
#               1. Distribution registry completeness
#               2. MLE fitting on simulated data (Normal, Gamma, Weibull)
#               3. MOM fitting on simulated data
#               4. L-Moment fitting on simulated data
#               5. Goodness-of-fit test output structure
#               6. compare_dists() output structure
#               7. S3 methods (coef, logLik, AIC, BIC)
#               8. Data-cleaning and error handling
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

library(testthat)
library(FitVerse)

set.seed(2026)

# Simulated datasets used across tests
x_norm    <- rnorm(300, mean = 5,    sd   = 1.5)
x_gamma   <- rgamma(300, shape = 2,  rate = 0.5)
x_weibull <- rweibull(300, shape = 2, scale = 3)
x_lnorm   <- rlnorm(300, meanlog = 1, sdlog = 0.4)


# -- 1. Registry --------------------------------------------------------------

test_that("fitverse_distributions() returns a data frame with 52 rows", {
  df <- fitverse_distributions()
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 52L)
  expect_true(all(c("Key", "Name", "Family", "MLE", "MOM", "LMOM") %in% names(df)))
})

test_that("get_dist_entry() returns correct structure for known key", {
  e <- FitVerse:::get_dist_entry("normal")
  expect_equal(e$r_name, "norm")
  expect_true(e$supports_mle)
  expect_true(e$supports_mom)
  expect_true(e$supports_lmom)
})

test_that("get_dist_entry() throws error for unknown key", {
  expect_error(FitVerse:::get_dist_entry("unicorn"), "Unknown distribution key")
})


# -- 2. MLE -------------------------------------------------------------------

test_that("MLE fits Normal correctly: parameters near true values", {
  fit <- fitverse(x_norm, dists = "normal", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  params <- coef(fit)
  expect_named(params, c("mean", "sd"))
  expect_equal(unname(params["mean"]), 5, tolerance = 0.3)
  expect_equal(unname(params["sd"]),   1.5, tolerance = 0.3)
})

test_that("MLE fits Gamma correctly: parameters near true values", {
  fit <- fitverse(x_gamma, dists = "gamma", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  params <- coef(fit)
  expect_named(params, c("shape", "rate"))
  expect_equal(unname(params["shape"]), 2,   tolerance = 0.5)
  expect_equal(unname(params["rate"]),  0.5, tolerance = 0.2)
})

test_that("MLE fits Weibull (2-Parameter) correctly: shape near 2", {
  fit <- fitverse(x_weibull, dists = "weibull", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  params <- coef(fit)
  expect_equal(unname(params["shape"]), 2, tolerance = 0.4)
})

test_that("MLE returns a fitverse S3 object", {
  fit <- fitverse(x_norm, dists = "normal", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_true(all(c("best_fit", "all_fits", "ranking", "data", "n") %in% names(fit)))
})

test_that("MLE ranking table has correct structure", {
  fit <- fitverse(x_norm, dists = c("normal", "logistic", "cauchy"),
                  method = "MLE", plot = FALSE, verbose = FALSE)
  tbl <- fit$ranking
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("Rank", "Distribution", "AIC", "BIC") %in% names(tbl)))
  expect_equal(tbl$Rank[1], 1L)
  # Normal should win on Normal data
  expect_equal(tbl$dist_key[1], "normal")
})


# -- 3. MOM -------------------------------------------------------------------

test_that("MOM fits Normal: parameters close to MLE", {
  fit_mle <- fitverse(x_norm, dists = "normal", method = "MLE",
                      plot = FALSE, verbose = FALSE)
  fit_mom <- fitverse(x_norm, dists = "normal", method = "MOM",
                      plot = FALSE, verbose = FALSE)
  expect_equal(coef(fit_mle)["mean"], coef(fit_mom)["mean"], tolerance = 0.01)
})

test_that("MOM fits Gamma: shape parameter close to MLE", {
  fit_mle <- fitverse(x_gamma, dists = "gamma", method = "MLE",
                      plot = FALSE, verbose = FALSE)
  fit_mom <- fitverse(x_gamma, dists = "gamma", method = "MOM",
                      plot = FALSE, verbose = FALSE)
  expect_equal(unname(coef(fit_mle)["shape"]),
               unname(coef(fit_mom)["shape"]), tolerance = 0.5)
})


# -- 4. L-Moments -------------------------------------------------------------

test_that("LMOM fits Normal: mean parameter close to true value", {
  skip_if_not_installed("lmomco")
  fit <- fitverse(x_norm, dists = "normal", method = "LMOM",
                  plot = FALSE, verbose = FALSE)
  expect_equal(unname(coef(fit)["mean"]), 5, tolerance = 0.3)
})

test_that("LMOM fits Gamma: shape parameter reasonable", {
  skip_if_not_installed("lmomco")
  fit <- fitverse(x_gamma, dists = "gamma", method = "LMOM",
                  plot = FALSE, verbose = FALSE)
  params <- coef(fit)
  expect_true(params["shape"] > 0)
})


# -- 5. Goodness-of-fit --------------------------------------------------------

test_that("GoF output has four rows with correct test names", {
  fit <- fitverse(x_norm, dists = "normal", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  gof <- fit$best_fit$gof
  expect_s3_class(gof, "data.frame")
  expect_equal(nrow(gof), 4L)
  expect_true("Kolmogorov-Smirnov" %in% gof$Test)
  expect_true("Anderson-Darling"   %in% gof$Test)
  expect_true("Cramer-von Mises"   %in% gof$Test)
  expect_true("Chi-Squared"        %in% gof$Test)
})

test_that("GoF p-value for correctly specified Normal is > 0.05", {
  fit <- fitverse(x_norm, dists = "normal", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  ks_p <- FitVerse:::get_ks_pvalue(fit$best_fit)
  expect_true(is.numeric(ks_p))
  expect_gt(ks_p, 0.05)
})


# -- 6. compare_dists() -------------------------------------------------------

test_that("compare_dists() returns a data frame sorted by AIC", {
  tbl <- compare_dists(x_lnorm,
                       dists   = c("lognormal", "gamma", "weibull"),
                       method  = "MLE",
                       verbose = FALSE)
  expect_s3_class(tbl, "data.frame")
  expect_equal(tbl$Rank[1], 1L)
  # AIC should be non-decreasing
  expect_true(all(diff(tbl$AIC) >= 0))
  # Log-Normal should win on log-normal data
  expect_equal(tbl$dist_key[1], "lognormal")
})


# -- 7. S3 generics -----------------------------------------------------------

test_that("coef() returns named numeric vector", {
  fit <- fitverse(x_norm, dists = "normal", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  params <- coef(fit)
  expect_true(is.numeric(params))
  expect_true(!is.null(names(params)))
})

test_that("logLik() returns a logLik object", {
  fit <- fitverse(x_norm, dists = "normal", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  ll <- logLik(fit)
  expect_s3_class(ll, "logLik")
  expect_true(is.finite(as.numeric(ll)))
})

test_that("AIC() and BIC() return finite numerics", {
  fit <- fitverse(x_norm, dists = "normal", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  expect_true(is.finite(AIC(fit)))
  expect_true(is.finite(BIC(fit)))
})


# -- 8. Error handling ---------------------------------------------------------

test_that("fitverse() stops on non-numeric input", {
  expect_error(fitverse(c("a", "b", "c"), plot = FALSE, verbose = FALSE),
               "numeric")
})

test_that("fitverse() stops on too-few observations", {
  expect_error(fitverse(c(1, 2, 3), plot = FALSE, verbose = FALSE),
               "4 finite")
})

test_that("fitverse() stops on unknown distribution key", {
  expect_error(fitverse(x_norm, dists = "unicorn",
                        plot = FALSE, verbose = FALSE),
               "Unknown distribution key")
})

test_that("fitverse() removes NAs silently and fits on clean data", {
  x_with_na <- c(x_norm, NA, NA, Inf, -Inf)
  # Should not error; should warn about removed values
  expect_message(
    fit <- fitverse(x_with_na, dists = "normal", method = "MLE",
                    plot = FALSE, verbose = FALSE),
    "non-finite"
  )
  expect_s3_class(fit, "fitverse")
})

test_that("Positive-only distribution skipped on mixed-sign data", {
  x_mixed <- c(-1, -0.5, rnorm(100, mean = 2, sd = 1))
  fit <- fitverse(x_mixed,
                  dists   = c("normal", "gamma"),   # gamma requires positive data
                  method  = "MLE",
                  plot    = FALSE,
                  verbose = FALSE)
  # Only 'normal' should appear in the ranking
  expect_true(all(fit$ranking$dist_key == "normal"))
})


# -- 9. Registry count --------------------------------------------------------

test_that("fitverse_distributions() has 52 rows and contains expected keys", {
  df <- fitverse_distributions()
  expect_equal(nrow(df), 52L)
  expected_keys <- c("gpd", "pearson3", "lpearson3", "glo",
                     "normal", "weibull", "gamma", "gev", "beta",
                     "invgaussian", "gengamma", "gompertz", "birnbaum_saunders",
                     "rayleigh", "halfnormal", "expweibull", "gamma3", "lnorm3")
  expect_true(all(expected_keys %in% df$Key))
})


# -- 10. New distributions - GPD -----------------------------------------------

set.seed(2026)
x_gpd <- FitVerse:::.qgpd_internal(runif(300), loc = 0, scale = 1, shape = 0.2)

test_that("GPD MLE: scale and shape near true values", {
  skip_if_not_installed("lmomco")
  fit <- fitverse(x_gpd, dists = "gpd", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  params <- coef(fit)
  expect_named(params, c("loc", "scale", "shape"))
  expect_equal(unname(params["scale"]), 1,   tolerance = 0.3)
  expect_equal(unname(params["shape"]), 0.2, tolerance = 0.2)
})

test_that("GPD LMOM: converges and returns finite parameters", {
  skip_if_not_installed("lmomco")
  fit <- fitverse(x_gpd, dists = "gpd", method = "LMOM",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_true(all(is.finite(coef(fit))))
})

test_that("GPD MOM: scale > 0 and shape < 0.5", {
  fit <- fitverse(x_gpd, dists = "gpd", method = "MOM",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  params <- coef(fit)
  expect_gt(unname(params["scale"]), 0)
  expect_lt(unname(params["shape"]), 0.5)
})

test_that("GPD return levels are non-decreasing", {
  fit <- fitverse(x_gpd, dists = "gpd", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  rl <- return_level(fit, return_periods = c(2, 10, 50, 100))
  expect_equal(nrow(rl), 4L)
  expect_true(all(diff(rl$return_level) > 0))
})


# -- 11. New distributions - Pearson III ---------------------------------------

set.seed(2026)
x_p3 <- FitVerse:::.qpearson3_internal(runif(300), shape = 4, scale = 2, loc = 5)

test_that("Pearson III MLE: shape and scale near true values", {
  fit <- fitverse(x_p3, dists = "pearson3", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  params <- coef(fit)
  expect_named(params, c("shape", "scale", "loc"))
  expect_equal(unname(params["shape"]), 4, tolerance = 1.5)
})

test_that("Pearson III MOM: shape > 0", {
  fit <- fitverse(x_p3, dists = "pearson3", method = "MOM",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_gt(unname(coef(fit)["shape"]), 0)
})

test_that("Pearson III LMOM: converges and parameters finite", {
  skip_if_not_installed("lmomco")
  fit <- fitverse(x_p3, dists = "pearson3", method = "LMOM",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_true(all(is.finite(coef(fit))))
})


# -- 12. New distributions - Log-Pearson III -----------------------------------

set.seed(2026)
x_lp3 <- FitVerse:::.qlpearson3_internal(runif(300), shape = 4, scale = 0.5, loc = 1)

test_that("Log-Pearson III MLE: all params finite, x is positive", {
  expect_true(all(x_lp3 > 0))
  fit <- fitverse(x_lp3, dists = "lpearson3", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_true(all(is.finite(coef(fit))))
})

test_that("Log-Pearson III LMOM: converges", {
  skip_if_not_installed("lmomco")
  fit <- fitverse(x_lp3, dists = "lpearson3", method = "LMOM",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_true(all(is.finite(coef(fit))))
})


# -- 13. New distributions - GLO -----------------------------------------------

set.seed(2026)
x_glo <- FitVerse:::.qglo_internal(runif(300), loc = 10, scale = 2, shape = 0.15)

test_that("GLO MLE: converges and scale > 0", {
  fit <- fitverse(x_glo, dists = "glo", method = "MLE",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_gt(unname(coef(fit)["scale"]), 0)
})

test_that("GLO LMOM: converges and parameters finite", {
  skip_if_not_installed("lmomco")
  fit <- fitverse(x_glo, dists = "glo", method = "LMOM",
                  plot = FALSE, verbose = FALSE)
  expect_s3_class(fit, "fitverse")
  expect_true(all(is.finite(coef(fit))))
})


# -- 14. Internal d/p/q/r - round-trip tests ----------------------------------

test_that("GPD q/p round-trip: qgpd(pgpd(x)) == x", {
  x_rt  <- seq(0.5, 5, by = 0.5)
  p_rt  <- FitVerse:::.pgpd_internal(x_rt, loc = 0, scale = 2, shape = 0.1)
  x_rt2 <- FitVerse:::.qgpd_internal(p_rt, loc = 0, scale = 2, shape = 0.1)
  expect_equal(x_rt, x_rt2, tolerance = 1e-9)
})

test_that("Pearson III q/p round-trip: qpearson3(ppearson3(x)) == x", {
  x_rt  <- seq(6, 14, by = 0.5)
  p_rt  <- FitVerse:::.ppearson3_internal(x_rt, shape = 4, scale = 2, loc = 5)
  x_rt2 <- FitVerse:::.qpearson3_internal(p_rt, shape = 4, scale = 2, loc = 5)
  expect_equal(x_rt, x_rt2, tolerance = 1e-6)
})

test_that("LP3 q/p round-trip: qlpearson3(plpearson3(x)) == x", {
  x_rt  <- exp(seq(1.5, 3.5, by = 0.2))
  p_rt  <- FitVerse:::.plpearson3_internal(x_rt, shape = 4, scale = 0.5, loc = 1)
  x_rt2 <- FitVerse:::.qlpearson3_internal(p_rt, shape = 4, scale = 0.5, loc = 1)
  expect_equal(x_rt, x_rt2, tolerance = 1e-6)
})

test_that("GLO q/p round-trip: qglo(pglo(x)) == x", {
  x_rt  <- seq(6, 16, by = 0.5)
  p_rt  <- FitVerse:::.pglo_internal(x_rt, loc = 10, scale = 2, shape = 0.15)
  x_rt2 <- FitVerse:::.qglo_internal(p_rt, loc = 10, scale = 2, shape = 0.15)
  expect_equal(x_rt, x_rt2, tolerance = 1e-9)
})

test_that("GPD density integrates to ~1", {
  integrand <- function(x) FitVerse:::.dgpd_internal(x, loc = 0, scale = 1, shape = 0.2)
  integral  <- stats::integrate(integrand, lower = 0, upper = 1e3)$value
  expect_equal(integral, 1, tolerance = 0.01)
})

test_that("Pearson III density integrates to ~1 (positive scale)", {
  integrand <- function(x) FitVerse:::.dpearson3_internal(x, shape = 3, scale = 1, loc = 0)
  integral  <- stats::integrate(integrand, lower = 1e-6, upper = 100)$value
  expect_equal(integral, 1, tolerance = 0.01)
})


# -- 15. fitverse_censored() ---------------------------------------------------

# Simulated datasets for censored tests
set.seed(2026)
x_surv   <- rweibull(120, shape = 1.8, scale = 50)
cens_t   <- runif(120, min = 20, max = 80)
status_rc <- ifelse(x_surv <= cens_t, 1L, 0L)
x_rc     <- pmin(x_surv, cens_t)          # right-censored survival data

x_env    <- rlnorm(80, meanlog = 1.5, sdlog = 0.7)
dl       <- 3.0
status_lc <- ifelse(x_env >= dl, 1L, 2L)
x_lc     <- ifelse(x_env >= dl, x_env, dl)  # left-censored env data

x_trunc  <- rlnorm(200, meanlog = 2.5, sdlog = 0.5)
x_trunc  <- x_trunc[x_trunc > 5]           # left-truncated: only values > 5


# -- 15a. Output structure -----------------------------------------------------

test_that("fitverse_censored() returns a fitverse_censored S3 object", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = c("weibull", "lognormal"),
                           draw = FALSE)
  expect_s3_class(res, "fitverse_censored")
  expect_true(all(c("fits", "rankings", "x", "status",
                    "criterion", "xlab") %in% names(res)))
})

test_that("fitverse_censored() rankings is a data frame sorted by criterion", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = c("weibull", "lognormal", "gamma"),
                           criterion = "AIC", draw = FALSE)
  tbl <- res$rankings
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("Rank", "Distribution", "LogLik", "AIC", "BIC") %in% names(tbl)))
  expect_equal(tbl$Rank[1], 1L)
  expect_true(all(diff(tbl$AIC) >= 0))  # non-decreasing AIC
})

test_that("fitverse_censored() x and status vectors are consistent", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = "weibull", draw = FALSE)
  expect_equal(length(res$x),              length(x_rc))
  expect_equal(sum(res$status == 1L),  sum(status_rc == 1L))
  expect_equal(sum(res$status == 0L),  sum(status_rc == 0L))
})


# -- 15b. Right-censored data --------------------------------------------------

test_that("Right-censored fit: Weibull parameters positive and finite", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = "weibull", draw = FALSE)
  params <- res$fits[[1L]]$par
  expect_true(all(is.finite(params)))
  expect_true(all(params > 0))
})

test_that("Right-censored fit: log-likelihood is finite and negative", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = "weibull", draw = FALSE)
  ll <- res$fits[[1L]]$log_lik
  expect_true(is.finite(ll))
  expect_lt(ll, 0)
})

test_that("Right-censored: Weibull ranks above Exponential on Weibull data", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = c("weibull", "exponential"),
                           criterion = "AIC", draw = FALSE)
  expect_equal(res$fits[[1L]]$dist_key, "weibull")
})


# -- 15c. Left-censored data ---------------------------------------------------

test_that("Left-censored fit: Log-Normal parameters finite", {
  res <- fitverse_censored(x_lc, status = status_lc,
                           dists = c("lognormal", "gamma"),
                           draw = FALSE)
  fit_keys <- vapply(res$fits, `[[`, character(1), "dist_key")
  ln_fit   <- res$fits[[which(fit_keys == "lognormal")]]
  expect_true(all(is.finite(ln_fit$par)))
})

test_that("Left-censored fit: status vector retains below-DL coding", {
  res <- fitverse_censored(x_lc, status = status_lc,
                           dists = "lognormal", draw = FALSE)
  expect_equal(sum(res$status == 2L), sum(status_lc == 2L))
})


# -- 15d. Left-truncated data --------------------------------------------------

test_that("Left-truncated fit: converges and AIC is finite", {
  res <- fitverse_censored(x_trunc,
                           dists = c("lognormal", "gamma"),
                           trunc_lower = 5,
                           draw = FALSE)
  expect_true(is.finite(res$rankings$AIC[1]))
})

test_that("Left-truncated fit: Log-Normal params near true values", {
  res <- fitverse_censored(x_trunc,
                           dists = "lognormal",
                           trunc_lower = 5,
                           draw = FALSE)
  params <- res$fits[[1L]]$par   # lognormal is the only dist, so rank 1
  # meanlog ~ 2.5, sdlog ~ 0.5 (with tolerance for truncation and n)
  expect_equal(unname(params["meanlog"]), 2.5, tolerance = 0.3)
  expect_equal(unname(params["sdlog"]),   0.5, tolerance = 0.2)
})


# -- 15e. Interval-censored data -----------------------------------------------

test_that("Interval-censored fit: converges with x_upper provided", {
  set.seed(42)
  x_int     <- rlnorm(60, meanlog = 2, sdlog = 0.5)
  x_lo      <- floor(x_int)
  x_hi      <- x_lo + 1
  status_ic <- rep(3L, 60)
  res <- fitverse_censored(x_lo, status = status_ic, x_upper = x_hi,
                           dists = "lognormal", draw = FALSE)
  expect_s3_class(res, "fitverse_censored")
  expect_true(all(is.finite(res$fits[["lognormal"]]$params)))
})


# -- 15f. Degenerate / error cases --------------------------------------------

test_that("fitverse_censored() errors when all observations are censored", {
  x_all_cens    <- rweibull(50, shape = 2, scale = 30)
  status_all_cens <- rep(0L, 50)   # all right-censored - no events
  expect_error(
    fitverse_censored(x_all_cens, status = status_all_cens,
                      dists = "weibull", draw = FALSE),
    "No completely observed or interval-censored events"
  )
})

test_that("fitverse_censored() errors on mismatched status length", {
  expect_error(
    fitverse_censored(x_rc, status = rep(1L, 10), dists = "weibull", draw = FALSE),
    "same length"
  )
})

test_that("fitverse_censored() errors on invalid status values", {
  bad_status <- sample(c(1L, 99L), length(x_rc), replace = TRUE)
  expect_error(
    fitverse_censored(x_rc, status = bad_status, dists = "weibull", draw = FALSE),
    "status.*values"
  )
})


# -- 15g. S3 methods ----------------------------------------------------------

test_that("print.fitverse_censored() runs without error", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = c("weibull", "lognormal"),
                           draw = FALSE)
  expect_output(print(res), "Censored")
})

test_that("plot.fitverse_censored() returns a ggplot object", {
  res <- fitverse_censored(x_rc, status = status_rc,
                           dists = c("weibull", "lognormal"),
                           draw = FALSE)
  p <- plot(res, top_n = 2)
  expect_s3_class(p, "gg")
})

\name{fitverse}
\alias{fitverse}
\title{Fit Probability Distributions to Univariate Continuous Data}

\description{
The primary function of the \pkg{FitVerse} package.  Given a numeric data
vector, \code{fitverse()} fits one or more parametric probability distributions,
ranks them by an information criterion, selects the best-fitting model,
computes goodness-of-fit statistics, and optionally produces a
publication-quality diagnostic plot.

Three estimation methods are supported: Maximum Likelihood Estimation
(\code{"MLE"}), Method of Moments (\code{"MOM"}), and L-Moments
(\code{"LMOM"}).  When multiple methods are requested, all results are
combined into a single ranking table and the global best model is returned.

\strong{Tip:} For a point-and-click experience, launch the built-in
interactive Shiny application with \code{\link{FitVerseApp}()}.
}

\usage{
fitverse(x,
         dists       = "all",
         method      = "MLE",
         criterion   = "AIC",
         xlab        = "x",
         plot        = TRUE,
         interactive = FALSE,
         top_n       = 3L,
         progress    = NULL,
         verbose     = TRUE,
         \dots)
}

\arguments{
  \item{x}{
    A numeric vector of observations.  At least 4 finite values are required.
    Non-finite values (\code{NA}, \code{NaN}, \code{Inf}, \code{-Inf}) are
    removed automatically before fitting; a message is printed if any are found.
  }

  \item{dists}{
    Character vector of distribution keys to try, or \code{"all"} (default)
    to attempt every distribution whose support is compatible with the data.
    Use \code{\link{fitverse_distributions}()} to see all valid keys.
    Example: \code{dists = c("gamma", "lognormal", "weibull")}.
  }

  \item{method}{
    Character vector specifying which estimation method(s) to use.
    One or more of:
    \describe{
      \item{\code{"MLE"}}{Maximum Likelihood Estimation via numerical
        optimisation (\pkg{fitdistrplus} or custom \code{stats::optim}).
        Available for all 52 distributions.}
      \item{\code{"MOM"}}{Method of Moments.  Uses closed-form expressions
        where available; numerical solution otherwise.  Available for most
        of the 52 distributions.}
      \item{\code{"LMOM"}}{L-Moment estimation via \pkg{lmomco}.
        Available for distributions with L-moment parameter estimators.}
    }
    When multiple methods are supplied (e.g. \code{method = c("MLE", "MOM")}),
    all fits are combined and ranked together.  Default: \code{"MLE"}.
  }

  \item{criterion}{
    Character: the information criterion used for ranking and best-model
    selection.  Either \code{"AIC"} (default) or \code{"BIC"}.

    AIC is recommended for exploratory analysis and prediction; BIC imposes
    a stronger penalty for model complexity and is preferred when the goal
    is identifying the true generating distribution.
  }

  \item{xlab}{
    Character: a label for the data variable used in plot axis titles and
    the auto-generated report.  Default \code{"x"}.
    Example: \code{xlab = "Annual maximum flood (m^3/s)"}.
  }

  \item{plot}{
    Logical: should a diagnostic plot be produced and printed?
    Default \code{TRUE}.  Set to \code{FALSE} for batch workflows or when
    embedding in scripts.  The plot can always be regenerated later with
    \code{\link{plot.fitverse}()}.
  }

  \item{interactive}{
    Logical: if \code{TRUE} and the \pkg{plotly} package is installed,
    produces an interactive HTML figure instead of a static ggplot2 image.
    Default \code{FALSE}.
  }

  \item{top_n}{
    Integer: number of top-ranked distributions to overlay on the density
    panel of the diagnostic plot.  The best-fit model is always drawn as a
    solid line; the remaining \code{top_n - 1} are drawn as dashed lines.
    Default \code{3}.  Range: 1 to \code{nrow(ranking)}.
  }

  \item{progress}{
    Optional function of the form \code{function(p)} where \code{p} is a
    numeric value in \eqn{[0, 1]} indicating the proportion of work completed.
    Called at several points during fitting.  Useful for updating a
    \pkg{shiny} progress bar.  Default \code{NULL} (no callback).
    Example: \code{progress = function(p) shiny::setProgress(p)}.
  }

  \item{verbose}{
    Logical: print progress messages to the console?  Default \code{TRUE}.
    Set to \code{FALSE} in batch workflows or when running inside \pkg{shiny}.
  }

  \item{\dots}{
    Additional arguments passed to \code{\link{plot.fitverse}()}.
  }
}

\details{
\subsection{Workflow}{
\code{fitverse()} executes the following steps:
\enumerate{
  \item \strong{Validate and clean} -- removes non-finite values; checks
    that at least 4 observations remain.
  \item \strong{Filter distributions} -- distributions whose support is
    incompatible with the data (e.g. positive-only Gamma when data contains
    negative values) are silently excluded.
  \item \strong{Fit} -- calls \code{fit_mle_all()}, \code{fit_mom_all()},
    and/or \code{fit_lmom_all()} according to \code{method}.
  \item \strong{Goodness-of-fit} -- attaches Kolmogorov-Smirnov (KS),
    Anderson-Darling (AD), and Cramer-von Mises (CvM) test results to each
    fit result.
  \item \strong{Rank} -- builds a table ordered by \code{criterion}
    (ascending AIC or BIC).
  \item \strong{Select} -- chooses the model with the lowest AIC (or BIC).
  \item \strong{Summarise} -- computes \code{\link{compute_data_summary}()}
    and stores it in the returned object.
  \item \strong{Plot} (optional) -- calls \code{\link{plot.fitverse}()}.
}
}

\subsection{Estimation methods}{
\describe{
  \item{MLE}{
    Maximises the log-likelihood
    \eqn{\ell(\boldsymbol\theta) = \sum_{i=1}^n \log f(x_i; \boldsymbol\theta)}.
    Implemented via \code{fitdistrplus::fitdist()} with bounded L-BFGS-B
    optimisation where parameter constraints exist.
  }
  \item{MOM}{
    Equates the first \eqn{k} sample moments to their theoretical counterparts
    and solves for the \eqn{k} parameters.  Closed-form solutions are used
    where available (Normal, Gamma, Log-Normal, Exponential, Beta, Logistic,
    Uniform, Gumbel, Pareto, Inverse Gamma); numerical solution via
    \code{stats::optimise()} for Weibull and Log-Logistic.
    \pkg{fitdistrplus} MME is used for standard \pkg{stats} distributions.
  }
  \item{LMOM}{
    Equates sample L-moments (linear combinations of order statistics) to
    their theoretical counterparts.  L-moments are more robust to outliers
    than conventional moments and are widely used in hydrology.
    Implemented via \pkg{lmomco}; available for Normal, Gamma, Exponential,
    Log-Normal, Weibull, GEV, GLO, Gumbel, Logistic, and Log-Logistic.
    Reference: Hosking (1990).
  }
}
}

\subsection{Model selection}{
The best-fit model is the one with the lowest AIC (or BIC) across all
fitted distributions and methods.  Delta AIC values in the ranking table
quantify evidence against each model relative to the best: models with
Delta AIC < 2 have "substantial support", 4--7 "considerably less", and
> 10 "essentially none" (Burnham and Anderson, 2002).
}

\subsection{Goodness-of-fit tests}{
Three non-parametric tests are applied to the best-fit model:
\describe{
  \item{Kolmogorov-Smirnov (KS)}{Measures the maximum absolute difference
    between the empirical and fitted CDFs.  Conservative; not recommended
    as the primary criterion.}
  \item{Anderson-Darling (AD)}{Gives extra weight to the distribution tails.
    More powerful than KS for detecting tail departures.  Implemented via
    \pkg{goftest}.}
  \item{Cramer-von Mises (CvM)}{Integrates the squared difference between
    empirical and fitted CDFs.  Implemented via \pkg{goftest}.}
}
A large p-value (> 0.05) indicates insufficient evidence to reject the
fitted distribution.
}
}

\value{
An S3 object of class \code{"fitverse"}.  It is a named list with the
following elements:

\describe{
  \item{\code{best_fit}}{
    Named list describing the best-fitting model, with elements:
    \describe{
      \item{\code{dist_key}}{Character: distribution key (e.g. \code{"gamma"}).}
      \item{\code{method}}{Character: estimation method (\code{"MLE"}, \code{"MOM"}, or \code{"LMOM"}).}
      \item{\code{params}}{Named numeric vector of parameter estimates.}
      \item{\code{loglik}}{Numeric: maximised log-likelihood.}
      \item{\code{aic}}{Numeric: Akaike Information Criterion.}
      \item{\code{bic}}{Numeric: Bayesian Information Criterion.}
      \item{\code{n_params}}{Integer: number of estimated parameters.}
      \item{\code{n}}{Integer: sample size used.}
      \item{\code{converged}}{Logical: did the optimiser converge?}
      \item{\code{gof}}{Data frame of goodness-of-fit test results (Test,
        Statistic, P_value, Stars), or \code{NULL} if unavailable.}
    }
  }
  \item{\code{all_fits}}{
    Named list of all fit-result lists, one per distribution-method
    combination.  Keys are of the form \code{"distkey_METHOD"}
    (e.g. \code{"gamma_MLE"}).
  }
  \item{\code{ranking}}{
    Data frame with columns: Rank, Distribution, Method, N_params, LogLik,
    AIC, Delta_AIC, BIC, Delta_BIC, KS_p, AD_p, Converged, dist_key.
    Ordered by the chosen \code{criterion} (ascending).
  }
  \item{\code{data}}{Numeric vector: the cleaned input data.}
  \item{\code{n}}{Integer: number of finite observations used.}
  \item{\code{xlab}}{Character: the variable label.}
  \item{\code{data_summary}}{
    Named list from \code{\link{compute_data_summary}()}: n, n_missing,
    mean, sd, cv, median, iqr, skewness, kurtosis, min, max, range, q25, q75.
  }
  \item{\code{criterion}}{Character: \code{"AIC"} or \code{"BIC"}.}
  \item{\code{method}}{Character vector: method(s) used.}
  \item{\code{call}}{The matched function call.}
}

The object is returned invisibly.  Use \code{print()}, \code{summary()},
\code{plot()}, \code{coef()}, \code{logLik()}, \code{AIC()}, and \code{BIC()}
for standard S3 method access.
}

\references{
Burnham, K. P. and Anderson, D. R. (2002).
\emph{Model Selection and Multimodel Inference} (2nd ed.). Springer.

Delignette-Muller, M. L. and Dutang, C. (2015).
fitdistrplus: An R package for fitting distributions.
\emph{Journal of Statistical Software}, \bold{64}(4), 1--34.
\doi{10.18637/jss.v064.i04}

Hosking, J. R. M. (1990). L-moments: Analysis and estimation of
distributions using linear combinations of order statistics.
\emph{Journal of the Royal Statistical Society, Series B}, \bold{52}(1), 105--124.

Akaike, H. (1974). A new look at the statistical model identification.
\emph{IEEE Transactions on Automatic Control}, \bold{19}(6), 716--723.
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\seealso{
\code{\link{compare_dists}} for a ranking table only (no plot). \cr
\code{\link{fitverse_distributions}} to list all supported distributions. \cr
\code{\link{plot.fitverse}}, \code{\link{summary.fitverse}},
\code{\link{bootstrap_ci}}, \code{\link{return_level}},
\code{\link{generate_report}}.
}

\examples{
## -----------------------------------------------------------
## Example 1: Basic MLE fit -- three distributions
## -----------------------------------------------------------
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)

fit <- fitverse(x,
                dists   = c("gamma", "lognormal", "weibull"),
                method  = "MLE",
                xlab    = "Waiting time (s)",
                plot    = FALSE,
                verbose = FALSE)

print(fit)
coef(fit)        # parameter estimates
AIC(fit)         # AIC of the best-fit model
logLik(fit)      # log-likelihood

## -----------------------------------------------------------
## Example 2: All three estimation methods, ranked by BIC
## -----------------------------------------------------------
\donttest{
fit2 <- fitverse(x,
                 method    = c("MLE", "MOM", "LMOM"),
                 criterion = "BIC",
                 plot      = FALSE,
                 verbose   = FALSE)
summary(fit2)
}

## -----------------------------------------------------------
## Example 3: Automatic best-fit selection across all distributions
## -----------------------------------------------------------
\donttest{
set.seed(7)
x3 <- rlnorm(500, meanlog = 1.5, sdlog = 0.6)

fit3 <- fitverse(x3,
                 xlab    = "Insurance claim size",
                 method  = "MLE",
                 plot    = FALSE,
                 verbose = FALSE)
fit3$ranking[1:5, c("Distribution","Method","AIC","Delta_AIC","AD_p")]
}

## -----------------------------------------------------------
## Example 4: Accessing the data summary
## -----------------------------------------------------------
\donttest{
fit4 <- fitverse(x, plot = FALSE, verbose = FALSE)
fit4$data_summary$mean
fit4$data_summary$skewness
}
}

\keyword{distribution}
\keyword{univar}
\keyword{models}

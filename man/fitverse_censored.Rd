\name{fitverse_censored}
\alias{fitverse_censored}
\alias{print.fitverse_censored}
\alias{plot.fitverse_censored}

\title{Fit Parametric Distributions to Censored or Truncated Data}

\description{
Estimates distribution parameters by maximising the censored log-likelihood
for data that may be right-censored, left-censored, interval-censored,
and/or left-/right-truncated, or any combination of these.  Seven
distribution families are supported.  Fits are ranked by AIC or BIC, and a
nonparametric Kaplan--Meier estimate is overlaid automatically on the
survival or CDF plot.
}

\usage{
fitverse_censored(x,
                  status      = NULL,
                  x_upper     = NULL,
                  trunc_lower = -Inf,
                  trunc_upper =  Inf,
                  dists       = c("normal", "lognormal", "weibull",
                                  "exponential", "gamma",
                                  "loglogistic", "gumbel"),
                  criterion   = c("AIC", "BIC"),
                  n_starts    = 3L,
                  xlab        = "x",
                  draw        = TRUE)

\method{print}{fitverse_censored}(x, \dots)

\method{plot}{fitverse_censored}(x, top_n = 3L, \dots)
}

\arguments{
  \item{x}{
    Numeric vector of observed or censored values.  For right-censored
    observations this is the last observed time; for left-censored
    observations it is the detection-limit value; for interval-censored
    observations it is the lower bound of the interval.
    In \code{print} and \code{plot} methods, \code{x} is the
    \code{"fitverse_censored"} object.
  }
  \item{status}{
    Integer vector of the same length as \code{x} encoding the observation
    type using the \pkg{survival}-package convention:
    \describe{
      \item{\code{1}}{Completely observed (event occurred).}
      \item{\code{0}}{Right-censored (event had not yet occurred at last follow-up).}
      \item{\code{2}}{Left-censored (true value is below a detection limit).}
      \item{\code{3}}{Interval-censored (true value lies in \code{[x, x_upper]}).}
    }
    If \code{NULL} (default), all observations are treated as completely
    observed (\code{status = 1}).
  }
  \item{x_upper}{
    Numeric vector of upper bounds for interval-censored observations
    (\code{status == 3}).  Must be the same length as \code{x}.  Ignored
    for all rows where \code{status != 3}.
  }
  \item{trunc_lower}{
    Numeric scalar: left (lower) truncation bound.  Observations below this
    value are never recorded; the likelihood is normalised by
    \eqn{P(X > \texttt{trunc\_lower})}.
    Default \code{-Inf} (no left truncation).
  }
  \item{trunc_upper}{
    Numeric scalar: right (upper) truncation bound.  Default \code{Inf}
    (no right truncation).
  }
  \item{dists}{
    Character vector of distribution keys to fit.  Any subset of
    \code{c("normal", "lognormal", "weibull", "exponential", "gamma",
    "loglogistic", "gumbel")}.  Default: all seven.
  }
  \item{criterion}{
    Character: \code{"AIC"} (default) or \code{"BIC"}.  Used to rank the
    fitted distributions.
  }
  \item{n_starts}{
    Positive integer: number of random starting points per distribution.
    Increasing this reduces the risk of the optimiser converging to a local
    optimum.  Default \code{3}.
  }
  \item{xlab}{
    Character: variable label used in plot axis titles.  Default \code{"x"}.
  }
  \item{draw}{
    Logical: produce a survival-function (right-censored) or
    cumulative-distribution (otherwise) plot?  Default \code{TRUE}.
  }
  \item{top_n}{
    Positive integer: number of top-ranked distributions to overlay on the
    plot.  Default \code{3}.
  }
  \item{\dots}{Further arguments passed to or from other methods.}
}

\value{
An object of class \code{"fitverse_censored"}, which is a named list with
the following elements:

\describe{
  \item{\code{fits}}{
    List of fit objects ordered by \code{criterion} (best first).  Each
    element is a list with: \code{dist_key} (character), \code{display}
    (character display name), \code{par} (named numeric vector of parameter
    estimates), \code{log_lik} (numeric), \code{aic} (numeric),
    \code{bic} (numeric), \code{n_par} (integer), \code{converged}
    (logical).
  }
  \item{\code{rankings}}{
    Data frame with columns \code{Rank}, \code{Distribution},
    \code{LogLik}, \code{AIC}, \code{BIC}, \code{Converged}, sorted by
    \code{criterion}.
  }
  \item{\code{x}, \code{status}, \code{x_upper}}{
    Cleaned copies of the input data vectors (non-finite values removed).
  }
  \item{\code{trunc_lower}, \code{trunc_upper}}{Truncation bounds as supplied.}
  \item{\code{xlab}}{Variable label.}
  \item{\code{criterion}}{Ranking criterion used (\code{"AIC"} or \code{"BIC"}).}
}
}

\references{
Meeker, W. Q. and Escobar, L. A. (1998).
\emph{Statistical Methods for Reliability Data}.
Wiley, New York.

Lawless, J. F. (2003).
\emph{Statistical Models and Methods for Lifetime Data}, 2nd ed.
Wiley, Hoboken.

Kaplan, E. L. and Meier, P. (1958).
Nonparametric estimation from incomplete observations.
\emph{Journal of the American Statistical Association}, \bold{53}(282),
457--481. \doi{10.1080/01621459.1958.10501452}
}

\examples{
## Right-censored survival data (Weibull true model)
set.seed(1)
times  <- rweibull(100, shape = 1.8, scale = 50)
censor <- rexp(100, rate = 1/60)
x      <- pmin(times, censor)
status <- as.integer(times <= censor)   # 1 = event, 0 = right-censored
\donttest{
fit_s <- fitverse_censored(x, status,
                           dists = c("weibull", "lognormal", "gamma"),
                           xlab  = "Survival time (hours)")
print(fit_s)
plot(fit_s, top_n = 3)
}

## Left-censored environmental data (below detection limit)
set.seed(2)
conc       <- rlnorm(80, meanlog = 1.5, sdlog = 0.7)
dl         <- 3.0
x_env      <- ifelse(conc >= dl, conc, dl)
status_env <- ifelse(conc >= dl, 1L, 2L)
\donttest{
fit_e <- fitverse_censored(x_env, status_env,
                           dists = c("lognormal", "gamma"),
                           xlab  = "Concentration (mg/L)")
fit_e$rankings
}

## Left-truncated occupational exposure data
set.seed(3)
exposure <- rlnorm(300, meanlog = 2.5, sdlog = 0.5)
exposure <- exposure[exposure > 5]   # only workers above LOQ recorded
\donttest{
fit_t <- fitverse_censored(exposure, trunc_lower = 5,
                           dists = c("lognormal", "gamma", "weibull"),
                           xlab  = "Exposure (mg/m3)")
fit_t$fits[[1L]]$par
}
}

\seealso{
\code{\link{fitverse}} for uncensored data fitting;
\code{\link{fitverse_distributions}} for the full distribution library.
}

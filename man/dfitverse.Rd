\name{dfitverse}
\alias{dfitverse}
\alias{pfitverse}
\alias{qfitverse}
\alias{rfitverse}
\title{Density, CDF, Quantile, and Random Sampling for a Fitted Distribution}

\description{
Convenience functions that evaluate the probability density function (PDF),
cumulative distribution function (CDF), quantile function (inverse CDF),
and random-number generator for the best-fit distribution stored in a
\code{fitverse} object.

These follow the standard R d/p/q/r naming convention and delegate to
the underlying \code{d*}, \code{p*}, \code{q*}, and \code{r*} functions
for the best-fit distribution.
}

\usage{
dfitverse(x,   fit, log = FALSE)
pfitverse(q,   fit, lower.tail = TRUE, log.p = FALSE)
qfitverse(p,   fit, lower.tail = TRUE, log.p = FALSE)
rfitverse(n,   fit)
}

\arguments{
  \item{x}{
    Numeric vector of quantiles at which to evaluate the density.
  }
  \item{q}{
    Numeric vector of quantiles at which to evaluate the CDF.
  }
  \item{p}{
    Numeric vector of probabilities \eqn{p \in (0, 1)}.
  }
  \item{n}{
    Integer: number of random samples to generate.
  }
  \item{fit}{
    An object of class \code{"fitverse"} as returned by
    \code{\link{fitverse}()}.
  }
  \item{log, log.p}{
    Logical: if \code{TRUE}, return (or accept) log-scale values.
    Default \code{FALSE}.
  }
  \item{lower.tail}{
    Logical: if \code{TRUE} (default), return \eqn{P(X \le q)};
    if \code{FALSE}, return \eqn{P(X > q)}.
  }
}

\details{
The correct d/p/q/r functions are resolved via \code{.lookup_dpqr()},
which follows a two-step priority:

\enumerate{
  \item \strong{Internal closed-form implementations} (highest priority):
    GEV, Gumbel, GPD, Pearson III, Log-Pearson III, and GLO all use
    FitVerse's own vectorised implementations registered in
    \code{.FITVERSE_DPQR_OVERRIDES}.  This avoids any package-version
    signature issues with \pkg{evd} and provides full support for the
    new hydrological distributions that have no dedicated CRAN d/p/q/r
    package.
  \item \strong{Package namespace lookup}: For all other distributions,
    \pkg{stats} functions are found via \code{match.fun()}; Imports/Suggests
    packages (\pkg{actuar}, \pkg{sn}, \pkg{mc2d}) are resolved via
    \code{getExportedValue()}.
}

The estimated parameters are extracted from \code{fit$best_fit$params}
and passed as named arguments to the underlying function.  This is
equivalent to calling, for example,
\code{dgamma(x, shape = fit$best_fit$params["shape"], rate = fit$best_fit$params["rate"])}.
}

\value{
\describe{
  \item{\code{dfitverse()}}{Numeric vector of density values.}
  \item{\code{pfitverse()}}{Numeric vector of probabilities.}
  \item{\code{qfitverse()}}{Numeric vector of quantiles.}
  \item{\code{rfitverse()}}{Numeric vector of random samples.}
}
}

\seealso{
\code{\link{return_level}} for T-year return levels. \cr
\code{\link{fitverse}} for the fitting step.
}

\examples{
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)
fit <- fitverse(x, dists = "gamma", plot = FALSE, verbose = FALSE)

# Density at x = 2
dfitverse(2, fit)

# Probability that X <= 5
pfitverse(5, fit)

# 90th percentile
qfitverse(0.90, fit)

# Generate 10 random samples
rfitverse(10, fit)
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}

\name{fitverse_s3methods}
\alias{print.fitverse}
\alias{coef.fitverse}
\alias{logLik.fitverse}
\alias{AIC.fitverse}
\alias{BIC.fitverse}
\alias{as.list.fitverse}
\title{S3 Methods for \code{fitverse} Objects}

\description{
Standard S3 methods for objects of class \code{"fitverse"} as returned by
\code{\link{fitverse}()}.  These methods provide a consistent interface
for extracting, displaying, and serialising fitted distribution results.
}

\usage{
\method{print}{fitverse}(x, \dots)

\method{coef}{fitverse}(object, \dots)

\method{logLik}{fitverse}(object, \dots)

\method{AIC}{fitverse}(object, \dots, k = 2)

\method{BIC}{fitverse}(object, \dots)

\method{as.list}{fitverse}(x, \dots)
}

\arguments{
  \item{x, object}{
    An object of class \code{"fitverse"}.
  }
  \item{k}{
    Numeric: penalty per parameter used by \code{AIC()}.  Default \code{2}
    (standard AIC).  Pass \code{k = log(n)} to obtain BIC-equivalent
    penalisation.
  }
  \item{\dots}{
    Currently unused.
  }
}

\details{
\describe{
  \item{\code{print.fitverse()}}{
    Prints a concise single-screen summary: best-fit distribution,
    estimation method, number of parameters, parameter estimates,
    log-likelihood, AIC, BIC, and up to three goodness-of-fit test results.
  }
  \item{\code{coef.fitverse()}}{
    Returns the named numeric vector of parameter estimates for the
    best-fit model.  For example, \code{c(shape = 2.14, rate = 0.43)}
    for a Gamma fit.
  }
  \item{\code{logLik.fitverse()}}{
    Returns an object of class \code{"logLik"} with the maximised
    log-likelihood and the degrees of freedom (number of parameters).
    Compatible with \code{stats::AIC()} and \code{stats::BIC()}.
  }
  \item{\code{AIC.fitverse()}}{
    Returns the Akaike Information Criterion
    \eqn{\mathrm{AIC} = -2\ell + k \cdot p}, where \eqn{\ell} is the
    log-likelihood, \eqn{p} is the number of parameters, and \eqn{k}
    is the penalty (default 2).
  }
  \item{\code{BIC.fitverse()}}{
    Returns the Bayesian Information Criterion
    \eqn{\mathrm{BIC} = -2\ell + p \log n}.
  }
  \item{\code{as.list.fitverse()}}{
    Converts the \code{fitverse} object to a plain list suitable for
    JSON serialisation (used by \code{\link{to_json}()}).
    Non-serialisable internals (raw fit objects, closures) are stripped.
    Returns: \code{best_fit} (dist_key, method, params, loglik, aic, bic),
    \code{ranking}, \code{data_summary}, \code{n}, \code{xlab},
    \code{criterion}.
  }
}
}

\value{
\describe{
  \item{\code{print.fitverse()}}{Invisibly returns \code{x}.}
  \item{\code{coef.fitverse()}}{Named numeric vector of parameter estimates.}
  \item{\code{logLik.fitverse()}}{Object of class \code{"logLik"}.}
  \item{\code{AIC.fitverse()}}{Numeric scalar.}
  \item{\code{BIC.fitverse()}}{Numeric scalar.}
  \item{\code{as.list.fitverse()}}{A named list safe for JSON serialisation.}
}
}

\seealso{
\code{\link{fitverse}}, \code{\link{to_json}}, \code{\link{plot.fitverse}}.
}

\examples{
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)
fit <- fitverse(x, plot = FALSE, verbose = FALSE)

print(fit)
coef(fit)
logLik(fit)
AIC(fit)
BIC(fit)

# Serialisation-ready list
lst <- as.list(fit)
names(lst)
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{methods}
\keyword{print}

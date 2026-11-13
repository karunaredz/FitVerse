\name{bootstrap_ci}
\alias{bootstrap_ci}
\alias{print.fitverse_bootstrap}
\title{Bootstrap Confidence Intervals for Distribution Parameters}

\description{
Computes non-parametric percentile bootstrap confidence intervals for the
parameters of the best-fit distribution in a \code{fitverse} object.
Optionally also bootstraps confidence intervals for T-year return levels.
}

\usage{
bootstrap_ci(fit,
             B              = 999L,
             conf           = 0.95,
             return_periods = NULL,
             progress       = NULL,
             seed           = NULL,
             verbose        = TRUE)

\method{print}{fitverse_bootstrap}(x, \dots)
}

\arguments{
  \item{fit}{
    An object of class \code{"fitverse"} as returned by
    \code{\link{fitverse}()}.  The bootstrap uses the best-fit distribution
    and estimation method stored in \code{fit$best_fit}.
  }
  \item{B}{
    Integer: number of bootstrap replicates.  Default \code{999}.
    At least \code{199} is recommended; \code{1999} for publication-quality
    intervals.
  }
  \item{conf}{
    Numeric in \eqn{(0, 1)}: confidence level.  Default \code{0.95}
    (95\% CI).
  }
  \item{return_periods}{
    Numeric vector of return periods for which bootstrap CIs on return
    levels should be computed.  \code{NULL} (default) skips this step.
    Example: \code{return_periods = c(10, 50, 100)}.
  }
  \item{progress}{
    Optional function of the form \code{function(p)} where \code{p} is a
    numeric value in \eqn{[0, 1]}.  Called after every 10\% of replicates.
    Useful for \pkg{shiny} progress bars.  Default \code{NULL}.
  }
  \item{seed}{
    Integer or \code{NULL}: random seed for reproducibility.
    Default \code{NULL} (no seed set).
  }
  \item{verbose}{
    Logical: print a progress message?  Default \code{TRUE}.
  }
  \item{x}{
    An object of class \code{"fitverse_bootstrap"}.
  }
  \item{\dots}{
    Currently unused.
  }
}

\details{
\subsection{Algorithm}{
The non-parametric percentile bootstrap proceeds as follows:
\enumerate{
  \item Draw \eqn{B} bootstrap samples of size \eqn{n} with replacement
    from the observed data.
  \item Re-fit the same distribution and estimation method to each sample,
    using the same internal engine as \code{\link{fitverse}()}.
  \item Collect the \eqn{B \times k} matrix of parameter estimates
    (where \eqn{k} is the number of parameters).
  \item For each parameter, report the \eqn{\alpha/2} and \eqn{1 - \alpha/2}
    empirical percentiles of the bootstrap distribution as the lower and
    upper confidence bounds.
}
Replicates that fail to converge are excluded; a warning is issued if
more than 10\% fail.
}

\subsection{Return-level CIs}{
When \code{return_periods} is supplied, the T-year return level is computed
for each bootstrap replicate using \code{\link{return_level}()}, and the
percentile CI is reported for each T.
}
}

\value{
An S3 object of class \code{"fitverse_bootstrap"} with the following
elements:

\describe{
  \item{\code{params_ci}}{
    Data frame with one row per parameter and columns:
    \code{parameter}, \code{estimate}, \code{lower}, \code{upper},
    \code{se_boot} (bootstrap standard error).
  }
  \item{\code{return_level_ci}}{
    Data frame (or \code{NULL} if \code{return_periods = NULL}) with columns:
    \code{T}, \code{return_level}, \code{lower}, \code{upper}.
  }
  \item{\code{boot_params}}{
    Numeric matrix of dimension \eqn{B_{eff} \times k}: all successful
    bootstrap parameter estimates.  Useful for plotting bootstrap
    distributions.
  }
  \item{\code{B}}{Integer: number of replicates requested.}
  \item{\code{B_eff}}{Integer: number of successful replicates.}
  \item{\code{conf}}{Numeric: confidence level.}
  \item{\code{dist_key}}{Character: distribution key.}
  \item{\code{method}}{Character: estimation method.}
}
}

\references{
Efron, B. and Tibshirani, R. J. (1993).
\emph{An Introduction to the Bootstrap}.
Chapman and Hall, New York.

Davison, A. C. and Hinkley, D. V. (1997).
\emph{Bootstrap Methods and Their Application}.
Cambridge University Press.
}

\seealso{
\code{\link{return_level}} for point estimates without CIs. \cr
\code{\link{fitverse}} for the initial fitting step.
}

\examples{
\donttest{
set.seed(42)
x <- rgamma(200, shape = 2, rate = 0.5)
fit <- fitverse(x, dists = "gamma", plot = FALSE, verbose = FALSE)

boot <- bootstrap_ci(fit, B = 499, conf = 0.95, seed = 1, verbose = FALSE)
print(boot)

# Parameter CI data frame
boot$params_ci

# With return-level CIs
boot2 <- bootstrap_ci(fit, B = 499, return_periods = c(10, 50, 100),
                      seed = 1, verbose = FALSE)
boot2$return_level_ci
}
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{htest}
\keyword{distribution}

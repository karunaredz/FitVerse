\name{return_level}
\alias{return_level}
\title{Compute T-Year Return Levels from a Fitted Distribution}

\description{
Computes return levels (quantiles) corresponding to specified return periods
from the best-fit distribution in a \code{fitverse} object.  Return levels
are widely used in hydrology, climatology, and actuarial science to
characterise the magnitude of rare events.
}

\usage{
return_level(fit,
             return_periods = c(2, 5, 10, 20, 50, 100, 200, 500))
}

\arguments{
  \item{fit}{
    An object of class \code{"fitverse"} as returned by
    \code{\link{fitverse}()}.
  }
  \item{return_periods}{
    Numeric vector of return periods (in years, or whichever time unit the
    data represent).  Must be \eqn{\ge 1}.
    Default: \code{c(2, 5, 10, 20, 50, 100, 200, 500)}.
  }
}

\details{
A T-year return level \eqn{x_T} is the value that is exceeded on average
once every \eqn{T} years (or time units), i.e.:
\deqn{P(X > x_T) = \frac{1}{T}}
\deqn{x_T = F^{-1}\!\left(1 - \frac{1}{T}\right)}
where \eqn{F^{-1}} is the quantile function of the fitted distribution.

Quantiles are computed using \code{\link{qfitverse}()}, which resolves
the quantile function from the internal distribution registry.
}

\value{
A data frame with one row per return period and columns:

\describe{
  \item{\code{T}}{Numeric: the return period.}
  \item{\code{exceedance_prob}}{Numeric: \eqn{1/T}, the annual exceedance
    probability.}
  \item{\code{return_level}}{Numeric: the estimated T-year return level
    (upper \eqn{1/T} quantile of the fitted distribution).}
}
}

\references{
Coles, S. (2001). \emph{An Introduction to Statistical Modeling of Extreme
Values}. Springer, London.

Katz, R. W., Parlange, M. B. and Naveau, P. (2002). Statistics of extremes
in hydrology. \emph{Advances in Water Resources}, \bold{25}(8-12), 1287--1304.
}

\seealso{
\code{\link{qfitverse}} for quantiles at arbitrary probabilities. \cr
\code{\link{bootstrap_ci}} for confidence intervals on return levels.
}

\examples{
\donttest{
# Flood frequency analysis with GEV distribution
set.seed(10)
floods <- evd::rgev(80, loc = 200, scale = 40, shape = 0.15)

fit <- fitverse(floods,
                dists   = c("gev", "gumbel", "lognormal"),
                method  = "MLE",
                xlab    = "Annual maximum flood (m3/s)",
                plot    = FALSE,
                verbose = FALSE)

rl <- return_level(fit)
print(rl)

# 100-year return level
rl[rl$T == 100, "return_level"]
}
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}
\keyword{extreme}

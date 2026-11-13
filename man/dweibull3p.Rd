\name{dweibull3p}
\alias{dweibull3p}
\alias{pweibull3p}
\alias{qweibull3p}
\alias{rweibull3p}

\title{Three-Parameter Weibull Distribution}

\description{
Density, cumulative distribution function, quantile function, and random
generation for the three-parameter Weibull distribution with shape
\eqn{\beta > 0}, scale \eqn{\alpha > 0}, and threshold (location)
\eqn{\gamma}.  The support is \eqn{x > \gamma}.
}

\usage{
dweibull3p(x,     shape, scale, thres, log = FALSE)
pweibull3p(q,     shape, scale, thres, lower.tail = TRUE, log.p = FALSE)
qweibull3p(p,     shape, scale, thres, lower.tail = TRUE, log.p = FALSE)
rweibull3p(n,     shape, scale, thres)
}

\arguments{
  \item{x, q}{Numeric vector of quantiles.}
  \item{p}{Numeric vector of probabilities in \eqn{(0, 1)}.}
  \item{n}{Integer: number of observations to generate.}
  \item{shape}{Positive shape parameter \eqn{\beta}.}
  \item{scale}{Positive scale parameter \eqn{\alpha}.}
  \item{thres}{Real-valued threshold (location) parameter \eqn{\gamma}.
    Values of \code{x} at or below \code{thres} have zero density.}
  \item{log, log.p}{Logical: if \code{TRUE}, return log-probabilities.}
  \item{lower.tail}{Logical: if \code{TRUE} (default), return
    \eqn{P(X \le q)}; otherwise \eqn{P(X > q)}.}
}

\details{
The three-parameter Weibull is a location-shifted version of the standard
two-parameter Weibull.  Let \eqn{Y = X - \gamma}; then \eqn{Y \sim
\mathrm{Weibull}(\beta, \alpha)}.  The PDF for \eqn{x > \gamma} is

\deqn{f(x) = \frac{\beta}{\alpha}
             \left(\frac{x - \gamma}{\alpha}\right)^{\beta - 1}
             \exp\!\left[-\left(\frac{x - \gamma}{\alpha}\right)^{\beta}\right].}

Internally these functions shift \code{x} by \code{thres} and delegate to
\code{\link[stats]{dweibull}}, \code{\link[stats]{pweibull}},
\code{\link[stats]{qweibull}}, and \code{\link[stats]{rweibull}}.
}

\value{
\describe{
  \item{\code{dweibull3p}}{Numeric vector of density values.}
  \item{\code{pweibull3p}}{Numeric vector of probabilities.}
  \item{\code{qweibull3p}}{Numeric vector of quantiles.}
  \item{\code{rweibull3p}}{Numeric vector of random samples.}
}
}

\seealso{
\code{\link[stats]{dweibull}} for the two-parameter Weibull;
\code{\link{fitverse}} with \code{dists = "weibull3"} to fit this
distribution by MLE.
}

\examples{
## PDF and CDF
curve(dweibull3p(x, shape = 2, scale = 10, thres = 5),
      from = 5, to = 35, ylab = "Density",
      main = "Weibull(2, 10) shifted by 5")

## Quantile / CDF round-trip
p  <- c(0.1, 0.5, 0.9)
q  <- qweibull3p(p, shape = 2, scale = 10, thres = 5)
pweibull3p(q, shape = 2, scale = 10, thres = 5)   # should recover p

## Random samples
set.seed(1)
x <- rweibull3p(200, shape = 1.8, scale = 50, thres = 10)
hist(x, breaks = 20, main = "Weibull(1.8, 50) with threshold 10")
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}

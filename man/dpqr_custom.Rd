\name{dpqr_custom}
\alias{dstudent_t}
\alias{pstudent_t}
\alias{qstudent_t}
\alias{rstudent_t}
\alias{dlaplace}
\alias{plaplace}
\alias{qlaplace}
\alias{rlaplace}
\alias{dinvgaussian}
\alias{pinvgaussian}
\alias{qinvgaussian}
\alias{rinvgaussian}
\alias{dlomax}
\alias{plomax}
\alias{qlomax}
\alias{rlomax}
\alias{dgengamma}
\alias{pgengamma}
\alias{qgengamma}
\alias{rgengamma}
\alias{dgompertz}
\alias{pgompertz}
\alias{qgompertz}
\alias{rgompertz}
\alias{dbirnbaum_saunders}
\alias{pbirnbaum_saunders}
\alias{qbirnbaum_saunders}
\alias{rbirnbaum_saunders}
\alias{drayleigh}
\alias{prayleigh}
\alias{qrayleigh}
\alias{rrayleigh}
\alias{dhalfnormal}
\alias{phalfnormal}
\alias{qhalfnormal}
\alias{rhalfnormal}
\alias{dexpweibull}
\alias{pexpweibull}
\alias{qexpweibull}
\alias{rexpweibull}
\alias{dgamma3}
\alias{pgamma3}
\alias{qgamma3}
\alias{rgamma3}
\alias{dlnorm3}
\alias{plnorm3}
\alias{qlnorm3}
\alias{rlnorm3}
\alias{drtriangle}
\alias{prtriangle}
\alias{qrtriangle}
\alias{rrtriangle}
\alias{dkumaraswamy}
\alias{pkumaraswamy}
\alias{qkumaraswamy}
\alias{rkumaraswamy}
\alias{dgb2}
\alias{pgb2}
\alias{qgb2}
\alias{rgb2}
\alias{dkap4}
\alias{pkap4}
\alias{qkap4}
\alias{rkap4}
\alias{dlindley}
\alias{plindley}
\alias{qlindley}
\alias{rlindley}
\alias{dgnorm}
\alias{pgnorm}
\alias{qgnorm}
\alias{rgnorm}
\alias{ddagum}
\alias{pdagum}
\alias{qdagum}
\alias{rdagum}
\alias{djohnsonsu}
\alias{pjohnsonsu}
\alias{qjohnsonsu}
\alias{rjohnsonsu}
\alias{dalaplace}
\alias{palaplace}
\alias{qalaplace}
\alias{ralaplace}
\alias{dexpexp}
\alias{pexpexp}
\alias{qexpexp}
\alias{rexpexp}
\alias{dnig}
\alias{pnig}
\alias{qnig}
\alias{rnig}
\alias{dwakeby}
\alias{pwakeby}
\alias{qwakeby}
\alias{rwakeby}
\alias{dghyp}
\alias{pghyp}
\alias{qghyp}
\alias{rghyp}
\alias{dtruncnorm}
\alias{ptruncnorm}
\alias{qtruncnorm}
\alias{rtruncnorm}
\alias{dnakagami}
\alias{pnakagami}
\alias{qnakagami}
\alias{rnakagami}

\title{Custom d/p/q/r Functions for Additional Distributions}

\description{
Density, CDF, quantile, and random-generation functions for distribution
families that are either unavailable on CRAN, parameterised differently
from existing packages, or implemented from scratch for performance and
consistency within \pkg{FitVerse}.

The following families are covered:
\describe{
  \item{Student-t (location-scale)}{Three-parameter Student-t with
    location \eqn{\mu}, scale \eqn{\sigma > 0}, and degrees of freedom
    \eqn{\nu > 0}: \code{dstudent_t}, \code{pstudent_t},
    \code{qstudent_t}, \code{rstudent_t}.}
  \item{Laplace}{Symmetric double-exponential with location \eqn{\mu}
    and scale \eqn{b > 0}: \code{dlaplace}, \code{plaplace},
    \code{qlaplace}, \code{rlaplace}.}
  \item{Inverse Gaussian}{Two-parameter (mean \eqn{\mu > 0}, shape
    \eqn{\lambda > 0}): \code{dinvgaussian}, \code{pinvgaussian},
    \code{qinvgaussian}, \code{rinvgaussian}.}
  \item{Lomax (Pareto Type II)}{Shape \eqn{\alpha > 0} and scale
    \eqn{\lambda > 0}: \code{dlomax}, \code{plomax}, \code{qlomax},
    \code{rlomax}.}
  \item{Generalised Gamma}{Three-parameter generalisation of the Gamma
    (shape \eqn{p > 0}, scale \eqn{d > 0}, power \eqn{b > 0}):
    \code{dgengamma}, \code{pgengamma}, \code{qgengamma},
    \code{rgengamma}.}
  \item{Gompertz}{Parameters \eqn{\alpha > 0} and \eqn{\beta > 0}:
    \code{dgompertz}, \code{pgompertz}, \code{qgompertz},
    \code{rgompertz}.}
  \item{Birnbaum-Saunders (Fatigue-Life)}{Shape \eqn{\alpha > 0} and
    scale \eqn{\beta > 0}: \code{dbirnbaum_saunders},
    \code{pbirnbaum_saunders}, \code{qbirnbaum_saunders},
    \code{rbirnbaum_saunders}.}
  \item{Rayleigh}{Scale \eqn{\sigma > 0}:
    \code{drayleigh}, \code{prayleigh}, \code{qrayleigh},
    \code{rrayleigh}.}
  \item{Half-Normal}{Scale \eqn{\sigma > 0}:
    \code{dhalfnormal}, \code{phalfnormal}, \code{qhalfnormal},
    \code{rhalfnormal}.}
  \item{Exponentiated Weibull}{Shape parameters \code{shape1 > 0} and
    \code{shape2 > 0}, scale \code{scale > 0}:
    \code{dexpweibull}, \code{pexpweibull}, \code{qexpweibull},
    \code{rexpweibull}.}
  \item{Three-Parameter Gamma}{Gamma with location threshold \eqn{\gamma}
    (shape \eqn{\alpha > 0}, scale \eqn{\beta > 0}, threshold
    \eqn{\gamma}): \code{dgamma3}, \code{pgamma3}, \code{qgamma3},
    \code{rgamma3}.}
  \item{Three-Parameter Log-Normal}{Log-Normal with threshold \eqn{\gamma}
    (meanlog \eqn{\mu}, sdlog \eqn{\sigma > 0}, threshold \eqn{\gamma}):
    \code{dlnorm3}, \code{plnorm3}, \code{qlnorm3}, \code{rlnorm3}.}
  \item{Right Triangular}{Special triangular with \code{min} and
    \code{max}, where the mode equals \code{max}:
    \code{drtriangle}, \code{prtriangle}, \code{qrtriangle},
    \code{rrtriangle}.}
  \item{Kumaraswamy}{Bounded distribution with shape parameters
    \code{shape1 > 0} and \code{shape2 > 0} on \eqn{(0, 1)}:
    \code{dkumaraswamy}, \code{pkumaraswamy}, \code{qkumaraswamy},
    \code{rkumaraswamy}.}
  \item{Generalised Beta of the Second Kind (GB2)}{Four-parameter family
    with \code{shape1 > 0}, \code{scale > 0}, \code{shape2 > 0},
    \code{shape3 > 0}: \code{dgb2}, \code{pgb2}, \code{qgb2},
    \code{rgb2}.}
  \item{Kappa Four-Parameter}{Generalised extreme-value family with
    location \code{xi}, scale \code{alpha > 0}, and shape parameters
    \code{kappa} and \code{h}: \code{dkap4}, \code{pkap4}, \code{qkap4},
    \code{rkap4}.}
  \item{Lindley}{One-parameter distribution with \code{theta > 0}:
    \code{dlindley}, \code{plindley}, \code{qlindley}, \code{rlindley}.}
  \item{Generalised Normal}{Three-parameter distribution with location
    \code{mu}, scale \code{sigma > 0}, and shape \code{beta > 0}:
    \code{dgnorm}, \code{pgnorm}, \code{qgnorm}, \code{rgnorm}.}
  \item{Dagum}{Three-parameter Burr Type III with \code{shape1 > 0},
    \code{scale > 0}, \code{shape2 > 0}: \code{ddagum}, \code{pdagum},
    \code{qdagum}, \code{rdagum}.}
  \item{Johnson SU}{Four-parameter unbounded system with \code{gamma},
    \code{delta > 0}, \code{xi}, \code{lambda > 0}: \code{djohnsonsu},
    \code{pjohnsonsu}, \code{qjohnsonsu}, \code{rjohnsonsu}.}
  \item{Asymmetric Laplace}{Three-parameter asymmetric Laplace with
    location \code{mu}, scale \code{sigma > 0}, and asymmetry
    \code{kappa > 0}: \code{dalaplace}, \code{palaplace},
    \code{qalaplace}, \code{ralaplace}.}
  \item{Exponentiated Exponential}{Two-parameter family with shape
    \code{alpha > 0} and rate \code{lambda > 0}: \code{dexpexp},
    \code{pexpexp}, \code{qexpexp}, \code{rexpexp}.}
  \item{Normal-Inverse Gaussian (NIG)}{Four-parameter family with
    location \code{mu}, spread \code{delta > 0}, steepness
    \code{alpha > 0}, and asymmetry \code{beta}: \code{dnig},
    \code{pnig}, \code{qnig}, \code{rnig}.}
  \item{Wakeby}{Five-parameter quantile-defined distribution with
    location \code{xi}, scale parameters \code{alpha} and \code{beta},
    and shape parameters \code{gamma} and \code{delta}:
    \code{dwakeby}, \code{pwakeby}, \code{qwakeby}, \code{rwakeby}.}
  \item{Generalised Hyperbolic (GHyp)}{Five-parameter family with
    \code{lambda}, \code{alpha > 0}, \code{beta}, \code{delta > 0},
    \code{mu}: \code{dghyp}, \code{pghyp}, \code{qghyp}, \code{rghyp}.}
  \item{Truncated Normal}{Normal distribution truncated to \eqn{[a, b]}
    with \code{mean} and \code{sd > 0}: \code{dtruncnorm},
    \code{ptruncnorm}, \code{qtruncnorm}, \code{rtruncnorm}.}
  \item{Nakagami}{Two-parameter fading distribution with shape
    \code{m >= 0.5} and spread \code{Omega > 0}: \code{dnakagami},
    \code{pnakagami}, \code{qnakagami}, \code{rnakagami}.}
}
}

\usage{
dstudent_t(x,   nu, mu, sigma, log = FALSE)
pstudent_t(q,   nu, mu, sigma)
qstudent_t(p,   nu, mu, sigma)
rstudent_t(n,   nu, mu, sigma)

dlaplace(x,   mu, b, log = FALSE)
plaplace(q,   mu, b)
qlaplace(p,   mu, b)
rlaplace(n,   mu, b)

dinvgaussian(x,   mu, lambda, log = FALSE)
pinvgaussian(q,   mu, lambda)
qinvgaussian(p,   mu, lambda)
rinvgaussian(n,   mu, lambda)

dlomax(x,   alpha, lambda, log = FALSE)
plomax(q,   alpha, lambda)
qlomax(p,   alpha, lambda)
rlomax(n,   alpha, lambda)

dgengamma(x,   p, d, b, log = FALSE)
pgengamma(q,   p, d, b)
qgengamma(prob, p, d, b)
rgengamma(n,   p, d, b)

dgompertz(x,   alpha, beta, log = FALSE)
pgompertz(q,   alpha, beta)
qgompertz(p,   alpha, beta)
rgompertz(n,   alpha, beta)

dbirnbaum_saunders(x,   alpha, beta, log = FALSE)
pbirnbaum_saunders(q,   alpha, beta)
qbirnbaum_saunders(p,   alpha, beta)
rbirnbaum_saunders(n,   alpha, beta)

drayleigh(x,   sigma, log = FALSE)
prayleigh(q,   sigma, lower.tail = TRUE, log.p = FALSE)
qrayleigh(p,   sigma, lower.tail = TRUE, log.p = FALSE)
rrayleigh(n,   sigma)

dhalfnormal(x,   sigma, log = FALSE)
phalfnormal(q,   sigma, lower.tail = TRUE, log.p = FALSE)
qhalfnormal(p,   sigma, lower.tail = TRUE, log.p = FALSE)
rhalfnormal(n,   sigma)

dexpweibull(x,   shape1, scale, shape2, log = FALSE)
pexpweibull(q,   shape1, scale, shape2, lower.tail = TRUE, log.p = FALSE)
qexpweibull(p,   shape1, scale, shape2, lower.tail = TRUE, log.p = FALSE)
rexpweibull(n,   shape1, scale, shape2)

dgamma3(x,   shape, scale, thres, log = FALSE)
pgamma3(q,   shape, scale, thres, lower.tail = TRUE, log.p = FALSE)
qgamma3(p,   shape, scale, thres, lower.tail = TRUE, log.p = FALSE)
rgamma3(n,   shape, scale, thres)

dlnorm3(x,   meanlog, sdlog, thres, log = FALSE)
plnorm3(q,   meanlog, sdlog, thres, lower.tail = TRUE, log.p = FALSE)
qlnorm3(p,   meanlog, sdlog, thres, lower.tail = TRUE, log.p = FALSE)
rlnorm3(n,   meanlog, sdlog, thres)

drtriangle(x,   min, max, log = FALSE)
prtriangle(q,   min, max)
qrtriangle(p,   min, max)
rrtriangle(n,   min, max)

dkumaraswamy(x,   shape1, shape2, log = FALSE)
pkumaraswamy(q,   shape1, shape2)
qkumaraswamy(p,   shape1, shape2)
rkumaraswamy(n,   shape1, shape2)

dgb2(x, shape1, scale, shape2, shape3, log = FALSE)
pgb2(q, shape1, scale, shape2, shape3, lower.tail = TRUE, log.p = FALSE)
qgb2(p, shape1, scale, shape2, shape3, lower.tail = TRUE, log.p = FALSE)
rgb2(n, shape1, scale, shape2, shape3)

dkap4(x, xi, alpha, kappa, h, log = FALSE)
pkap4(q, xi, alpha, kappa, h, lower.tail = TRUE, log.p = FALSE)
qkap4(p, xi, alpha, kappa, h, lower.tail = TRUE, log.p = FALSE)
rkap4(n, xi, alpha, kappa, h)

dlindley(x, theta, log = FALSE)
plindley(q, theta, lower.tail = TRUE, log.p = FALSE)
qlindley(p, theta, lower.tail = TRUE, log.p = FALSE)
rlindley(n, theta)

dgnorm(x, mu, sigma, beta, log = FALSE)
pgnorm(q, mu, sigma, beta, lower.tail = TRUE, log.p = FALSE)
qgnorm(p, mu, sigma, beta, lower.tail = TRUE, log.p = FALSE)
rgnorm(n, mu, sigma, beta)

ddagum(x, shape1, scale, shape2, log = FALSE)
pdagum(q, shape1, scale, shape2, lower.tail = TRUE, log.p = FALSE)
qdagum(p, shape1, scale, shape2, lower.tail = TRUE, log.p = FALSE)
rdagum(n, shape1, scale, shape2)

djohnsonsu(x, gamma, delta, xi, lambda, log = FALSE)
pjohnsonsu(q, gamma, delta, xi, lambda, lower.tail = TRUE, log.p = FALSE)
qjohnsonsu(p, gamma, delta, xi, lambda, lower.tail = TRUE, log.p = FALSE)
rjohnsonsu(n, gamma, delta, xi, lambda)

dalaplace(x, mu, sigma, kappa, log = FALSE)
palaplace(q, mu, sigma, kappa, lower.tail = TRUE, log.p = FALSE)
qalaplace(p, mu, sigma, kappa, lower.tail = TRUE, log.p = FALSE)
ralaplace(n, mu, sigma, kappa)

dexpexp(x, alpha, lambda, log = FALSE)
pexpexp(q, alpha, lambda, lower.tail = TRUE, log.p = FALSE)
qexpexp(p, alpha, lambda, lower.tail = TRUE, log.p = FALSE)
rexpexp(n, alpha, lambda)

dnig(x, mu, delta, alpha, beta, log = FALSE)
pnig(q, mu, delta, alpha, beta, lower.tail = TRUE, log.p = FALSE)
qnig(p, mu, delta, alpha, beta, lower.tail = TRUE, log.p = FALSE)
rnig(n, mu, delta, alpha, beta)

dwakeby(x, xi, alpha, beta, gamma, delta, log = FALSE)
pwakeby(q, xi, alpha, beta, gamma, delta, lower.tail = TRUE, log.p = FALSE)
qwakeby(p, xi, alpha, beta, gamma, delta, lower.tail = TRUE, log.p = FALSE)
rwakeby(n, xi, alpha, beta, gamma, delta)

dghyp(x, lambda, alpha, beta, delta, mu, log = FALSE)
pghyp(q, lambda, alpha, beta, delta, mu, lower.tail = TRUE, log.p = FALSE)
qghyp(p, lambda, alpha, beta, delta, mu, lower.tail = TRUE, log.p = FALSE)
rghyp(n, lambda, alpha, beta, delta, mu)

dtruncnorm(x, mean, sd, a, b, log = FALSE)
ptruncnorm(q, mean, sd, a, b, lower.tail = TRUE, log.p = FALSE)
qtruncnorm(p, mean, sd, a, b, lower.tail = TRUE, log.p = FALSE)
rtruncnorm(n, mean, sd, a, b)

dnakagami(x, m, Omega, log = FALSE)
pnakagami(q, m, Omega, lower.tail = TRUE, log.p = FALSE)
qnakagami(p, m, Omega, lower.tail = TRUE, log.p = FALSE)
rnakagami(n, m, Omega)
}

\arguments{
  \item{x, q}{Numeric vector of quantiles.}
  \item{p}{Numeric vector of probabilities in \eqn{(0, 1)} for quantile
    functions; also the power shape parameter in \code{dgengamma},
    \code{pgengamma}, \code{qgengamma}, and \code{rgengamma}.}
  \item{prob}{Numeric vector of probabilities in \eqn{(0, 1)} for
    \code{qgengamma} (first argument, to avoid ambiguity with the \code{p}
    shape parameter).}
  \item{n}{Integer: number of observations to generate.}
  \item{nu}{Degrees of freedom for the Student-t (\eqn{\nu > 0}).}
  \item{mu, sigma}{Location and scale parameters.}
  \item{b}{Scale parameter for Laplace; also the shape parameter of the
    Generalised Gamma (used as the third shape in \code{dgengamma} etc.);
    also the upper truncation bound for \code{dtruncnorm} and related
    functions.}
  \item{d}{Depth shape parameter of the Generalised Gamma.}
  \item{lambda}{Inverse Gaussian shape parameter; Lomax scale parameter;
    Johnson SU scale parameter (\eqn{\lambda > 0}).}
  \item{scale, shape}{Scale and shape parameters.}
  \item{shape1, shape2}{Shape parameters.}
  \item{alpha, beta}{Gompertz and Birnbaum-Saunders parameters; Lomax uses \code{alpha}.}
  \item{meanlog}{Mean of the log-transformed variable (log-scale location) for
    \code{dlnorm3}/\code{plnorm3}/\code{qlnorm3}/\code{rlnorm3}.}
  \item{sdlog}{Standard deviation of the log-transformed variable (log-scale
    spread, \eqn{> 0}) for \code{dlnorm3}/\code{plnorm3}/\code{qlnorm3}/\code{rlnorm3}.}
  \item{thres}{Lower threshold (location shift) for 3-parameter distributions.}
  \item{min, max}{Support bounds for the Right Triangular.}
  \item{log, log.p}{Logical: if \code{TRUE}, probabilities are on the log scale.}
  \item{lower.tail}{Logical: if \code{TRUE} (default), \eqn{P(X \le q)};
    otherwise \eqn{P(X > q)}.}
  \item{shape3}{Third shape parameter for \code{dgb2} and related functions.}
  \item{xi}{Location parameter for Kappa, Wakeby, and Johnson SU distributions.}
  \item{h}{Lower shape parameter for the Kappa four-parameter distribution.}
  \item{kappa}{Upper shape parameter for the Kappa four-parameter distribution;
    also the asymmetry parameter for the Asymmetric Laplace.}
  \item{theta}{Rate parameter for the Lindley distribution (\eqn{\theta > 0}).}
  \item{gamma}{Shape parameter for the Johnson SU distribution;
    also a scale parameter in the Wakeby distribution.}
  \item{delta}{Shape parameter for the Johnson SU and Wakeby distributions;
    also the spread parameter for the NIG and GHyp distributions
    (\eqn{\delta > 0}).}
  \item{m}{Shape parameter for the Nakagami distribution (\eqn{m \ge 0.5}).}
  \item{Omega}{Spread parameter for the Nakagami distribution (\eqn{\Omega > 0}).}
  \item{mean, sd}{Mean and standard deviation for the Truncated Normal.}
  \item{a}{Lower truncation bound for \code{dtruncnorm} and related functions.}
}

\value{
A numeric vector of densities, probabilities, quantiles, or random samples,
consistent with the standard R \code{d}/\code{p}/\code{q}/\code{r}
convention.
}

\seealso{
\code{\link{fitverse}} to fit these distributions to data;
\code{\link{fitverse_distributions}} for the full distribution catalogue;
\code{\link{dfitverse}} for evaluating the fitted best-fit distribution.
}

\examples{
## Rayleigh
curve(drayleigh(x, sigma = 2), from = 0, to = 10,
      main = "Rayleigh(sigma=2)", ylab = "Density")

## Kumaraswamy (bounded to (0,1))
curve(dkumaraswamy(x, shape1 = 2, shape2 = 5), from = 0, to = 1,
      main = "Kumaraswamy(2, 5)", ylab = "Density")

## Birnbaum-Saunders round-trip
p <- c(0.1, 0.5, 0.9)
q <- qbirnbaum_saunders(p, alpha = 0.5, beta = 100)
pbirnbaum_saunders(q, alpha = 0.5, beta = 100)   # recovers p

## Inverse Gaussian random samples
set.seed(1)
x <- rinvgaussian(200, mu = 5, lambda = 10)
hist(x, breaks = 20, main = "Inverse Gaussian(5, 10)")
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}

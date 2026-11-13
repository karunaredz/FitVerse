\name{lmrd}
\alias{lmrd}
\alias{ev_threshold}

\title{L-Moment Ratio Diagram and Extreme-Value Threshold Selection}

\description{
Two diagnostic tools for exploratory data analysis in \pkg{FitVerse}.

\code{lmrd()} produces an L-moment ratio diagram plotting the sample
\eqn{(\tau_3, \tau_4)} pair against the theoretical curves of major
distribution families (GLO, GEV, GNO, PE3, GPA) and their known special
cases (Normal, Logistic, Gumbel, Exponential, Uniform).  The diagram
guides distributional identification before formal model fitting.

\code{ev_threshold()} plots three threshold-stability diagnostics for
Peaks-Over-Threshold (POT) extreme-value analysis: the Mean Residual Life
(MRL) plot, the shape-stability plot, and the modified-scale-stability
plot.  These are used to identify the lowest threshold above which the
Generalised Pareto Distribution (GPD) is a valid model for exceedances.
}

\usage{
lmrd(x = NULL,
     xlab = NULL,
     distributions = c("glo", "gev", "gno", "pe3", "gpa"),
     show_ref_points = TRUE,
     draw = TRUE)

ev_threshold(x,
             thresholds      = NULL,
             n_thresholds    = 40L,
             min_exceedances = 10L,
             conf            = 0.95,
             xlab            = "x",
             draw            = TRUE)
}

\arguments{
  \item{x}{Numeric vector of observations.  For \code{lmrd}, may be
    \code{NULL} to produce a blank reference diagram.}
  \item{xlab}{Character: x-axis label used in plots.  Defaults to
    \code{NULL} (auto-labelled) for \code{lmrd} and \code{"x"} for
    \code{ev_threshold}.}
  \item{distributions}{Character vector: distribution families whose
    theoretical L-moment ratio curves are drawn.  Default covers GLO, GEV,
    GNO, PE3, and GPA.}
  \item{show_ref_points}{Logical: whether to plot the known special-case
    reference points (Normal, Logistic, Gumbel, Exponential, Uniform).
    Default \code{TRUE}.}
  \item{draw}{Logical: if \code{TRUE} (default) the plot is printed;
    otherwise the \code{ggplot} object is returned invisibly.}
  \item{thresholds}{Optional numeric vector of candidate threshold values.
    If \code{NULL} (default), thresholds are chosen automatically.}
  \item{n_thresholds}{Integer: number of candidate threshold values to
    evaluate when \code{thresholds = NULL}.  Default \code{40}.}
  \item{min_exceedances}{Integer: minimum number of exceedances required at
    each threshold.  Thresholds with fewer exceedances are excluded.
    Default \code{10}.}
  \item{conf}{Numeric in \eqn{(0, 1)}: confidence level for the pointwise
    confidence bands.  Default \code{0.95}.}
}

\details{
\code{lmrd()} computes the sample L-skewness \eqn{\hat\tau_3} and
L-kurtosis \eqn{\hat\tau_4} via \code{\link[lmomco]{lmoms}} and plots
the sample point against the theoretical loci for the GLO, GEV, GPA,
GNO, and PE3 families.  Exact closed-form curves are used for GLO, GEV,
and GPA (Hosking, 1990); polynomial approximations are used for GNO and
PE3 (Hosking and Wallis, 1997).

\code{ev_threshold()} fits a GPD to exceedances at each candidate
threshold \eqn{u} and extracts the estimated shape \eqn{\hat\xi} and
modified scale \eqn{\hat\sigma^* = \hat\sigma - \hat\xi u}.  A stable
region in both plots, combined with approximate linearity of the MRL
plot, indicates a suitable threshold.
}

\value{
Both functions return a \code{ggplot} object invisibly and (by default)
display the plot.
}

\references{
Hosking, J. R. M. (1990).
L-moments: Analysis and estimation of distributions using linear
combinations of order statistics.
\emph{Journal of the Royal Statistical Society B}, \bold{52}(1), 105--124.
\doi{10.1111/j.2517-6161.1990.tb01775.x}

Hosking, J. R. M. and Wallis, J. R. (1997).
\emph{Regional Frequency Analysis: An Approach Based on L-Moments}.
Cambridge University Press.

Davison, A. C. and Smith, R. L. (1990).
Models for exceedances over high thresholds.
\emph{Journal of the Royal Statistical Society B}, \bold{52}(3), 393--442.
}

\seealso{
\code{\link{fitverse}} with \code{dists = "gpd"} for formal GPD fitting
after threshold selection.
}

\examples{
\donttest{
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)
lmrd(x, xlab = "Simulated Gamma data")
}

\donttest{
set.seed(1)
x_ev <- rexp(500, rate = 0.1)
ev_threshold(x_ev, n_thresholds = 30, xlab = "Simulated exponential")
}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{hplot}
\keyword{distribution}

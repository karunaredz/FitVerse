\name{FitVerse-package}
\alias{FitVerse-package}
\alias{FitVerse}
\docType{package}
\title{FitVerse: Parametric Distribution Fitting and Analysis}

\description{
FitVerse provides a unified, user-friendly interface for fitting parametric
probability distributions to continuous univariate data.  The package supports
\strong{52 distribution families} spanning symmetric, right-skewed, heavy-tailed,
bounded, and extreme-value shapes, and three estimation methods:
\strong{Maximum Likelihood Estimation (MLE)},
\strong{Method of Moments (MOM)}, and
\strong{L-Moments (LMOM)}.

Automatic best-fit selection is performed using AIC and BIC, supplemented by
four goodness-of-fit tests: Kolmogorov-Smirnov (KS), Anderson-Darling (AD),
Cramer-von Mises (CvM), and Chi-Squared.

Every fitted model produces a publication-quality diagnostic plot: a histogram
overlaid with the fitted density curve and the estimated PDF formula annotated
directly on the figure.  Additional diagnostic panels (Q-Q, P-P, CDF, Survival,
Hazard, and Cumulative Hazard) are available via \code{plot.fitverse()}.

FitVerse is designed as the data-characterisation layer of the
\emph{OptiStrata} ecosystem, with seamless handoff to \pkg{stratifyR}
for optimal stratification of survey populations.
}

\section{Main fitting functions}{
\tabular{ll}{
  \code{\link{fitverse}}               \tab Fit distributions to individual observations \cr
  \code{\link{fitverse_grouped}}       \tab Fit distributions to grouped/interval data \cr
  \code{\link{fitverse_censored}}      \tab Fit distributions to censored data \cr
  \code{\link{fitverse_batch}}         \tab Fit all columns of a data frame \cr
  \code{\link{compare_dists}}          \tab Return the ranked comparison table only \cr
  \code{\link{fitverse_distributions}} \tab List all 52 supported distributions \cr
}
}

\section{Probability tools}{
\tabular{ll}{
  \code{\link{dfitverse}}    \tab Density of the best-fit distribution \cr
  \code{\link{pfitverse}}    \tab CDF of the best-fit distribution \cr
  \code{\link{qfitverse}}    \tab Quantile function of the best-fit distribution \cr
  \code{\link{rfitverse}}    \tab Random samples from the best-fit distribution \cr
  \code{\link{return_level}} \tab T-year return levels from the fitted model \cr
  \code{\link{lmrd}}         \tab L-moment ratio diagram \cr
  \code{\link{ev_threshold}} \tab Extreme-value threshold stability plots \cr
}
}

\section{Inference and reporting}{
\tabular{ll}{
  \code{\link{bootstrap_ci}}  \tab Bootstrap confidence intervals for parameters \cr
  \code{\link{generate_report}} \tab Produce a self-contained HTML or PDF report \cr
  \code{\link{save_plot}}     \tab Save the diagnostic plot to a file \cr
  \code{\link{to_json}}       \tab Serialise a fitverse object to JSON \cr
}
}

\section{Sample datasets}{
\tabular{ll}{
  \code{\link{FitVerse_datasets}} \tab Bundled CSV files for individual and grouped data \cr
  \code{\link{read_grouped_csv}}  \tab Read a grouped-data CSV into list format \cr
}
}

\section{S3 methods for \code{"fitverse"} objects}{
\tabular{ll}{
  \code{\link{print.fitverse}}   \tab Concise best-fit summary \cr
  \code{\link{summary.fitverse}} \tab Full diagnostic report to the console \cr
  \code{\link{plot.fitverse}}    \tab Diagnostic plots (density, Q-Q, P-P, CDF, etc.) \cr
  \code{\link{coef.fitverse}}    \tab Extract parameter estimates \cr
  \code{\link{logLik.fitverse}}  \tab Extract log-likelihood \cr
  \code{\link{AIC.fitverse}}     \tab Extract AIC \cr
  \code{\link{BIC.fitverse}}     \tab Extract BIC \cr
  \code{\link{as.list.fitverse}} \tab Convert to a serialisation-ready list \cr
}
}

\section{Supported distributions (52 families)}{
\tabular{lll}{
  \strong{Shape}   \tab \strong{Distribution}                  \tab \strong{Key} \cr
  Symmetric        \tab Normal                                  \tab \code{normal} \cr
  Symmetric        \tab Logistic                                \tab \code{logistic} \cr
  Symmetric        \tab Cauchy                                  \tab \code{cauchy} \cr
  Symmetric        \tab Laplace                                 \tab \code{laplace} \cr
  Symmetric        \tab Student-t (location-scale)              \tab \code{student_t} \cr
  Symmetric        \tab Truncated Normal                        \tab \code{truncnorm} \cr
  Asymmetric       \tab Skew-Normal                             \tab \code{skewnormal} \cr
  Asymmetric       \tab Generalised Normal                      \tab \code{gnorm} \cr
  Asymmetric       \tab Asymmetric Laplace                      \tab \code{alaplace} \cr
  Asymmetric       \tab Johnson SU                              \tab \code{johnsonsu} \cr
  Right-skewed     \tab Gamma                                   \tab \code{gamma} \cr
  Right-skewed     \tab Gamma (3-parameter)                     \tab \code{gamma3} \cr
  Right-skewed     \tab Log-Normal                              \tab \code{lognormal} \cr
  Right-skewed     \tab Log-Normal (3-parameter)                \tab \code{lognormal3} \cr
  Right-skewed     \tab Exponential                             \tab \code{exponential} \cr
  Right-skewed     \tab Exponentiated Exponential               \tab \code{expexp} \cr
  Right-skewed     \tab Weibull (2-parameter)                   \tab \code{weibull} \cr
  Right-skewed     \tab Weibull (3-parameter)                   \tab \code{weibull3p} \cr
  Right-skewed     \tab Exponentiated Weibull                   \tab \code{expweibull} \cr
  Right-skewed     \tab Log-Logistic                            \tab \code{loglogistic} \cr
  Right-skewed     \tab Inverse Gaussian                        \tab \code{invgaussian} \cr
  Right-skewed     \tab Inverse Gamma                           \tab \code{invgamma} \cr
  Right-skewed     \tab Inverse Weibull                         \tab \code{invweibull} \cr
  Right-skewed     \tab Gompertz                                \tab \code{gompertz} \cr
  Right-skewed     \tab Birnbaum-Saunders                       \tab \code{birnbaum_saunders} \cr
  Right-skewed     \tab Rayleigh                                \tab \code{rayleigh} \cr
  Right-skewed     \tab Half-Normal                             \tab \code{halfnormal} \cr
  Right-skewed     \tab Lindley                                 \tab \code{lindley} \cr
  Right-skewed     \tab Nakagami                                \tab \code{nakagami} \cr
  Heavy-tailed     \tab Pareto                                  \tab \code{pareto} \cr
  Heavy-tailed     \tab Burr XII                                \tab \code{burr} \cr
  Heavy-tailed     \tab Lomax (Pareto Type II)                  \tab \code{lomax} \cr
  Heavy-tailed     \tab Paralogistic                            \tab \code{paralogistic} \cr
  Heavy-tailed     \tab Dagum                                   \tab \code{dagum} \cr
  Heavy-tailed     \tab GB2 (Generalised Beta II)               \tab \code{gb2} \cr
  Heavy-tailed     \tab Normal Inverse Gaussian                 \tab \code{nig} \cr
  Heavy-tailed     \tab Generalised Hyperbolic                  \tab \code{ghyp} \cr
  Heavy-tailed     \tab Generalised Gamma                       \tab \code{gengamma} \cr
  Extreme value    \tab Generalised Extreme Value               \tab \code{gev} \cr
  Extreme value    \tab Gumbel                                  \tab \code{gumbel} \cr
  Extreme value    \tab Generalised Pareto                      \tab \code{gpd} \cr
  Extreme value    \tab Generalised Logistic                    \tab \code{glo} \cr
  Extreme value    \tab Pearson Type III                        \tab \code{pearson3} \cr
  Extreme value    \tab Log-Pearson Type III                    \tab \code{lpearson3} \cr
  Extreme value    \tab Kappa-4                                 \tab \code{kap4} \cr
  Extreme value    \tab Wakeby                                  \tab \code{wakeby} \cr
  Bounded          \tab Beta                                    \tab \code{beta} \cr
  Bounded          \tab Uniform                                 \tab \code{uniform} \cr
  Bounded          \tab Triangular                              \tab \code{triangular} \cr
  Bounded          \tab Right Triangular                        \tab \code{rtriangle} \cr
  Bounded          \tab PERT                                    \tab \code{pert} \cr
  Bounded          \tab Kumaraswamy                             \tab \code{kumaraswamy} \cr
}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} (University of Auckland, New Zealand) \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj} (University of the South Pacific, Fiji)

Maintainer: Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz}
}

\references{
Hosking, J. R. M. (1990). L-moments: Analysis and estimation of distributions
using linear combinations of order statistics.
\emph{Journal of the Royal Statistical Society, Series B}, \bold{52}(1), 105--124.
\doi{10.1111/j.2517-6161.1990.tb01775.x}

Delignette-Muller, M. L. and Dutang, C. (2015).
fitdistrplus: An R package for fitting distributions.
\emph{Journal of Statistical Software}, \bold{64}(4), 1--34.
\doi{10.18637/jss.v064.i04}

Akaike, H. (1974). A new look at the statistical model identification.
\emph{IEEE Transactions on Automatic Control}, \bold{19}(6), 716--723.
\doi{10.1109/TAC.1974.1100705}
}

\seealso{
\code{\link{fitverse}}, \code{\link{fitverse_distributions}},
\code{\link{fitverse_grouped}}, \code{\link{FitVerse_datasets}},
\pkg{fitdistrplus}, \pkg{actuar}, \pkg{lmomco}
}

\examples{
## Quick start: fit three distributions to simulated Gamma data
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)

fit <- fitverse(x,
                dists   = c("gamma", "lognormal", "weibull"),
                method  = "MLE",
                xlab    = "Waiting time (s)",
                plot    = FALSE,
                verbose = FALSE)

print(fit)

## See all 52 supported distributions
fitverse_distributions()
}

\keyword{package}
\keyword{distribution}
\keyword{univar}

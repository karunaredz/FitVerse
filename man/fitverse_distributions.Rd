\name{fitverse_distributions}
\alias{fitverse_distributions}
\title{List All Distributions Supported by FitVerse}

\description{
Returns a data frame describing all 52 probability distributions that
\pkg{FitVerse} can fit, including their distribution keys, full names,
number of parameters, support, available estimation methods, and the
underlying R function source.

This page also documents the mathematical details of the four
hydrological / extreme-value distributions:
Generalised Pareto (GPD), Pearson Type III (P3), Log-Pearson Type III
(LP3), and Generalised Logistic (GLO).
}

\usage{
fitverse_distributions(method = NULL)
}

\arguments{
  \item{method}{
    Optional character scalar: filter to distributions available for a
    specific estimation method.  One of \code{"MLE"}, \code{"MOM"}, or
    \code{"LMOM"}.  If \code{NULL} (default), all 52 distributions are
    returned.
  }
}

\value{
A data frame with one row per distribution and columns:

\describe{
  \item{Key}{Character: the string to pass in the \code{dists} argument
    of \code{\link{fitverse}()} and related functions.}
  \item{Name}{Full distribution name.}
  \item{Family}{Broad family tag (e.g. \code{"Extreme-value"},
    \code{"Right-skewed"}).}
  \item{Package}{The R package supplying \code{d/p/q/r} functions, or
    \code{NA} for distributions with FitVerse-internal implementations.}
  \item{MLE}{Character \code{"Yes"} or \code{"-"}: MLE available?}
  \item{MOM}{Character \code{"Yes"} or \code{"-"}: MOM available?}
  \item{LMOM}{Character \code{"Yes"} or \code{"-"}: L-Moments available?}
}
}

% ============================================================
% SECTION 1 -- COMPLETE DISTRIBUTION TABLE (all 52)
% ============================================================
\details{
\strong{Complete distribution table (52 families)}

\tabular{lllll}{
  \strong{Key}              \tab \strong{Distribution}             \tab \strong{Params} \tab \strong{Support}               \tab \strong{Family}    \cr
  \code{normal}             \tab Normal                            \tab 2 \tab \eqn{(-\infty,\infty)}       \tab Symmetric      \cr
  \code{logistic}           \tab Logistic                          \tab 2 \tab \eqn{(-\infty,\infty)}       \tab Symmetric      \cr
  \code{cauchy}             \tab Cauchy                            \tab 2 \tab \eqn{(-\infty,\infty)}       \tab Symmetric      \cr
  \code{laplace}            \tab Laplace                           \tab 2 \tab \eqn{(-\infty,\infty)}       \tab Symmetric      \cr
  \code{student_t}          \tab Student-t (location-scale)        \tab 3 \tab \eqn{(-\infty,\infty)}       \tab Symmetric      \cr
  \code{truncnorm}          \tab Truncated Normal                  \tab 4 \tab \eqn{[a,b]}                  \tab Symmetric      \cr
  \code{skewnormal}         \tab Skew-Normal                       \tab 3 \tab \eqn{(-\infty,\infty)}       \tab Asymmetric     \cr
  \code{gnorm}              \tab Generalised Normal                \tab 3 \tab \eqn{(-\infty,\infty)}       \tab Asymmetric     \cr
  \code{alaplace}           \tab Asymmetric Laplace                \tab 3 \tab \eqn{(-\infty,\infty)}       \tab Asymmetric     \cr
  \code{johnsonsu}          \tab Johnson SU                        \tab 4 \tab \eqn{(-\infty,\infty)}       \tab Asymmetric     \cr
  \code{gamma}              \tab Gamma                             \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{gamma3}             \tab Gamma (3-parameter)               \tab 3 \tab \eqn{(\mu,\infty)}           \tab Right-skewed   \cr
  \code{lognormal}          \tab Log-Normal                        \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{lnorm3}             \tab Log-Normal (3-parameter)          \tab 3 \tab \eqn{(\mu,\infty)}           \tab Right-skewed   \cr
  \code{exponential}        \tab Exponential                       \tab 1 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{expexp}             \tab Exponentiated Exponential         \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{weibull}            \tab Weibull (2-parameter)             \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{weibull3p}          \tab Weibull (3-parameter)             \tab 3 \tab \eqn{(\mu,\infty)}           \tab Right-skewed   \cr
  \code{expweibull}         \tab Exponentiated Weibull             \tab 3 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{loglogistic}        \tab Log-Logistic                      \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{invgaussian}        \tab Inverse Gaussian                  \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{invgamma}           \tab Inverse Gamma                     \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{invweibull}         \tab Inverse Weibull                   \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{gompertz}           \tab Gompertz                          \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{birnbaum_saunders}  \tab Birnbaum-Saunders                 \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{rayleigh}           \tab Rayleigh                          \tab 1 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{halfnormal}         \tab Half-Normal                       \tab 1 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{lindley}            \tab Lindley                           \tab 1 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{nakagami}           \tab Nakagami                          \tab 2 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{gengamma}           \tab Generalised Gamma                 \tab 3 \tab \eqn{(0,\infty)}             \tab Right-skewed   \cr
  \code{pareto}             \tab Pareto                            \tab 2 \tab \eqn{(x_m,\infty)}           \tab Heavy-tailed   \cr
  \code{burr}               \tab Burr XII                          \tab 3 \tab \eqn{(0,\infty)}             \tab Heavy-tailed   \cr
  \code{lomax}              \tab Lomax (Pareto Type II)            \tab 2 \tab \eqn{(0,\infty)}             \tab Heavy-tailed   \cr
  \code{paralogistic}       \tab Paralogistic                      \tab 2 \tab \eqn{(0,\infty)}             \tab Heavy-tailed   \cr
  \code{dagum}              \tab Dagum                             \tab 3 \tab \eqn{(0,\infty)}             \tab Heavy-tailed   \cr
  \code{gb2}                \tab GB2 (Generalised Beta II)         \tab 4 \tab \eqn{(0,\infty)}             \tab Heavy-tailed   \cr
  \code{nig}                \tab Normal Inverse Gaussian           \tab 4 \tab \eqn{(-\infty,\infty)}       \tab Heavy-tailed   \cr
  \code{ghyp}               \tab Generalised Hyperbolic            \tab 5 \tab \eqn{(-\infty,\infty)}       \tab Heavy-tailed   \cr
  \code{gev}                \tab Generalised Extreme Value         \tab 3 \tab depends on shape             \tab Extreme-value  \cr
  \code{gumbel}             \tab Gumbel (Type I EV)                \tab 2 \tab \eqn{(-\infty,\infty)}       \tab Extreme-value  \cr
  \code{gpd}                \tab Generalised Pareto                \tab 3 \tab \eqn{[\mu,\infty)} or bdd    \tab Extreme-value  \cr
  \code{glo}                \tab Generalised Logistic              \tab 3 \tab depends on shape             \tab Extreme-value  \cr
  \code{pearson3}           \tab Pearson Type III                  \tab 3 \tab \eqn{(-\infty,\infty)}       \tab Extreme-value  \cr
  \code{lpearson3}          \tab Log-Pearson Type III              \tab 3 \tab \eqn{(0,\infty)}             \tab Extreme-value  \cr
  \code{kap4}               \tab Kappa-4                           \tab 4 \tab depends on shape             \tab Extreme-value  \cr
  \code{wakeby}             \tab Wakeby                            \tab 5 \tab \eqn{(0,\infty)}             \tab Extreme-value  \cr
  \code{beta}               \tab Beta                              \tab 2 \tab \eqn{(0,1)}                  \tab Bounded        \cr
  \code{uniform}            \tab Uniform                           \tab 2 \tab \eqn{[a,b]}                  \tab Bounded        \cr
  \code{triangular}         \tab Triangular                        \tab 3 \tab \eqn{[a,b]}                  \tab Bounded        \cr
  \code{rtriangle}          \tab Right Triangular                  \tab 2 \tab \eqn{[a,b]}                  \tab Bounded        \cr
  \code{pert}               \tab PERT (Beta-PERT)                  \tab 3 \tab \eqn{[a,b]}                  \tab Bounded        \cr
  \code{kumaraswamy}        \tab Kumaraswamy                       \tab 2 \tab \eqn{(0,1)}                  \tab Bounded        \cr
}

% ============================================================
% SECTION 2 -- HYDROLOGICAL DISTRIBUTIONS: MATHEMATICAL DETAIL
%   (GPD, Pearson III, Log-Pearson III, GLO)
%   Added in FitVerse v1.0.  All four use FitVerse-internal
%   closed-form d/p/q/r implementations; no external package
%   is required beyond lmomco (already an Import).
% ============================================================

\strong{Hydrological and extreme-value distributions (v1.0 additions)}

The four distributions below -- GPD, Pearson III, Log-Pearson III, and
GLO -- are central to flood-frequency analysis and Peaks-Over-Threshold
modelling.  They share a common implementation strategy: closed-form
\code{d/p/q/r} functions are registered in the internal
\code{.FITVERSE_DPQR_OVERRIDES} table, bypassing any external package
dependency and guaranteeing consistent behaviour across platforms.


% ------ GPD ------
\subsection{Generalised Pareto Distribution (GPD) -- key: \code{"gpd"}}{

\strong{Parameters:} \eqn{\mu} (location / threshold), \eqn{\sigma > 0}
(scale), \eqn{\xi} (shape).

\strong{PDF} (\eqn{\xi \neq 0}):
\deqn{f(x\,;\,\mu,\sigma,\xi)=\frac{1}{\sigma}
  \Bigl(1+\xi\,\frac{x-\mu}{\sigma}\Bigr)^{-1/\xi-1}}

Support: \eqn{x \ge \mu} when \eqn{\xi \ge 0};
\eqn{\mu \le x \le \mu - \sigma/\xi} when \eqn{\xi < 0}.
Reduces to \eqn{\mathrm{Exp}(1/\sigma)} when \eqn{\xi = 0}.

\strong{Quantile function:}
\deqn{Q(p)=\mu+\frac{\sigma}{\xi}\bigl[(1-p)^{-\xi}-1\bigr],\quad\xi\neq 0
  \qquad Q(p)=\mu-\sigma\log(1-p),\quad\xi=0}

\strong{Method of Moments:} Fix \eqn{\hat\mu=\min(x)-\varepsilon};
let \eqn{y_i=x_i-\hat\mu}.  Then
\eqn{\hat\xi=(1-\bar y^2/s_y^2)/2},
\eqn{\hat\sigma=\bar y(1-\hat\xi)}.
Valid only when \eqn{\hat\xi<1/2}.

\strong{L-Moments:} via \code{lmomco::pargpa()}.
Sign convention: FitVerse \eqn{\xi=-\kappa_{\text{lmomco}}}.

\strong{Use case:} Peaks-Over-Threshold analysis; insurance excess-of-loss
modelling (Pickands, 1975).
}


% ------ Pearson III ------
\subsection{Pearson Type III (P3) -- key: \code{"pearson3"}}{

\strong{Parameters:} \eqn{\alpha>0} (shape), \eqn{\beta} (scale; may be
negative), \eqn{\zeta} (location).

\strong{Relationship to Gamma:}
\deqn{X\sim\mathrm{P3}(\alpha,\beta,\zeta)
  \;\Leftrightarrow\;
  (X-\zeta)/\beta\sim\mathrm{Gamma}(\alpha,\,\mathrm{rate}=1)}

\strong{PDF:}
\deqn{f(x\,;\,\alpha,\beta,\zeta)=
  \frac{|\beta|^{-1}}{\Gamma(\alpha)}
  \Bigl(\frac{x-\zeta}{\beta}\Bigr)^{\!\alpha-1}
  \exp\!\Bigl(-\frac{x-\zeta}{\beta}\Bigr),
  \quad\frac{x-\zeta}{\beta}>0}

Positive \eqn{\beta} gives right-skewed; negative \eqn{\beta} gives
left-skewed data.

\strong{Method of Moments:} Let \eqn{\bar x,s,g} be the sample mean,
standard deviation, and skewness coefficient.  Then:
\deqn{\hat\alpha=4/g^2,\qquad\hat\beta=sg/2,\qquad\hat\zeta=\bar x-2s/g}

\strong{L-Moments:} via \code{lmomco::parpe3()}, which returns
\eqn{(\mu,\sigma,\gamma)}.  FitVerse converts using the MOM formulae above.

\strong{Use case:} Annual peak-flow frequency analysis (US Bulletin 17C;
England \emph{et al.}, 2019); meteorological extremes.
}


% ------ Log-Pearson III ------
\subsection{Log-Pearson Type III (LP3) -- key: \code{"lpearson3"}}{

\strong{Parameters:} \eqn{\alpha>0} (shape), \eqn{\beta} (scale),
\eqn{\zeta} (location) -- all defined in log-space.

\deqn{X\sim\mathrm{LP3}(\alpha,\beta,\zeta)
  \;\Leftrightarrow\;
  \log X\sim\mathrm{P3}(\alpha,\beta,\zeta)}

\strong{PDF:}
\deqn{f(x\,;\,\alpha,\beta,\zeta)
  =\frac{f_{\mathrm{P3}}(\log x\,;\,\alpha,\beta,\zeta)}{x},\quad x>0}

\strong{CDF and quantile:}
\deqn{F(x)=F_{\mathrm{P3}}(\log x),\qquad Q(p)=\exp\bigl[Q_{\mathrm{P3}}(p)\bigr]}

\strong{Method of Moments and L-Moments:} Pearson III estimators applied
to \eqn{\log x_i}.  L-Moments use \code{lmomco::parpe3()} on the
log-transformed sample.

\strong{Use case:} The US federal standard for flood-frequency estimation
(Bulletin 17C; England \emph{et al.}, 2019).
}


% ------ GLO ------
\subsection{Generalised Logistic Distribution (GLO) -- key: \code{"glo"}}{

\strong{Parameters:} \eqn{\xi} (location), \eqn{\alpha>0} (scale),
\eqn{\kappa} (shape).

\strong{CDF} (\eqn{\kappa\neq 0}):
\deqn{F(x\,;\,\xi,\alpha,\kappa)
  =\frac{1}{1+\bigl[1-\kappa(x-\xi)/\alpha\bigr]^{1/\kappa}}}

Reduces to \eqn{\mathrm{Logistic}(\xi,\alpha)} when \eqn{\kappa=0}.

\strong{Quantile function:}
\deqn{Q(p)=\xi+\frac{\alpha}{\kappa}\Bigl[1-\Bigl(\frac{1-p}{p}\Bigr)^{\!\kappa}\Bigr],
  \quad\kappa\neq 0
  \qquad Q(p)=\xi+\alpha\log\frac{p}{1-p},\quad\kappa=0}

Support: \eqn{(-\infty,\,\xi+\alpha/\kappa)} for \eqn{\kappa>0};
\eqn{(\xi+\alpha/\kappa,\,\infty)} for \eqn{\kappa<0};
all real for \eqn{\kappa=0}.

\strong{L-Moments:} via \code{lmomco::parglo()}.
No sign flip: FitVerse \eqn{\kappa=\kappa_{\text{lmomco}}}.

MOM is not implemented for GLO (complex moment system; use LMOM or MLE).

\strong{Use case:} The recommended flood-frequency distribution in the
United Kingdom (WINFAP; Robson & Reed, 1999).
}


% ------ Sign convention table ------
\subsection{L-moment shape-parameter sign conventions}{

\pkg{lmomco} uses the opposite sign for the shape parameter of
GEV and GPD:

\tabular{lll}{
  \strong{Distribution} \tab \strong{lmomco \eqn{\kappa}} \tab
    \strong{FitVerse shape}   \cr
  GEV                   \tab \eqn{\kappa}                  \tab
    \eqn{-\kappa} (sign flip) \cr
  GPD                   \tab \eqn{\kappa}                  \tab
    \eqn{-\kappa} (sign flip) \cr
  GLO                   \tab \eqn{\kappa}                  \tab
    \eqn{\kappa}  (no flip)   \cr
}
}

}% end \details

\seealso{
\code{\link{fitverse}} for fitting any of the 52 distributions. \cr
\code{\link{compare_dists}} for quick multi-distribution screening. \cr
\code{\link{return_level}} for T-year return levels. \cr
\code{\link{bootstrap_ci}} for parametric bootstrap confidence intervals.
}

\examples{
# All 52 distributions
fitverse_distributions()

# Only those with L-moment support
fitverse_distributions(method = "LMOM")

# Extreme-value / hydrological families
ev <- fitverse_distributions()
ev[ev$Key \%in\% c("gev","gumbel","gpd","pearson3","lpearson3","glo"), ]

## ---- GPD (Peaks-Over-Threshold) ----------------------------------------
set.seed(42)
x_excess <- evd::rgpd(300, loc = 0, scale = 2, shape = 0.1)
fit_gpd   <- fitverse(x_excess, dists = "gpd", method = "LMOM",
                      plot = FALSE, verbose = FALSE)
coef(fit_gpd)
return_level(fit_gpd, return_periods = c(10, 50, 100))

## ---- Pearson III (Bulletin 17C style) ----------------------------------
## Pearson III with shape=5, scale=3, loc=10 is equivalent to
## a shifted Gamma: X = loc + Gamma(shape, scale)
set.seed(42)
x_p3  <- rgamma(200, shape = 5, scale = 3) + 10
fit_p3 <- fitverse(x_p3, dists = "pearson3", method = "LMOM",
                   plot = FALSE, verbose = FALSE)
coef(fit_p3)

## ---- Log-Pearson III ----------------------------------------------------
## Log-Pearson III: log(X) ~ Pearson III(shape, scale, loc)
## so X = exp(loc + Gamma(shape, scale))
set.seed(42)
x_lp3  <- exp(rgamma(200, shape = 5, scale = 0.4) + 2)
fit_lp3 <- fitverse(x_lp3, dists = "lpearson3", method = "LMOM",
                    plot = FALSE, verbose = FALSE)
coef(fit_lp3)

## ---- GLO (UK WINFAP style) ----------------------------------------------
## Generate GLO samples via the lmomco quantile function
set.seed(42)
x_glo  <- lmomco::qlmomco(runif(200),
             lmomco::vec2par(c(50, 8, 0.12), type = "glo"))
fit_glo <- fitverse(x_glo, dists = "glo", method = "LMOM",
                    plot = FALSE, verbose = FALSE)
coef(fit_glo)
return_level(fit_glo, return_periods = c(2, 10, 50, 100, 200))
}

\references{
England, J. F., Jr., et al. (2019).
\emph{Guidelines for Determining Flood Flow Frequency -- Bulletin 17C.}
USGS Techniques and Methods, Book 4, Chapter B5.

Hosking, J. R. M. and Wallis, J. R. (1997).
\emph{Regional Frequency Analysis: An Approach Based on L-moments.}
Cambridge University Press.

Pickands, J. (1975). Statistical inference using extreme order statistics.
\emph{The Annals of Statistics}, \strong{3}(1), 119--131.

Robson, A. and Reed, D. (1999).
\emph{Flood Estimation Handbook, Volume 3.}
Institute of Hydrology, Wallingford, UK.
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}
\keyword{models}

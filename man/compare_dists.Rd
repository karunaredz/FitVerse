\name{compare_dists}
\alias{compare_dists}
\title{Compare Distribution Fits and Return a Ranking Table}

\description{
Fits one or more parametric distributions to a numeric vector and returns
an ordered ranking table without producing a plot or a full \code{fitverse}
object.  Use this function when you want a quick tabular comparison and
do not need the fitted-object infrastructure.
}

\usage{
compare_dists(x,
              dists     = "all",
              method    = "MLE",
              criterion = "AIC",
              xlab      = "x",
              verbose   = TRUE)
}

\arguments{
  \item{x}{
    A numeric vector of observations.  Non-finite values are removed
    automatically before fitting.
  }
  \item{dists}{
    Character vector of distribution keys, or \code{"all"} to try every
    compatible distribution.  See \code{\link{fitverse_distributions}()}.
  }
  \item{method}{
    Character vector: one or more of \code{"MLE"}, \code{"MOM"},
    \code{"LMOM"}.  All results are combined into a single table when
    multiple methods are specified.
  }
  \item{criterion}{
    \code{"AIC"} (default) or \code{"BIC"}: the criterion by which rows
    are sorted.
  }
  \item{xlab}{
    Character: a label for the data variable, passed through to any
    downstream functions.  Default \code{"x"}.
  }
  \item{verbose}{
    Logical: print progress messages?  Default \code{TRUE}.
  }
}

\details{
\code{compare_dists()} calls the same internal fitting engine as
\code{\link{fitverse}()} but skips GoF test computation, data-summary
computation, and plotting.  It is therefore faster and better suited to
exploratory screening of many distributions or methods.

The returned data frame can be used directly with \code{\link[knitr]{kable}}
or \code{\link[DT]{datatable}} for reporting.
}

\value{
A data frame with one row per distribution-method combination, ordered by
the chosen information criterion (ascending), with columns:

\describe{
  \item{Rank}{Integer rank (1 = best).}
  \item{Distribution}{Full distribution name.}
  \item{Method}{Estimation method (\code{"MLE"}, \code{"MOM"}, or \code{"LMOM"}).}
  \item{N_params}{Number of free parameters.}
  \item{LogLik}{Maximised log-likelihood.}
  \item{AIC}{Akaike Information Criterion.}
  \item{Delta_AIC}{AIC difference relative to the best model.}
  \item{BIC}{Bayesian Information Criterion.}
  \item{Delta_BIC}{BIC difference relative to the best model.}
  \item{Converged}{Logical: did the optimiser converge?}
  \item{dist_key}{Internal distribution key (character).}
}
}

\seealso{
\code{\link{fitverse}} for a full fitted object plus diagnostic plot.
}

\examples{
set.seed(1)
x <- rgamma(200, shape = 3, rate = 0.5)

tbl <- compare_dists(x,
                     dists   = c("gamma", "lognormal", "weibull", "exponential"),
                     method  = "MLE",
                     verbose = FALSE)
head(tbl)
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}
\keyword{models}

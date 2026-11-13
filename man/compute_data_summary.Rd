\name{compute_data_summary}
\alias{compute_data_summary}
\title{Compute a Descriptive Statistics Summary}

\description{
\code{compute_data_summary()} computes a comprehensive set of descriptive
statistics for a numeric vector and returns them as a named list.
It is called automatically by \code{\link{fitverse}()} and
\code{\link{summary.fitverse}()}, but can also be used independently for
exploratory data analysis.
}

\usage{
compute_data_summary(x, xlab = "x", n_raw = NULL)
}

\arguments{
  \item{x}{
    A numeric vector, already cleaned of non-finite values (or not -- the
    function removes them internally).
  }
  \item{xlab}{
    Character: variable name, stored in the returned list for labelling.
    Default \code{"x"}.
  }
  \item{n_raw}{
    Integer or \code{NULL}: the original sample size before cleaning.
    If supplied, \code{n_missing} is computed as \code{n_raw - n}.
    If \code{NULL}, \code{n_missing} is set to \code{NA}.
  }
}

\value{
\code{compute_data_summary()} returns a named list with the following
elements:

\describe{
  \item{\code{n}}{Integer: number of finite (non-missing) observations used.}
  \item{\code{n_missing}}{Integer or \code{NA}: number of non-finite values removed.}
  \item{\code{mean}}{Arithmetic mean.}
  \item{\code{sd}}{Standard deviation (denominator \eqn{n-1}).}
  \item{\code{cv}}{Coefficient of variation (\code{sd / mean}).
    \code{NA} when \code{mean == 0}.}
  \item{\code{median}}{Sample median.}
  \item{\code{iqr}}{Interquartile range (\eqn{Q_{0.75} - Q_{0.25}}).}
  \item{\code{skewness}}{Sample skewness (Fisher's \eqn{g_1}).}
  \item{\code{kurtosis}}{Excess kurtosis (Fisher's \eqn{g_2}).}
  \item{\code{min}}{Sample minimum.}
  \item{\code{max}}{Sample maximum.}
  \item{\code{range}}{Range (\code{max - min}).}
  \item{\code{q25}}{25th percentile.}
  \item{\code{q75}}{75th percentile.}
  \item{\code{xlab}}{Character: the variable label.}
}

}

\details{
Skewness and kurtosis are computed using the bias-corrected (unbiased)
estimators:
\deqn{g_1 = \frac{n}{(n-1)(n-2)} \sum_{i=1}^n
      \left(\frac{x_i - \bar{x}}{s}\right)^3}
\deqn{g_2 = \frac{n(n+1)}{(n-1)(n-2)(n-3)}
      \sum_{i=1}^n \left(\frac{x_i - \bar{x}}{s}\right)^4
      - \frac{3(n-1)^2}{(n-2)(n-3)}}
These correspond to the \code{skewness} and \code{kurtosis} functions in
the \pkg{moments} package (type 3 in \code{e1071}) and are consistent with
the convention in \emph{Applied Statistics} textbooks.  The Normal
distribution has skewness 0 and excess kurtosis 0.
}

\seealso{
\code{\link{fitverse}} (calls this automatically),
\code{\link{summary.fitverse}}.
}

\examples{
set.seed(3)
x <- rlnorm(500, meanlog = 1, sdlog = 0.5)

ds <- compute_data_summary(x, xlab = "Claim amount", n_raw = 510)
ds$skewness
ds$cv
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{univar}
\keyword{descriptive}

\name{ranking}
\alias{ranking}
\alias{ranking.fitverse}

\title{Extract the Ranking Table from a Fitted Object}

\description{
Retrieves the full ranking table from a \code{fitverse} object without
using the \code{$} operator.  The table contains one row per distribution
fitted, sorted by the selection criterion (AIC or BIC).
}

\usage{
ranking(object, \dots)
\method{ranking}{fitverse}(object, \dots)
}

\arguments{
  \item{object}{A \code{fitverse} object.}
  \item{\dots}{Passed to methods.}
}

\value{
A data frame with columns including \code{Rank}, \code{Distribution},
\code{Method}, \code{AIC}, \code{Delta_AIC}, \code{BIC}, \code{KS_p},
\code{AD_p}, and others depending on the methods used.
}

\examples{
\donttest{
  set.seed(1)
  x   <- rgamma(200, shape = 2, rate = 0.5)
  fit <- fitverse(x)
  r   <- ranking(fit)
  head(r)
}
}

\seealso{
\code{\link{fitverse}}, \code{\link{all_fits}}, \code{\link{summary.fitverse}}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{methods}

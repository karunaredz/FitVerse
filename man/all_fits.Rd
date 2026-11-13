\name{all_fits}
\alias{all_fits}
\alias{all_fits.fitverse}

\title{Extract All Individual Fit Results from a Fitted Object}

\description{
Retrieves the list of per-distribution fit results stored inside a
\code{fitverse} object.  Each element corresponds to one model that
was attempted during the fitting process.
}

\usage{
all_fits(object, \dots)
\method{all_fits}{fitverse}(object, \dots)
}

\arguments{
  \item{object}{A \code{fitverse} object.}
  \item{\dots}{Passed to methods.}
}

\value{
A named list of fit results, one per distribution attempted.
}

\examples{
\donttest{
  set.seed(1)
  x    <- rgamma(200, shape = 2, rate = 0.5)
  fit  <- fitverse(x)
  fits <- all_fits(fit)
  length(fits)
}
}

\seealso{
\code{\link{fitverse}}, \code{\link{ranking}}, \code{\link{summary.fitverse}}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{methods}

\name{fitverse_summary}
\alias{fitverse_summary}
\alias{summary.fitverse}
\alias{print.fitverse_summary}

\title{Summary of a fitverse Object}

\description{
\code{summary.fitverse} returns an object of class \code{"fitverse_summary"}
containing the best-fit results and a ranked table of all models fitted.
Printing the object (via \code{print.fitverse_summary}) displays a
formatted diagnostic report; assigning the result to a variable suppresses
output.
}

\usage{
\method{summary}{fitverse}(object, top_n = 10L, \dots)
\method{print}{fitverse_summary}(x, \dots)
}

\arguments{
  \item{object}{A \code{fitverse} object.}
  \item{top_n}{Integer: number of rows to show in the ranking table.
    Default \code{10}.}
  \item{x}{A \code{fitverse_summary} object.}
  \item{\dots}{Ignored.}
}

\value{
An object of class \code{"fitverse_summary"}, a list with components:
\describe{
  \item{\code{object}}{The original \code{fitverse} object.}
  \item{\code{top_n}}{The number of rows requested.}
  \item{\code{data_summary}}{Data summary list (or \code{NULL}).}
  \item{\code{ranking}}{Data frame with the top \code{top_n} rows of the
    ranking table.}
}
\code{print.fitverse_summary} returns \code{x} invisibly.
}

\examples{
\donttest{
  set.seed(1)
  x   <- rgamma(200, shape = 2, rate = 0.5)
  fit <- fitverse(x)

  # Assign without printing:
  s <- summary(fit)

  # Inspect individual components:
  head(s$ranking)
  s$data_summary$mean

  # Print explicitly:
  print(s)
}
}

\seealso{
\code{\link{fitverse}}, \code{\link{print.fitverse}},
\code{\link{ranking}}, \code{\link{all_fits}}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{methods}

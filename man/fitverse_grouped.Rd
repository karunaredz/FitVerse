\name{fitverse_grouped}
\alias{fitverse_grouped}
\alias{read_grouped_csv}

\title{Fit Distributions to Grouped or Interval-Censored Data}

\description{
\code{fitverse_grouped()} fits one or more parametric distributions to
data that are available only in grouped (frequency-table) or
interval-censored form.  It accepts either class midpoints with counts, or
explicit lower and upper interval boundaries with counts.

\code{read_grouped_csv()} reads a two- or three-column CSV file into the
format expected by \code{fitverse_grouped()}.
}

\usage{
fitverse_grouped(midpoints = NULL,
                 counts,
                 lower   = NULL,
                 upper   = NULL,
                 dists   = "auto",
                 method  = "All",
                 xlab    = "x",
                 top_n   = 5L,
                 verbose = FALSE)

read_grouped_csv(file, sep = ",")
}

\arguments{
  \item{midpoints}{Numeric vector of class midpoints (one per class).
    Supply either \code{midpoints} or both \code{lower} and \code{upper},
    but not both.}
  \item{counts}{Integer vector of observed frequencies for each class.
    Must be the same length as \code{midpoints} (or \code{lower}/\code{upper}),
    all non-negative, and sum to at least 10.}
  \item{lower}{Numeric vector of lower interval boundaries.  Required when
    \code{midpoints} is not supplied.}
  \item{upper}{Numeric vector of upper interval boundaries.  Required when
    \code{midpoints} is not supplied.  Each element must exceed the
    corresponding element of \code{lower}.}
  \item{dists}{Character vector of distribution names to fit, or
    \code{"auto"} (default) to use all distributions in the FitVerse
    registry that support MLE.}
  \item{method}{Fitting method: \code{"MLE"}, \code{"MOM"}, \code{"LMOM"},
    or \code{"All"} (default).}
  \item{xlab}{Character label for the data axis used in output summaries.
    Default \code{"x"}.}
  \item{top_n}{Integer: number of top-ranked distributions to include in
    the printed summary.  Default \code{5L}.}
  \item{verbose}{Logical: print per-distribution fitting progress?
    Default \code{FALSE}.}
  \item{file}{Path to a CSV file.  The file may contain comment lines
    beginning with \code{#}.}
  \item{sep}{Column separator for \code{read_grouped_csv()}.  Default
    \code{","}.}
}

\details{
\subsection{Input formats}{
\code{fitverse_grouped()} accepts two input formats.

\strong{Midpoint format:} Supply \code{midpoints} and \code{counts}.
Interval boundaries are reconstructed by assuming equal-width classes.

\strong{Boundary format:} Supply \code{lower}, \code{upper}, and
\code{counts}.  Classes need not be equal-width.  Open-ended classes
(e.g., an upper bound of \code{Inf}) are handled by replacing the
infinite bound with a large finite value for midpoint computation.
}

\subsection{Estimation}{
For each candidate distribution, parameters are estimated by maximising
the interval-censored log-likelihood
\deqn{
  \ell(\theta) = \sum_{j} n_j \log\bigl[F(u_j;\theta) - F(l_j;\theta)\bigr]
}
where \eqn{n_j} is the count in class \eqn{j}, and \eqn{l_j}, \eqn{u_j}
are its lower and upper boundaries.
}

\subsection{CSV format for \code{read_grouped_csv()}}{
The CSV file should have either two columns (midpoint, count) or three
columns (lower, upper, count).  Column names are detected automatically
from a set of common variants (\code{mid}, \code{midpoint}, \code{lower},
\code{upper}, etc.).  Lines starting with \code{#} are treated as comments
and ignored.
}
}

\value{
\code{fitverse_grouped()} returns an object of class
\code{"fitverse_grouped"} containing:
\describe{
  \item{\code{results}}{A data frame of fitted distributions ranked by
    AIC, with columns for distribution name, AIC, BIC, log-likelihood,
    estimated parameters, and goodness-of-fit statistics.}
  \item{\code{best}}{The name of the best-fitting distribution.}
  \item{\code{data}}{A list with elements \code{midpoints},
    \code{counts}, \code{lower}, and \code{upper}.}
  \item{\code{call}}{The matched call.}
}

\code{read_grouped_csv()} returns a named list with elements
\code{midpoints} and \code{counts} (midpoint format) or \code{lower},
\code{upper}, and \code{counts} (boundary format), ready to be passed
directly to \code{fitverse_grouped()}.
}

\seealso{
\code{\link{fitverse}} for fitting to individual (ungrouped) data. \cr
\code{\link{fitverse_distributions}} for the full distribution catalogue.
}

\examples{
\donttest{
## Midpoint format
mid   <- c(5, 15, 25, 35, 45, 55, 65, 75)
count <- c(3, 12, 25, 30, 18, 8, 3, 1)
fit <- fitverse_grouped(midpoints = mid, counts = count,
                        method = "MLE", verbose = FALSE)
print(fit)

## Boundary format
low  <- c(0, 10, 20, 30, 40, 50, 60, 70)
high <- c(10, 20, 30, 40, 50, 60, 70, 80)
fit2 <- fitverse_grouped(lower = low, upper = high, counts = count,
                         method = "MLE", verbose = FALSE)
print(fit2)

## Reading from CSV
## A file with columns: lower, upper, count
tmp <- tempfile(fileext = ".csv")
writeLines(c(
  "# Example grouped data",
  "lower,upper,count",
  "0,10,5",
  "10,20,18",
  "20,30,30",
  "30,40,25",
  "40,50,12",
  "50,60,4"
), tmp)
dat <- read_grouped_csv(tmp)
fit3 <- fitverse_grouped(lower = dat$lower, upper = dat$upper,
                         counts = dat$counts)
print(fit3)
}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}
\keyword{models}

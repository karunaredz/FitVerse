\name{to_json}
\alias{to_json}
\alias{to_json.fitverse}
\title{Serialise a \code{fitverse} Object to JSON}

\description{
Converts a \code{fitverse} object to a JSON string.  Useful for passing
fit results to web front-ends, storing results in document databases, or
integrating with REST APIs.

The serialisation uses \code{\link[jsonlite]{toJSON}()} on the output of
\code{\link{as.list.fitverse}()}, which strips non-serialisable internals
(raw optimiser fit objects, closures, environments) and returns a flat,
portable representation.
}

\usage{
to_json(x, pretty = TRUE, \dots)

\method{to_json}{fitverse}(x, pretty = TRUE, \dots)
}

\arguments{
  \item{x}{
    An object with a \code{to_json} method; currently
    \code{"fitverse"} objects are supported.
  }
  \item{pretty}{
    Logical: if \code{TRUE} (default), produce indented, human-readable
    JSON.  If \code{FALSE}, produce compact JSON (smaller output, faster
    parsing).
  }
  \item{\dots}{
    Additional arguments passed to \code{jsonlite::toJSON()}, for
    example \code{digits}, \code{na} or \code{null}.
  }
}

\details{
Requires the \pkg{jsonlite} package (listed in \code{Suggests}).
An informative error is raised if it is not installed.

The JSON output includes:
\itemize{
  \item \code{best_fit}: distribution key, method, parameter estimates,
    loglik, AIC, BIC.
  \item \code{ranking}: full ranking data frame.
  \item \code{data_summary}: descriptive statistics.
  \item \code{n}, \code{xlab}, \code{criterion}.
}

The raw data vector is deliberately \emph{not} included to keep the output
compact and privacy-friendly for web contexts.
}

\value{
A single character string containing the JSON representation of \code{x}.
}

\seealso{
\code{\link{as.list.fitverse}} for the underlying list conversion. \cr
\code{\link[jsonlite]{fromJSON}} to parse the JSON back to R.
}

\examples{
\donttest{
if (requireNamespace("jsonlite", quietly = TRUE)) {
  set.seed(42)
  x <- rgamma(300, shape = 2, rate = 0.5)
  fit <- fitverse(x, plot = FALSE, verbose = FALSE)

  json_str <- to_json(fit)
  cat(substr(json_str, 1, 400))

  # Round-trip back to R list
  lst <- jsonlite::fromJSON(json_str)
  lst$best_fit$params
}
}
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{IO}
\keyword{distribution}

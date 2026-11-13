\name{fitverse_batch}
\alias{fitverse_batch}
\alias{print.fitverse_batch}
\alias{summary.fitverse_batch}
\title{Fit Distributions to All Columns of a Data Frame}

\description{
Applies \code{\link{fitverse}()} to every numeric column of a data frame
(or matrix, or named list), collects the results, and returns a tidy
summary table alongside the full list of fitted objects.

Use \code{fitverse_batch()} for automated data characterisation workflows,
survey-population profiling, or as a pre-processing step before optimal
stratification with \pkg{stratifyR}.
}

\usage{
fitverse_batch(data,
               cols      = NULL,
               dists     = "all",
               method    = "MLE",
               criterion = "AIC",
               top_n     = 3L,
               plot      = FALSE,
               progress  = NULL,
               verbose   = TRUE)

\method{print}{fitverse_batch}(x, \dots)

\method{summary}{fitverse_batch}(object, \dots)
}

\arguments{
  \item{data}{
    A \code{data.frame}, \code{matrix}, or named \code{list} of numeric
    vectors.  Non-numeric columns are silently ignored.
  }
  \item{cols}{
    Character vector of column names to process.  If \code{NULL}
    (default), all numeric columns are processed.
  }
  \item{dists}{
    Distribution keys to try, or \code{"all"}.  Passed to
    \code{\link{fitverse}()}.
  }
  \item{method}{
    Estimation method(s): \code{"MLE"}, \code{"MOM"}, \code{"LMOM"}, or
    a character vector of multiple methods.  Default \code{"MLE"}.
  }
  \item{criterion}{
    \code{"AIC"} or \code{"BIC"}.  Default \code{"AIC"}.
  }
  \item{top_n}{
    Integer: number of top-ranked distributions stored in each fit.
    Default \code{3}.
  }
  \item{plot}{
    Logical: produce and print a diagnostic plot for each variable?
    Default \code{FALSE} (recommended for batch use).
  }
  \item{progress}{
    Optional callback \code{function(p)} for reporting overall batch
    progress, where \code{p} is a value in \eqn{[0, 1]}.
  }
  \item{verbose}{
    Logical: print per-column progress messages?  Default \code{TRUE}.
  }
  \item{x, object}{
    An object of class \code{"fitverse_batch"}.
  }
  \item{\dots}{
    Currently unused.
  }
}

\details{
\code{fitverse_batch()} iterates over columns with a \code{tryCatch()}
wrapper so that a fitting failure for one column does not abort the
entire batch.  Failed columns are collected in the \code{failed} element
of the returned object.

Column names containing spaces or special characters are handled correctly
via \code{make.names()} internally.
}

\value{
An S3 object of class \code{"fitverse_batch"} with elements:

\describe{
  \item{\code{fits}}{
    Named list of \code{"fitverse"} objects, one per successfully fitted
    column.
  }
  \item{\code{summary}}{
    Data frame with one row per column, columns:
    \code{variable}, \code{n}, \code{best_dist}, \code{method},
    \code{AIC}, \code{BIC}, \code{loglik}, \code{KS_p}, \code{AD_p}.
  }
  \item{\code{failed}}{
    Character vector of column names for which fitting failed, or
    \code{character(0)} if all succeeded.
  }
  \item{\code{criterion}}{Character: the criterion used.}
  \item{\code{call}}{The matched call.}
}
}

\seealso{
\code{\link{fitverse}} for single-column fitting. \cr
\code{\link{generate_report}} for producing a report from batch results.
}

\examples{
\donttest{
# Fit all numeric columns of the airquality dataset
data(airquality)
batch <- fitverse_batch(airquality,
                        method  = "MLE",
                        verbose = FALSE)
print(batch)
summary(batch)

# Access the fit for the Ozone column
batch$fits[["Ozone"]]
}
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{distribution}
\keyword{models}

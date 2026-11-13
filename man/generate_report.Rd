\name{generate_report}
\alias{generate_report}
\title{Generate an HTML or PDF Report for a Fitted Distribution}

\description{
Produces a standalone, self-contained HTML (or PDF) report that
documents the fitted distribution, estimated parameters, goodness-of-fit
tests, descriptive statistics, diagnostic plots, and (optionally) bootstrap
confidence intervals.

If \pkg{rmarkdown} is installed, the report is rendered from an R Markdown
template.  Otherwise, a pure HTML fallback is used with the diagnostic
plot embedded as a base64-encoded PNG image.
}

\usage{
generate_report(fit,
                output_file = NULL,
                title       = "FitVerse: Distribution Fitting Report",
                author      = "",
                boot        = NULL,
                top_n       = 5L,
                open        = TRUE)
}

\arguments{
  \item{fit}{
    An object of class \code{"fitverse"} as returned by
    \code{\link{fitverse}()}.
  }
  \item{output_file}{
    Character: path to the output file.  If \code{NULL} (default), a
    temporary file is used:
    \code{fitverse_report_<dist>_<timestamp>.html}.
  }
  \item{title}{
    Character: report title.  Defaults to
    \code{"FitVerse: Distribution Fitting Report"}.
  }
  \item{author}{
    Character: author name to include in the report header.
    Default \code{""} (no author line).
  }
  \item{boot}{
    An object of class \code{"fitverse_bootstrap"} (from
    \code{\link{bootstrap_ci}()}), or \code{NULL}.  If supplied, a
    confidence-interval table is included in the report.
  }
  \item{top_n}{
    Integer: number of distributions to show in the ranking table.
    Default \code{3}.
  }
  \item{open}{
    Logical: open the report in the system's default browser after
    writing?  Default \code{TRUE}.  Set to \code{FALSE} in automated
    pipelines or when running inside \pkg{shiny}.
  }
}

\details{
\subsection{R Markdown path (recommended)}{
When \pkg{rmarkdown} is available, \code{generate_report()} writes a
temporary \code{.Rmd} file and calls \code{rmarkdown::render()} to produce
a self-contained HTML document using the \code{html_document} format with
\code{self_contained = TRUE}.  The fitted object is passed via a temporary
\code{.rds} file to avoid workspace pollution.
}

\subsection{HTML fallback path}{
When \pkg{rmarkdown} is unavailable, a self-contained HTML file is built
in pure R:
\itemize{
  \item The diagnostic plot is saved to a temporary PNG and encoded as
    a base64 data URI using \pkg{base64enc} (if available) or a manual
    hex-to-base64 conversion.
  \item Tables are rendered as HTML \code{<table>} elements.
  \item The output is a single-file HTML document with embedded CSS.
}
}
}

\value{
Invisibly returns the normalised path to the output file (character).
}

\seealso{
\code{\link{bootstrap_ci}} for confidence intervals to include in the
report. \cr
\code{\link{save_plot}} to save the diagnostic plot separately.
}

\examples{
\donttest{
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)
fit <- fitverse(x, xlab = "Waiting time (s)", plot = FALSE, verbose = FALSE)

# Write report to a temporary directory so no files are left in the working dir
path <- generate_report(fit,
                        title       = "Gamma Fit Example",
                        author      = "Karuna Reddy",
                        output_file = file.path(tempdir(), "fitverse_report.html"),
                        open        = FALSE)
message("Report saved to: ", path)
}
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{IO}
\keyword{distribution}

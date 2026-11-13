\name{FitVerseApp}
\alias{FitVerseApp}
\title{Launch the FitVerse Interactive Shiny Application}

\description{
Opens the FitVerse web interface in the default browser.  The app provides
a point-and-click interface for uploading data, fitting distributions,
inspecting diagnostic plots, and downloading results --- without writing
any R code.
}

\usage{
FitVerseApp(\dots)
}

\arguments{
  \item{\dots}{Additional arguments passed to \code{\link[shiny]{runApp}},
    for example \code{port} or \code{launch.browser}.}
}

\value{
Called for its side effect (launches a Shiny app); returns \code{NULL}
invisibly.
}

\details{
Requires the \pkg{shiny} package (listed in \code{Suggests}).
An informative error is raised if it is not installed.

The app files are stored in \code{inst/app/} and located at runtime via
\code{system.file("app", package = "FitVerse")}.
}

\seealso{
\code{\link{fitverse}} for the programmatic fitting interface.
}

\examples{
if (interactive()) {
  FitVerseApp()
}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{utilities}

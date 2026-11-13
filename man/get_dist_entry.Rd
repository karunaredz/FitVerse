\name{get_dist_entry}
\alias{get_dist_entry}

\title{Retrieve a Distribution Registry Entry}

\description{
Look up a single distribution entry from the \pkg{FitVerse} distribution
registry by its key.  This function is primarily intended for use by the
\code{FitVerseApp} Shiny application, which needs to access display
names, parameter labels, and PDF equation strings at runtime.
}

\usage{
get_dist_entry(dist_key)
}

\arguments{
  \item{dist_key}{
    Character: the distribution key (e.g., \code{"normal"},
    \code{"weibull"}, \code{"gev"}).  See \code{\link{fitverse_distributions}}
    for the full list of valid keys.
  }
}

\value{
A named list containing the registry fields for the requested
distribution (display name, parameter names, support type, method
availability flags, etc.).  Raises an error if \code{dist_key} is not
found in the registry.
}

\seealso{
\code{\link{fitverse_distributions}} for a tabular view of all registered
distributions and their supported methods.
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{internal}

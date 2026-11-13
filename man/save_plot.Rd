\name{save_plot}
\alias{save_plot}
\title{Save a FitVerse Diagnostic Plot to File}

\description{
Renders the two-panel diagnostic plot for a \code{fitverse} object and
saves it as a high-resolution PNG (or PDF) file, without displaying the
figure on the current graphics device.  Suitable for embedding in reports
or automated pipelines.
}

\usage{
save_plot(fit,
          file        = file.path(tempdir(), "fitverse_plot.png"),
          which       = "both",
          top_n       = 3L,
          xlab        = NULL,
          width       = 14,
          height      = 5.5,
          dpi         = 600,
          \dots)
}

\arguments{
  \item{fit}{
    An object of class \code{"fitverse"}.
  }
  \item{file}{
    Character: output file path.  The file extension determines the format:
    \code{.png} (default), \code{.pdf}, or \code{.svg}.
    Default \code{file.path(tempdir(), "fitverse_plot.png")}.
  }
  \item{which}{
    Which panels to save: \code{"both"} (default), \code{"density"}, or
    \code{"qq"}.  See \code{\link{plot.fitverse}} for details.
  }
  \item{top_n}{
    Number of top-ranked distributions to show on the density panel.
    Default \code{3}.
  }
  \item{xlab}{
    Character: x-axis label.  Defaults to \code{fit$xlab} if present,
    otherwise \code{"x"}.
  }
  \item{width}{
    Numeric: plot width in inches.  Default \code{14}.
  }
  \item{height}{
    Numeric: plot height in inches.  Default \code{5.5}.
  }
  \item{dpi}{
    Integer: resolution in dots per inch.  Default \code{600}.
    Ignored for PDF output.
  }
  \item{\dots}{
    Additional arguments passed to \code{\link{plot.fitverse}()}.
  }
}

\details{
Uses \code{grDevices::png()} (or \code{grDevices::pdf()} / \code{grDevices::svg()}
depending on file extension) and \code{gridExtra::grid.arrange()} to render
both panels in a single device call, ensuring consistent alignment.

The function opens the graphics device, renders the figure, and closes the
device via \code{on.exit()}, so the current interactive device is unaffected.
}

\value{
Invisibly returns the normalised path to the saved file (character).
}

\seealso{
\code{\link{plot.fitverse}} for interactive display.
}

\examples{
\donttest{
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)
fit <- fitverse(x, plot = FALSE, verbose = FALSE)

save_plot(fit, file = tempfile(fileext = ".png"))
}
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{hplot}

\name{plot.fitverse}
\alias{plot.fitverse}
\title{Diagnostic Plot for a \code{fitverse} Object}

\description{
Produces a two-panel diagnostic figure for a fitted \code{fitverse} object:
\enumerate{
  \item A histogram of the data overlaid with the fitted density curves for
    the top-ranked distributions.  The PDF formula is annotated directly on
    the figure.
  \item A Q-Q plot of the empirical quantiles against theoretical quantiles
    from the best-fit distribution.
}
Either or both panels can be requested.  The plot can optionally be returned
as an interactive \pkg{plotly} HTML widget.
}

\usage{
\method{plot}{fitverse}(x,
     which       = "both",
     top_n       = 3L,
     xlab        = NULL,
     interactive = FALSE,
     draw        = TRUE,
     \dots)
}

\arguments{
  \item{x}{
    An object of class \code{"fitverse"} as returned by
    \code{\link{fitverse}()}.
  }
  \item{which}{
    Character scalar controlling which panels to draw.  One of:
    \describe{
      \item{\code{"both"}}{(default) Both density and Q-Q panels side by side.}
      \item{\code{"density"}}{Density histogram with overlaid PDF curves only.}
      \item{\code{"qq"}}{Q-Q plot only.}
    }
  }
  \item{top_n}{
    Integer: number of top-ranked models to overlay on the density panel.
    The best-fit model is drawn as a solid line; others as dashed lines
    with distinct colours.  Default \code{3}.
  }
  \item{xlab}{
    Character: x-axis label.  If \code{NULL} (default), falls back to the
    \code{xlab} stored in \code{x} (set when \code{fitverse()} was called),
    then to \code{"x"} if that is also absent.
  }
  \item{interactive}{
    Logical: if \code{TRUE} and \pkg{plotly} is installed, converts the
    ggplot2 figure to a \pkg{plotly} interactive widget.
    Default \code{FALSE}.
  }
  \item{draw}{
    Logical: if \code{TRUE} (default), print the plot to the current
    graphics device.  Set to \code{FALSE} to return the plot object
    silently without displaying it.
  }
  \item{\dots}{
    Currently unused; reserved for future extensions.
  }
}

\details{
The density panel uses \code{ggplot2::geom_histogram(aes(y = after_stat(density)))}
with Freedman-Diaconis binning, overlaid with \code{geom_line()} curves
evaluated on a fine grid of 512 points spanning the observed data range.

The Q-Q panel compares theoretical quantiles from the best-fit distribution
(evaluated via \code{\link{qfitverse}()}) against the empirical quantiles of
the data, with a 45-degree reference line.

When \code{which = "both"}, the two ggplot2 objects are arranged side-by-side
using \code{gridExtra::grid.arrange()}.  When \code{interactive = TRUE} and
\code{which != "both"}, a single panel is converted with
\code{plotly::ggplotly()}.
}

\value{
Invisibly returns the plot object(s):
\itemize{
  \item A single ggplot2 object when \code{which} is \code{"density"} or \code{"qq"}.
  \item A \code{grob} (arranged grid object) when \code{which = "both"}.
  \item A plotly widget when \code{interactive = TRUE}.
}
}

\seealso{
\code{\link{save_plot}} to write the figure to a PNG or PDF file. \cr
\code{\link{fitverse}} which calls this function automatically when \code{plot = TRUE}.
}

\examples{
set.seed(42)
x <- rgamma(300, shape = 2, rate = 0.5)
fit <- fitverse(x, plot = FALSE, verbose = FALSE)

# Both panels (default)
plot(fit)

# Density panel only with custom x-axis label
plot(fit, which = "density", xlab = "Value", top_n = 5)

# Q-Q plot only
plot(fit, which = "qq")
}


\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{hplot}
\keyword{distribution}

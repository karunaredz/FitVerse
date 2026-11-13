\name{FitVerse_datasets}
\alias{FitVerse_datasets}
\alias{annual_max_flows}
\alias{wind_speeds}
\alias{failure_times}
\alias{insurance_claims}
\alias{annual_max_flows.csv}
\alias{wind_speeds.csv}
\alias{failure_times.csv}
\alias{insurance_claims.csv}
\alias{concrete.csv}
\alias{rainfall_grouped.csv}
\alias{failure_times_grouped.csv}
\alias{insurance_losses_grouped.csv}

\title{Sample Datasets Bundled with FitVerse}

\description{
FitVerse includes several comma-separated value (CSV) files in
\code{inst/extdata/} that can be used to explore the package's fitting
functions.  All files are accessed with \code{system.file()}.

\subsection{Individual-observation datasets (for \code{\link{fitverse}()})}{
\describe{
  \item{\code{annual_max_flows.csv}}{
    Annual maximum daily peak flows (m\eqn{^3}/s) for the Rewa River at
    Navolau, Fiji, covering a 75-year record (1950--2024).  The data are
    right-skewed; Gumbel, GEV, Log-Pearson III, and Log-Normal are
    natural candidates.
  }
  \item{\code{wind_speeds.csv}}{
    Daily maximum wind gust speeds (m/s) recorded at Nadi Airport, Fiji,
    over 200 days (2020--2024).  Weibull and Rayleigh distributions are
    commonly used for wind data.
  }
  \item{\code{failure_times.csv}}{
    Failure times (hours) from a life test of 150 aluminium electrolytic
    capacitors under 85 \eqn{{}^\circ}C stress.  Weibull, Gamma,
    Log-Normal, and Birnbaum-Saunders are typical reliability choices.
  }
  \item{\code{insurance_claims.csv}}{
    Residential property insurance claim amounts (in thousands of USD)
    from 180 claims settled by Pacific Insurance Ltd between 2018 and
    2024.  Log-Normal, Gamma, and Pareto distributions are common in
    loss modelling.
  }
  \item{\code{concrete.csv}}{
    Compressive strength (MPa) and mix-design variables for 1030 concrete
    specimens (UCI Machine Learning Repository).  The \code{Strength}
    column is suitable for fitting; Normal, Gamma, and Weibull are
    plausible choices.
  }
}
}

\subsection{Grouped / interval-censored datasets (for \code{\link{fitverse_grouped}()})}{
\describe{
  \item{\code{rainfall_grouped.csv}}{
    Annual maximum daily rainfall (mm) at Suva, Fiji, summarised into a
    frequency table with columns \code{midpoint} and \code{count}
    (80 years of record).
  }
  \item{\code{failure_times_grouped.csv}}{
    Failure times for 120 electronic components given as an interval
    frequency table with columns \code{lower}, \code{upper}, and
    \code{count}.
  }
  \item{\code{insurance_losses_grouped.csv}}{
    Property insurance claim amounts for 250 claims given as an interval
    frequency table with columns \code{lower}, \code{upper}, and
    \code{count}.
  }
}
}
}

\format{
Each file is a plain-text CSV with a column-name header row.  Lines
beginning with \code{#} are comments describing the dataset.  All
numeric values use the standard decimal point.
}

\seealso{
\code{\link{fitverse}} for fitting to individual observations. \cr
\code{\link{fitverse_grouped}} for fitting to grouped / interval data.
}

\examples{
## Annual maximum flows
path <- system.file("extdata", "annual_max_flows.csv", package = "FitVerse")
x <- read.csv(path, comment.char = "#")$peak_flow_m3s
summary(x)

\donttest{
fit <- fitverse(x, xlab = "Peak flow (m3/s)", plot = FALSE)
print(fit)
}

## Grouped rainfall data
gpath <- system.file("extdata", "rainfall_grouped.csv", package = "FitVerse")
dat   <- read_grouped_csv(gpath)
\donttest{
gfit  <- fitverse_grouped(midpoints = dat$midpoints, counts = dat$counts,
                          xlab = "Rainfall (mm)")
print(gfit)
}
}

\author{
Karuna G. Reddy \email{karuna.reddy@auckland.ac.nz} \cr
M. G. M. Khan \email{mgm.khan@usp.ac.fj}
}

\keyword{datasets}

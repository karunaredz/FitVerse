#' FitVerse: Parametric Distribution Fitting and Analysis
#'
#' Provides a unified interface for fitting parametric probability
#' distributions to continuous univariate data.  The package supports
#' 52 distribution families and three estimation methods (MLE, MOM,
#' L-moments), with automatic AIC/BIC ranking, goodness-of-fit testing,
#' bootstrap confidence intervals, return-level estimation, and a
#' companion Shiny application (\code{FitVerseApp()}).
#'
#' @importFrom stats sd var median IQR quantile density ks.test approx AIC BIC logLik coef dnorm pnorm qnorm rnorm dgamma pgamma qgamma rgamma dexp pexp qexp rexp dweibull pweibull qweibull rweibull dbeta pbeta qbeta rbeta dlogis plogis qlogis rlogis dcauchy pcauchy qcauchy rcauchy dunif punif qunif runif dlnorm plnorm qlnorm rlnorm optim nlminb optimise ppoints ecdf
#' @importFrom fitdistrplus fitdist gofstat
#' @importFrom actuar dllogis pllogis dpareto ppareto dburr pburr dinvgamma pinvgamma dinvweibull pinvweibull dparalogis pparalogis
#' @importFrom mc2d dtriang ptriang dpert ppert
#' @importFrom lmomco lmoms parnor pargam parexp parln3 parwei pargev parglo pargum pargpa parpe3
#' @importFrom ggplot2 ggplot aes geom_histogram stat_function geom_density annotate labs theme_classic theme element_text element_blank scale_fill_manual scale_colour_manual after_stat
#' @importFrom evd dgumbel pgumbel qgumbel rgumbel dgev pgev qgev rgev
#' @importFrom sn dsn psn qsn rsn
#' @importFrom goftest ad.test cvm.test
#' @importFrom grDevices rgb colorRampPalette png dev.off
#' @importFrom graphics hist lines legend box axis mtext
#' @importFrom utils browseURL head read.csv write.csv
#' @importFrom tools file_ext
#' @importFrom gridExtra arrangeGrob grid.arrange
"_PACKAGE"

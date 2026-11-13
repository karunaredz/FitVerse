# ============================================================
# FitVerse Shiny App
# global.R -- loaded once before ui.R and server.R
# ============================================================

# - Load FitVerse ---------------------------------------------------------------
# Deployment (SHINY_PORT set): use the installed FitVerse package.
# Local development:          ALWAYS source from the sibling R/ folder so that
#                             edits to the source files take effect immediately
#                             without having to reinstall the package.

.fv_loaded    <- FALSE
.is_deployed  <- nzchar(Sys.getenv("SHINY_PORT"))

if (requireNamespace("FitVerse", quietly = TRUE)) {
  # ---- Deployed: use installed package ----------------------------------------
  library(FitVerse)
  # actuar and mc2d are in FitVerse's Imports (not Depends), so they are NOT
  # attached to the search path automatically.  fitdistrplus looks up d/p
  # functions by name via match.fun(), which requires them on the path.
  suppressPackageStartupMessages({
    library(actuar)
    library(mc2d)
    library(evd)
    library(sn)
    library(lmomco)
  })
  # Expose the internal registry to the app global env (needed by Simulate tab).
  if (!exists("FITVERSE_DIST_REGISTRY"))
    FITVERSE_DIST_REGISTRY <- FitVerse:::FITVERSE_DIST_REGISTRY
  .fv_loaded <- TRUE
} else {
  # ---- Local development: source from R/ files (always up-to-date) ------------
  app_dir <- dirname(normalizePath("app.R", mustWork = FALSE))

  # On shinyapps.io the R source is bundled inside pkg/R/
  # Locally it falls back to the sibling FitVerse 1.0-1/R/ folder
  pkg_r <- if (.is_deployed) {
    normalizePath(file.path(app_dir, "pkg", "R"), mustWork = FALSE)
  } else {
    normalizePath(file.path(app_dir, "..", "FitVerse 1.0-1", "R"), mustWork = FALSE)
  }

  if (dir.exists(pkg_r)) {
    suppressPackageStartupMessages({
      library(actuar)
      library(mc2d)
      library(evd)
      library(sn)
      library(fitdistrplus)
      library(lmomco)
      library(ggplot2)
      library(goftest)
    })
    src_order <- c(
      "utils.R", "distributions_registry.R",
      "fit_mle.R", "fit_mom.R", "fit_lmom.R",
      "gof_tests.R", "model_selection.R",
      "data_summary.R", "s3_methods.R",
      "probability_tools.R", "bootstrap_ci.R",
      "fitverse_batch.R", "fit_grouped.R", "report.R",
      "plot_fitverse.R", "lmrd.R", "extreme_value.R", "censored.R", "fitverse.R"
    )
    for (f in src_order) {
      fp <- file.path(pkg_r, f)
      if (file.exists(fp)) source(fp, local = FALSE)
    }
    .fv_loaded <- TRUE
  }
}

if (!.fv_loaded) {
  stop(paste(
    "FitVerse package not found.",
    "Install it with: devtools::install('path/to/FitVerse')",
    "or place the FitVerseApp folder next to the FitVerse package folder."
  ))
}

# - Shiny dependencies ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
})


# - Distribution registry for UI -
# Organised by family for the "Manual" distribution picker.

FV_DIST_CHOICES <- list(
  "Symmetric / Unbounded" = c(
    "Normal"                    = "normal",
    "Logistic"                  = "logistic",
    "Cauchy"                    = "cauchy",
    "Skew-Normal"               = "skewnormal",
    "Laplace"                   = "laplace",
    "Student-t (loc-scale)"     = "student_t",
    "Generalised Normal"        = "gnorm",
    "Asymmetric Laplace"        = "alaplace",
    "Johnson SU"                = "johnsonsu",
    "Normal Inverse Gaussian"   = "nig",
    "Generalised Hyperbolic"    = "ghyp",
    "Truncated Normal"          = "truncnorm"
  ),
  "Right-skewed (positive support)" = c(
    "Gamma"                     = "gamma",
    "Log-Normal"                = "lognormal",
    "Exponential"               = "exponential",
    "Weibull (2P)"              = "weibull",
    "Weibull (3P)"              = "weibull3p",
    "Log-Logistic"              = "loglogistic",
    "Pareto"                    = "pareto",
    "Inverse Gamma"             = "invgamma",
    "Inverse Weibull"           = "invweibull",
    "Paralogistic"              = "paralogistic",
    "Burr XII"                  = "burr",
    "Inverse Gaussian"          = "invgaussian",
    "Lomax (Pareto Type II)"    = "lomax",
    "Generalised Gamma"         = "gengamma",
    "Gompertz"                  = "gompertz",
    "Birnbaum-Saunders"         = "birnbaum_saunders",
    "Rayleigh"                  = "rayleigh",
    "Half-Normal"               = "halfnormal",
    "Exponentiated Weibull"     = "expweibull",
    "Gamma (3-Parameter)"       = "gamma3",
    "Log-Normal (3-Parameter)"  = "lnorm3",
    "Lindley"                   = "lindley",
    "Exponentiated Exponential" = "expexp",
    "Nakagami"                  = "nakagami",
    "Dagum"                     = "dagum"
  ),
  "Heavy-tailed" = c(
    "GB2 (Gen. Beta 2nd Kind)"  = "gb2"
  ),
  "Extreme value / Hydrological" = c(
    "Generalised Extreme Value" = "gev",
    "Gumbel"                    = "gumbel",
    "Generalised Pareto (GPD)"  = "gpd",
    "Pearson Type III"          = "pearson3",
    "Log-Pearson Type III"      = "lpearson3",
    "Generalised Logistic (GLO)"= "glo",
    "Kappa-4"                   = "kappa4",
    "Wakeby"                    = "wakeby"
  ),
  "Bounded / Finite support" = c(
    "Beta"                      = "beta",
    "Uniform"                   = "uniform",
    "Triangular"                = "triangular",
    "Right Triangular"          = "rtriangle",
    "PERT"                      = "pert",
    "Kumaraswamy"               = "kumaraswamy"
  )
)

FV_ALL_DIST_KEYS <- unname(unlist(FV_DIST_CHOICES))

FV_METHOD_CHOICES <- c(
  "Maximum Likelihood (MLE)"  = "MLE",
  "Method of Moments (MOM)"   = "MOM",
  "L-Moments (LMOM)"          = "LMOM",
  "All three methods"         = "ALL"
)

FV_CRITERION_CHOICES <- c("AIC", "BIC")

# - Parameter symbols (param_name -> Greek/symbol label) -
# Used to annotate the parameter estimates panel.
# Format: c(r_param_name = "display (symbol)")
FV_PARAM_SYMBOLS <- list(
  normal          = c(mean     = "mean (mu)",   sd       = "std dev (sigma)"),
  logistic        = c(location = "loc (mu)",    scale    = "scale (s)"),
  skewnormal      = c(xi       = "xi (xi)",     omega    = "omega (omega)",   alpha = "shape (alpha)"),
  cauchy          = c(location = "loc (mu)",    scale    = "scale (gamma)"),
  laplace         = c(mu       = "mean (mu)",   b        = "scale (b)"),
  student_t       = c(nu       = "df (nu)",     mu       = "loc (mu)",     sigma = "scale (sigma)"),
  lognormal       = c(meanlog  = "log-mean (mu)", sdlog  = "log-sd (sigma)"),
  gamma           = c(shape    = "shape (alpha)",  rate     = "rate (beta)"),
  exponential     = c(rate     = "rate (lambda)"),
  weibull         = c(shape    = "shape (k)",       scale    = "scale (lambda)"),
  weibull3p       = c(shape    = "shape (k)",       scale    = "scale (lambda)",   thres = "threshold (gamma)"),
  loglogistic     = c(shape    = "shape (beta)",  scale    = "scale (alpha)"),
  invgamma        = c(shape    = "shape (alpha)",  scale    = "scale (beta)"),
  invweibull      = c(shape    = "shape (beta)",  scale    = "scale (alpha)"),
  paralogistic    = c(shape    = "shape (alpha)",  scale    = "scale (theta)"),
  pearson3        = c(shape    = "shape (alpha)",  scale    = "scale (beta)",   loc   = "loc (mu)"),
  lpearson3       = c(shape    = "shape (alpha)",  scale    = "scale (beta)",   loc   = "loc (mu)"),
  pareto          = c(shape    = "shape (alpha)",  scale    = "scale (theta)"),
  burr            = c(shape1   = "shape1 (c)",      shape2   = "shape2 (k)",       scale = "scale (gamma)"),
  invgaussian     = c(mu       = "mean (mu)",   lambda   = "precision (lambda)"),
  lomax           = c(alpha    = "shape (alpha)",  lambda   = "scale (lambda)"),
  gengamma        = c(p        = "power (p)",       d        = "shape (d)",         b    = "scale (b)"),
  gompertz        = c(alpha    = "scale (alpha)",  beta     = "growth (beta)"),
  birnbaum_saunders = c(alpha  = "shape (alpha)", beta      = "scale (beta)"),
  gumbel          = c(loc      = "loc (mu)",    scale    = "scale (sigma)"),
  gev             = c(loc      = "loc (mu)",    scale    = "scale (sigma)",    shape = "shape (xi)"),
  gpd             = c(loc      = "loc (mu)",    scale    = "scale (sigma)",    shape = "shape (xi)"),
  glo             = c(loc      = "loc (mu)",    scale    = "scale (alpha)",    shape = "shape (kappa)"),
  beta            = c(shape1   = "shape1 (alpha)", shape2   = "shape2 (beta)"),
  uniform         = c(min      = "min (a)",          max     = "max (b)"),
  triangular      = c(min      = "min (a)",          mode    = "mode (c)",          max  = "max (b)"),
  rtriangle       = c(min      = "min (a)",          max     = "max (b)"),
  pert            = c(min      = "min (a)",          mode    = "mode (c)",          max  = "max (b)"),
  kumaraswamy     = c(shape1   = "shape1 (a)",       shape2  = "shape2 (b)"),
  rayleigh        = c(sigma    = "scale (sigma)"),
  halfnormal      = c(sigma    = "scale (sigma)"),
  expweibull      = c(shape1   = "shape1 (alpha)", scale    = "scale (lambda)",   shape2 = "shape2 (beta)"),
  gamma3          = c(shape    = "shape (alpha)",  scale    = "scale (beta)",   thres  = "threshold (gamma)"),
  lnorm3          = c(meanlog  = "log-mean (mu)", sdlog  = "log-sd (sigma)", thres  = "threshold (gamma)"),
  gb2             = c(shape1   = "shape1 (p)",     scale    = "scale (b)",   shape2 = "shape2 (a)",   shape3 = "shape3 (q)"),
  kappa4          = c(xi       = "loc (xi)",        alpha    = "scale (alpha)",   kappa  = "shape1 (kappa)", h = "shape2 (h)"),
  lindley         = c(theta    = "rate (theta)"),
  gnorm           = c(mu       = "loc (mu)",        sigma    = "scale (sigma)",   beta   = "shape (beta)"),
  dagum           = c(shape1   = "shape1 (p)",      scale    = "scale (b)",       shape2 = "shape2 (a)"),
  johnsonsu       = c(gamma    = "loc-shape (gamma)", delta  = "scale-shape (delta)", xi = "loc (xi)", lambda = "scale (lambda)"),
  alaplace        = c(mu       = "loc (mu)",        sigma    = "scale (sigma)",   kappa  = "skew (kappa)"),
  expexp          = c(alpha    = "shape (alpha)",   lambda   = "rate (lambda)"),
  nig             = c(mu       = "loc (mu)",        delta    = "scale (delta)",   alpha  = "shape (alpha)", beta = "skew (beta)"),
  wakeby          = c(xi       = "loc (xi)",        alpha    = "scale1 (alpha)",  beta   = "shape1 (beta)", gamma = "scale2 (gamma)", delta = "shape2 (delta)"),
  ghyp            = c(lambda   = "index (lambda)",  alpha    = "shape (alpha)",   beta   = "skew (beta)",   delta = "scale (delta)",   mu    = "loc (mu)"),
  truncnorm       = c(mean     = "mean (mu)",       sd       = "std dev (sigma)", a      = "lower (a)",     b    = "upper (b)"),
  nakagami        = c(m        = "fading (m)",      Omega    = "spread (Omega)")
)

# - PDF equations in LaTeX (display math) -
# Use \\(...\\) for inline, \\[...\\] for display.  Shiny withMathJax() renders these.
FV_PDF_EQUATIONS <- list(
  normal     = "\\[ f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\exp\\!\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right), \\quad x\\in\\mathbb{R} \\]",
  logistic   = "\\[ f(x) = \\frac{e^{-(x-\\mu)/s}}{s\\left(1+e^{-(x-\\mu)/s}\\right)^{2}}, \\quad x\\in\\mathbb{R} \\]",
  skewnormal = "\\[ f(x) = \\frac{2}{\\omega}\\,\\phi\\!\\left(\\frac{x-\\xi}{\\omega}\\right)\\Phi\\!\\left(\\alpha\\frac{x-\\xi}{\\omega}\\right), \\quad x\\in\\mathbb{R} \\]",
  cauchy     = "\\[ f(x) = \\frac{1}{\\pi\\gamma\\!\\left[1+\\left(\\dfrac{x-\\mu}{\\gamma}\\right)^{\\!2}\\right]}, \\quad x\\in\\mathbb{R} \\]",
  laplace    = "\\[ f(x) = \\frac{1}{2b}\\exp\\!\\left(-\\frac{|x-\\mu|}{b}\\right), \\quad x\\in\\mathbb{R} \\]",
  student_t  = "\\[ f(x) = \\frac{\\Gamma\\!\\left(\\frac{\\nu+1}{2}\\right)}{\\sigma\\sqrt{\\nu\\pi}\\,\\Gamma\\!\\left(\\frac{\\nu}{2}\\right)} \\left(1+\\frac{1}{\\nu}\\!\\left(\\frac{x-\\mu}{\\sigma}\\right)^{\\!2}\\right)^{\\!-\\frac{\\nu+1}{2}} \\]",
  lognormal  = "\\[ f(x) = \\frac{1}{x\\sigma\\sqrt{2\\pi}}\\exp\\!\\left(-\\frac{(\\ln x-\\mu)^2}{2\\sigma^2}\\right), \\quad x>0 \\]",
  gamma      = "\\[ f(x) = \\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)}\\,x^{\\alpha-1}e^{-\\beta x}, \\quad x>0 \\]",
  exponential= "\\[ f(x) = \\lambda\\, e^{-\\lambda x}, \\quad x>0 \\]",
  weibull    = "\\[ f(x) = \\frac{k}{\\lambda}\\!\\left(\\frac{x}{\\lambda}\\right)^{\\!k-1}\\!\\exp\\!\\left(-\\!\\left(\\frac{x}{\\lambda}\\right)^{\\!k}\\right), \\quad x>0 \\]",
  weibull3p  = "\\[ f(x) = \\frac{k}{\\lambda}\\!\\left(\\frac{x-\\gamma}{\\lambda}\\right)^{\\!k-1}\\!\\exp\\!\\left(-\\!\\left(\\frac{x-\\gamma}{\\lambda}\\right)^{\\!k}\\right), \\quad x>\\gamma \\]",
  loglogistic= "\\[ f(x) = \\frac{(\\beta/\\alpha)(x/\\alpha)^{\\beta-1}}{\\left(1+(x/\\alpha)^{\\beta}\\right)^{2}}, \\quad x>0 \\]",
  invgamma   = "\\[ f(x) = \\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)}\\,x^{-\\alpha-1}e^{-\\beta/x}, \\quad x>0 \\]",
  invweibull = "\\[ f(x) = \\frac{\\beta}{\\alpha}\\!\\left(\\frac{\\alpha}{x}\\right)^{\\!\\beta+1}\\!\\exp\\!\\left(-\\!\\left(\\frac{\\alpha}{x}\\right)^{\\!\\beta}\\right), \\quad x>0 \\]",
  paralogistic="\\[ f(x) = \\frac{\\alpha^2(x/\\theta)^{\\alpha}}{\\theta\\left[1+(x/\\theta)^{\\alpha}\\right]^{\\alpha+1}}, \\quad x>0 \\]",
  pearson3   = "\\[ f(x) = \\frac{1}{\\beta^{\\alpha}\\Gamma(\\alpha)}\\,(x-\\mu)^{\\alpha-1}e^{-(x-\\mu)/\\beta}, \\quad x>\\mu \\]",
  lpearson3  = "\\[ f(x) = \\frac{(\\ln x-\\mu)^{\\alpha-1}}{\\beta^{\\alpha}\\Gamma(\\alpha)\\,x}\\,e^{-(\\ln x-\\mu)/\\beta}, \\quad \\ln x>\\mu \\]",
  pareto     = "\\[ f(x) = \\frac{\\alpha\\,\\theta^{\\alpha}}{x^{\\alpha+1}}, \\quad x>\\theta \\]",
  burr       = "\\[ f(x) = \\frac{ck}{\\gamma}\\!\\left(\\frac{x}{\\gamma}\\right)^{\\!c-1}\\!\\left(1+\\left(\\frac{x}{\\gamma}\\right)^{\\!c}\\right)^{\\!-k-1}, \\quad x>0 \\]",
  invgaussian= "\\[ f(x) = \\sqrt{\\frac{\\lambda}{2\\pi x^3}}\\exp\\!\\left(-\\frac{\\lambda(x-\\mu)^2}{2\\mu^2 x}\\right), \\quad x>0 \\]",
  lomax      = "\\[ f(x) = \\frac{\\alpha}{\\lambda}\\!\\left(1+\\frac{x}{\\lambda}\\right)^{\\!-\\alpha-1}, \\quad x>0 \\]",
  gengamma   = "\\[ f(x) = \\frac{p}{b^d\\,\\Gamma(d/p)}\\,x^{d-1}\\exp\\!\\left(-\\!\\left(\\frac{x}{b}\\right)^{\\!p}\\right), \\quad x>0 \\]",
  gompertz   = "\\[ f(x) = \\alpha\\,e^{\\beta x}\\exp\\!\\left(-\\frac{\\alpha}{\\beta}\\left(e^{\\beta x}-1\\right)\\right), \\quad x>0 \\]",
  birnbaum_saunders = "\\[ f(x) = \\frac{1}{2\\alpha\\beta}\\!\\left[\\!\\left(\\frac{\\beta}{x}\\right)^{\\!1/2}+\\left(\\frac{\\beta}{x}\\right)^{\\!3/2}\\right]\\phi\\!\\left(\\frac{1}{\\alpha}\\!\\left[\\!\\left(\\frac{x}{\\beta}\\right)^{\\!1/2}-\\left(\\frac{\\beta}{x}\\right)^{\\!1/2}\\right]\\right) \\]",
  gumbel     = "\\[ f(x) = \\frac{1}{\\sigma}\\exp\\!\\left(-\\frac{x-\\mu}{\\sigma}-e^{-(x-\\mu)/\\sigma}\\right), \\quad x\\in\\mathbb{R} \\]",
  gev        = "\\[ f(x) = \\frac{1}{\\sigma}\\!\\left(1+\\xi\\frac{x-\\mu}{\\sigma}\\right)^{\\!-1/\\xi-1}\\!\\exp\\!\\left(-\\!\\left(1+\\xi\\frac{x-\\mu}{\\sigma}\\right)^{\\!-1/\\xi}\\right) \\]",
  gpd        = "\\[ f(x) = \\frac{1}{\\sigma}\\!\\left(1+\\xi\\frac{x-\\mu}{\\sigma}\\right)^{\\!-1/\\xi-1}, \\quad x>\\mu \\]",
  glo        = "\\[ f(x) = \\frac{\\left(1-\\kappa(x-\\mu)/\\alpha\\right)^{1/\\kappa-1}}{\\alpha\\left(1+\\left(1-\\kappa(x-\\mu)/\\alpha\\right)^{1/\\kappa}\\right)^{2}} \\]",
  beta       = "\\[ f(x) = \\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)}, \\quad 0&lt;x&lt;1 \\]",
  uniform    = "\\[ f(x) = \\frac{1}{b-a}, \\quad a\\leq x\\leq b \\]",
  triangular = "\\[ f(x) = \\begin{cases} \\dfrac{2(x-a)}{(b-a)(c-a)}, & a\\leq x\\leq c \\\\ \\dfrac{2(b-x)}{(b-a)(b-c)}, & c&lt;x\\leq b \\end{cases} \\]",
  rtriangle  = "\\[ f(x) = \\frac{2(b-x)}{(b-a)^2}, \\quad a\\leq x\\leq b \\]",
  pert       = "\\[ f(x) = \\frac{(x-a)^{\\alpha_1-1}(b-x)^{\\alpha_2-1}}{B(\\alpha_1,\\alpha_2)(b-a)^{\\alpha_1+\\alpha_2-1}},\\; \\alpha_i = 1+4\\tfrac{c_i-a}{b-a} \\]",
  kumaraswamy= "\\[ f(x) = a\\,b\\,x^{a-1}(1-x^a)^{b-1}, \\quad 0&lt;x&lt;1 \\]",
  rayleigh   = "\\[ f(x) = \\frac{x}{\\sigma^2}\\exp\\!\\left(-\\frac{x^2}{2\\sigma^2}\\right), \\quad x>0 \\]",
  halfnormal = "\\[ f(x) = \\frac{\\sqrt{2}}{\\sigma\\sqrt{\\pi}}\\exp\\!\\left(-\\frac{x^2}{2\\sigma^2}\\right), \\quad x>0 \\]",
  expweibull = "\\[ f(x) = \\frac{\\alpha\\beta}{\\lambda}\\!\\left(\\frac{x}{\\lambda}\\right)^{\\!\\alpha-1}\\!e^{-(x/\\lambda)^\\alpha}\\!\\left(1-e^{-(x/\\lambda)^\\alpha}\\right)^{\\!\\beta-1}, \\quad x>0 \\]",
  gamma3     = "\\[ f(x) = \\frac{(x-\\gamma)^{\\alpha-1}}{\\beta^{\\alpha}\\Gamma(\\alpha)}\\,e^{-(x-\\gamma)/\\beta}, \\quad x>\\gamma \\]",
  lnorm3     = "\\[ f(x) = \\frac{1}{(x-\\gamma)\\,\\sigma\\sqrt{2\\pi}}\\exp\\!\\left(-\\frac{(\\ln(x-\\gamma)-\\mu)^2}{2\\sigma^2}\\right), \\quad x>\\gamma \\]",
  gb2        = "\\[ f(x) = \\frac{|a|\\,x^{ap-1}}{b^{ap}B(p,q)\\left(1+(x/b)^a\\right)^{p+q}}, \\quad x>0 \\]",
  kappa4     = "\\[ F(x) = \\left(1 - h\\!\\left(1 - \\kappa\\frac{x-\\xi}{\\alpha}\\right)^{1/\\kappa}\\right)^{1/h} \\quad \\text{(4-parameter quantile family)} \\]",
  lindley    = "\\[ f(x) = \\frac{\\theta^2}{1+\\theta}(1+x)\\,e^{-\\theta x}, \\quad x>0 \\]",
  gnorm      = "\\[ f(x) = \\frac{\\beta}{2\\sigma\\Gamma(1/\\beta)}\\exp\\!\\left(-\\left|\\frac{x-\\mu}{\\sigma}\\right|^\\beta\\right), \\quad x\\in\\mathbb{R} \\]",
  dagum      = "\\[ f(x) = \\frac{ap}{x}\\!\\left(\\frac{x}{b}\\right)^{\\!ap}\\!\\left(1+\\left(\\frac{x}{b}\\right)^{\\!a}\\right)^{-(p+1)}, \\quad x>0 \\]",
  johnsonsu  = "\\[ f(x) = \\frac{\\delta}{\\lambda\\sqrt{2\\pi(1+z^2)}}\\exp\\!\\left(-\\tfrac{(\\gamma+\\delta\\sinh^{-1}z)^2}{2}\\right),\\quad z=(x-\\xi)/\\lambda \\]",
  alaplace   = "\\[ f(x) = \\frac{\\kappa}{\\sigma(1+\\kappa^2)}\\times\\begin{cases}e^{-\\kappa(x-\\mu)/\\sigma},& x\\geq\\mu\\\\ e^{(x-\\mu)/(\\kappa\\sigma)},& x<\\mu\\end{cases} \\]",
  expexp     = "\\[ f(x) = \\alpha\\lambda\\,e^{-\\lambda x}\\!\\left(1-e^{-\\lambda x}\\right)^{\\alpha-1}, \\quad x>0 \\]",
  nig        = "\\[ f(x) = \\frac{\\alpha\\delta}{\\pi}\\,K_1\\!\\left(\\alpha\\sqrt{\\delta^2+(x-\\mu)^2}\\right)\\frac{e^{\\delta\\sqrt{\\alpha^2-\\beta^2}+\\beta(x-\\mu)}}{\\sqrt{\\delta^2+(x-\\mu)^2}} \\]",
  wakeby     = "\\[ Q(F) = \\xi + \\frac{\\alpha}{\\beta}\\!\\left(1-(1-F)^{\\beta}\\right) + \\frac{\\gamma}{\\delta}\\!\\left(1-(1-F)^{-\\delta}\\right) \\quad\\text{(quantile function)} \\]",
  ghyp       = "\\[ f(x) \\propto \\left(\\delta^2+(x-\\mu)^2\\right)^{(\\lambda-1/2)/2}K_{\\lambda-1/2}\\!\\left(\\alpha\\sqrt{\\delta^2+(x-\\mu)^2}\\right)e^{\\beta(x-\\mu)} \\]",
  truncnorm  = "\\[ f(x) = \\frac{\\phi((x-\\mu)/\\sigma)/\\sigma}{\\Phi((b-\\mu)/\\sigma)-\\Phi((a-\\mu)/\\sigma)}, \\quad a\\leq x\\leq b \\]",
  nakagami   = "\\[ f(x) = \\frac{2m^m}{\\Gamma(m)\\Omega^m}\\,x^{2m-1}e^{-mx^2/\\Omega}, \\quad x>0 \\]"
)

# - Built-in demo datasets ---------------------------------------------------------------
# Each entry: label (shown in UI), xlab (axis label), desc (blurb), load (fn)
FV_DEMOS <- list(
  precip = list(
    label = "US city annual precipitation",
    xlab  = "Annual precipitation (inches)",
    desc  = paste(
      "Mean annual precipitation (inches) for 70 US cities.",
      "Right-skewed positive data; Gamma, Weibull, and Log-Normal tend to fit well.",
      "Built-in R dataset (Statistical Abstracts of the United States, 1975)."
    ),
    load  = function() as.numeric(datasets::precip)
  ),
  portpirie = list(
    label = "Port Pirie annual max sea levels",
    xlab  = "Annual max sea level (m)",
    desc  = paste(
      "Annual maximum sea levels (m) at Port Pirie, South Australia",
      "(1923-1987, n = 65). Classic extreme-value dataset;",
      "GEV and Gumbel fit well. Source: Smith (1985)."
    ),
    load  = function() {
      d <- evd::portpirie
      if (is.data.frame(d)) as.numeric(d[[1]]) else as.numeric(d)
    }
  ),
  rivers = list(
    label = "North American river lengths",
    xlab  = "River length (miles)",
    desc  = paste(
      "Lengths (miles) of 141 major rivers in North America.",
      "Heavy right tail; Inverse Weibull, Log-Normal, and Gamma all compete well.",
      "Built-in R dataset (McNeil, 1977)."
    ),
    load  = function() as.numeric(datasets::rivers)
  ),
  ozone = list(
    label = "New York daily ozone levels",
    xlab  = "Ozone (ppb)",
    desc  = paste(
      "Daily maximum ozone concentrations (ppb), Roosevelt Island, New York,",
      "May-September 1973 (n = 116, missing values removed).",
      "Right-skewed; Gamma and Log-Normal fit well."
    ),
    load  = function() as.numeric(stats::na.omit(datasets::airquality$Ozone))
  )
)


# - Simulate Data: default parameter values per distribution -
# Used to pre-populate the numeric inputs in the Simulate Data tab.

FV_SIM_DEFAULTS <- list(
  normal            = c(mean = 0,     sd = 1),
  logistic          = c(location = 0, scale = 1),
  skewnormal        = c(xi = 0,       omega = 1,    alpha = 2),
  cauchy            = c(location = 0, scale = 1),
  laplace           = c(mu = 0,       b = 1),
  student_t         = c(nu = 5,       mu = 0,       sigma = 1),
  lognormal         = c(meanlog = 0,  sdlog = 0.5),
  gamma             = c(shape = 2,    rate = 0.5),
  exponential       = c(rate = 1),
  weibull           = c(shape = 2,    scale = 1),
  weibull3p         = c(shape = 2,    scale = 1,    thres = 0),
  loglogistic       = c(shape = 3,    scale = 2),
  invgamma          = c(shape = 3,    scale = 2),
  invweibull        = c(shape = 2,    scale = 1),
  paralogistic      = c(shape = 2,    scale = 1),
  pearson3          = c(shape = 4,    scale = 0.5,  loc = 0),
  lpearson3         = c(shape = 4,    scale = 0.5,  loc = 0),
  invgaussian       = c(mu = 1,       lambda = 1),
  gengamma          = c(p = 1,        d = 2,        b = 1),
  gompertz          = c(alpha = 0.1,  beta = 0.5),
  birnbaum_saunders = c(alpha = 0.5,  beta = 1),
  rayleigh          = c(sigma = 1),
  halfnormal        = c(sigma = 1),
  expweibull        = c(shape1 = 2,   scale = 1,    shape2 = 1.5),
  gamma3            = c(shape = 2,    scale = 1,    thres = 0),
  lnorm3            = c(meanlog = 0,  sdlog = 0.5,  thres = 0),
  pareto            = c(shape = 3,    scale = 1),
  burr              = c(shape1 = 2,   shape2 = 2,   scale = 1),
  lomax             = c(alpha = 3,    lambda = 2),
  gumbel            = c(loc = 0,      scale = 1),
  gev               = c(loc = 0,      scale = 1,    shape = 0.1),
  gpd               = c(loc = 0,      scale = 1,    shape = 0.1),
  glo               = c(loc = 0,      scale = 1,    shape = 0.1),
  beta              = c(shape1 = 2,   shape2 = 5),
  uniform           = c(min = 0,      max = 1),
  triangular        = c(min = 0,      mode = 0.5,   max = 1),
  rtriangle         = c(min = 0,      max = 1),
  pert              = c(min = 0,      mode = 0.5,   max = 1),
  kumaraswamy       = c(shape1 = 2,   shape2 = 5),
  gb2               = c(shape1 = 2,   scale = 1,   shape2 = 2,   shape3 = 2),
  kappa4            = c(xi = 5,       alpha = 1,   kappa = 0.1,  h = 0.1),
  lindley           = c(theta = 1.5),
  gnorm             = c(mu = 0,       sigma = 1,   beta = 1.5),
  dagum             = c(shape1 = 2,   scale = 1,   shape2 = 2),
  johnsonsu         = c(gamma = 0,    delta = 1,   xi = 0,       lambda = 1),
  alaplace          = c(mu = 0,       sigma = 1,   kappa = 0.7),
  expexp            = c(alpha = 3,    lambda = 1),
  nig               = c(mu = 0,       delta = 1,   alpha = 2,    beta = 0),
  wakeby            = c(xi = 0,       alpha = 1,   beta = 0.5,   gamma = 0.1, delta = 0.1),
  ghyp              = c(lambda = -0.5, alpha = 1,  beta = 0,     delta = 1,   mu = 0),
  truncnorm         = c(mean = 5,     sd = 2,      a = 0,        b = 10),
  nakagami          = c(m = 2,        Omega = 4)
)


# - Simulate Data: random number generator for all 52 distributions -
#
# Arguments
#   dist_key  Character. Key in FITVERSE_DIST_REGISTRY.
#   params    Named numeric vector of parameter values (names match registry).
#   n         Integer sample size.
#   seed      Integer random seed, or NULL to skip set.seed().
#
# Returns a numeric vector of length n (or fewer if finite-value filtering needed).

fv_simulate <- function(dist_key, params, n, seed = NULL) {
  if (!is.null(seed) && is.finite(seed)) set.seed(as.integer(seed))
  p <- as.list(params)

  switch(dist_key,

    # - Symmetric / Unbounded -
    normal          = rnorm(n, mean = p$mean, sd = p$sd),

    logistic        = rlogis(n, location = p$location, scale = p$scale),

    skewnormal      = sn::rsn(n, xi = p$xi, omega = p$omega, alpha = p$alpha),

    cauchy          = rcauchy(n, location = p$location, scale = p$scale),

    laplace         = {
      # Double-exponential: difference of two Exponentials
      p$mu + p$b * (rexp(n, 1) - rexp(n, 1))
    },

    student_t       = p$mu + p$sigma * rt(n, df = p$nu),

    # - Right-skewed / Positive -
    lognormal       = rlnorm(n, meanlog = p$meanlog, sdlog = p$sdlog),

    gamma           = rgamma(n, shape = p$shape, rate = p$rate),

    exponential     = rexp(n, rate = p$rate),

    weibull         = rweibull(n, shape = p$shape, scale = p$scale),

    weibull3p       = p$thres + rweibull(n, shape = p$shape, scale = p$scale),

    loglogistic     = actuar::rllogis(n, shape = p$shape, scale = p$scale),

    invgamma        = actuar::rinvgamma(n, shape = p$shape, scale = p$scale),

    invweibull      = actuar::rinvweibull(n, shape = p$shape, scale = p$scale),

    paralogistic    = actuar::rparalogis(n, shape = p$shape, scale = p$scale),

    pearson3        = {
      # Shifted Gamma (Pearson III); scale can be negative for left-skew
      if (abs(p$scale) < 1e-12) return(rep(p$loc, n))
      p$loc + rgamma(n, shape = p$shape, rate = 1 / abs(p$scale)) * sign(p$scale)
    },

    lpearson3       = {
      # Log-Pearson III: Pearson III in log-space, then exponentiate
      if (abs(p$scale) < 1e-12) return(exp(rep(p$loc, n)))
      lx <- p$loc + rgamma(n, shape = p$shape, rate = 1 / abs(p$scale)) * sign(p$scale)
      exp(lx)
    },

    invgaussian     = {
      # Michael-Schucany-Haas (1976) algorithm for Inverse Gaussian / Wald
      y   <- rnorm(n)^2
      x   <- p$mu + (p$mu^2 * y) / (2 * p$lambda) -
             (p$mu / (2 * p$lambda)) * sqrt(4 * p$mu * p$lambda * y + p$mu^2 * y^2)
      z   <- runif(n)
      ifelse(z <= p$mu / (p$mu + x), x, p$mu^2 / x)
    },

    gengamma        = {
      # Generalised Gamma(p, d, b): X = b * G^(1/p), G ~ Gamma(d/p, 1)
      p$b * rgamma(n, shape = p$d / p$p, rate = 1)^(1 / p$p)
    },

    gompertz        = {
      # CDF inversion: Q(u) = log(1 + beta/alpha * rexp) / beta
      log(1 + (p$beta / p$alpha) * rexp(n)) / p$beta
    },

    birnbaum_saunders = {
      # Standard simulation (Rieck, 1999):  X = beta * Y^2,  Y = alpha*Z/2 + sqrt((alpha*Z/2)^2 + 1)
      z <- rnorm(n)
      y <- p$alpha * z / 2 + sqrt((p$alpha * z / 2)^2 + 1)
      p$beta * y^2
    },

    rayleigh        = sqrt(-2 * log(runif(n))) * p$sigma,

    halfnormal      = abs(rnorm(n, 0, p$sigma)),

    expweibull      = {
      # CDF inversion: Q(u) = scale * (-log(1 - u^(1/shape2)))^(1/shape1)
      u <- runif(n)
      p$scale * (-log(1 - u^(1 / p$shape2)))^(1 / p$shape1)
    },

    gamma3          = p$thres + rgamma(n, shape = p$shape, scale = p$scale),

    lnorm3          = p$thres + rlnorm(n, meanlog = p$meanlog, sdlog = p$sdlog),

    # - Heavy-tailed ---------------------------------------------------------------
    pareto          = actuar::rpareto(n, shape = p$shape, scale = p$scale),

    burr            = actuar::rburr(n, shape1 = p$shape1, shape2 = p$shape2,
                                    scale = p$scale),

    lomax           = {
      # CDF inversion: Q(u) = lambda * ((1-u)^(-1/alpha) - 1)
      p$lambda * ((1 - runif(n))^(-1 / p$alpha) - 1)
    },

    # - Extreme value ---------------------------------------------------------------
    gumbel          = evd::rgumbel(n, loc = p$loc, scale = p$scale),

    gev             = evd::rgev(n, loc = p$loc, scale = p$scale, shape = p$shape),

    gpd             = {
      # CDF inversion: Q(u) = loc + scale/xi * ((1-u)^(-xi) - 1), xi != 0
      #                       loc - scale * log(1-u),               xi == 0
      u <- runif(n)
      if (abs(p$shape) < 1e-10) {
        p$loc - p$scale * log(1 - u)
      } else {
        p$loc + p$scale / p$shape * ((1 - u)^(-p$shape) - 1)
      }
    },

    glo             = {
      # Generalised Logistic via lmomco
      para <- lmomco::vec2par(c(p$loc, p$scale, p$shape), type = "glo")
      lmomco::rlmomco(n, para)
    },

    # - Bounded / Finite support -
    beta            = rbeta(n, shape1 = p$shape1, shape2 = p$shape2),

    uniform         = runif(n, min = p$min, max = p$max),

    triangular      = mc2d::rtriang(n, min = p$min, mode = p$mode, max = p$max),

    rtriangle       = {
      # Right-triangular with mode at min; CDF: F(x) = 1 - ((max-x)/(max-min))^2
      # Q(u) = max - (max-min)*sqrt(1-u)
      p$max - (p$max - p$min) * sqrt(1 - runif(n))
    },

    pert            = mc2d::rpert(n, min = p$min, mode = p$mode, max = p$max),

    kumaraswamy     = {
      # CDF inversion: Q(u) = (1 - (1-u)^(1/b))^(1/a)
      u <- runif(n)
      (1 - (1 - u)^(1 / p$shape2))^(1 / p$shape1)
    },

    # - New distributions (40-52) -----------------------------------------------
    gb2             = rgb2(n, shape1 = p$shape1, scale = p$scale,
                           shape2 = p$shape2, shape3 = p$shape3),

    kappa4          = rkap4(n, xi = p$xi, alpha = p$alpha,
                            kappa = p$kappa, h = p$h),

    lindley         = rlindley(n, theta = p$theta),

    gnorm           = rgnorm(n, mu = p$mu, sigma = p$sigma, beta = p$beta),

    dagum           = rdagum(n, shape1 = p$shape1, scale = p$scale,
                             shape2 = p$shape2),

    johnsonsu       = rjohnsonsu(n, gamma = p$gamma, delta = p$delta,
                                 xi = p$xi, lambda = p$lambda),

    alaplace        = ralaplace(n, mu = p$mu, sigma = p$sigma, kappa = p$kappa),

    expexp          = rexpexp(n, alpha = p$alpha, lambda = p$lambda),

    nig             = rnig(n, mu = p$mu, delta = p$delta,
                           alpha = p$alpha, beta = p$beta),

    wakeby          = rwakeby(n, xi = p$xi, alpha = p$alpha,
                              beta = p$beta, gamma = p$gamma, delta = p$delta),

    ghyp            = rghyp(n, lambda = p$lambda, alpha = p$alpha,
                             beta = p$beta, delta = p$delta, mu = p$mu),

    truncnorm       = rtruncnorm(n, mean = p$mean, sd = p$sd,
                                 a = p$a, b = p$b),

    nakagami        = rnakagami(n, m = p$m, Omega = p$Omega),

    stop(paste("Unknown distribution key:", dist_key))
  )
}

# - Force external browser when running locally in RStudio -------------------
# Prevents the app opening in RStudio's small built-in viewer pane.
# The JS resize script in app.R then sizes the window to 90% of the screen.
# No effect when deployed (SHINY_PORT is set on shinyapps.io).
if (!.is_deployed && identical(Sys.getenv("RSTUDIO"), "1")) {
  options(shiny.launch.browser = TRUE)
}

# - Graphics device: prevent macOS Quartz window crash -
# When ggplotly() converts a ggplot that contains text layers or annotations,
# it calls graphics::strwidth() to measure text for layout.  strwidth() MUST
# have an open graphics device -- if none exists, macOS R opens a Quartz window
# as a fallback, which conflicts with Shiny's session management and crashes
# the app on the first "Plots" render (works on second run because the session
# state has changed by then).
#
# Fix: keep a null PDF device open for the lifetime of the Shiny process.
# Shiny's renderPlot opens its own png device (which becomes the *current*
# device), renders to it, then closes it -- the null PDF is unaffected.
# When ggplotly calls strwidth(), it finds the null PDF already open and
# uses it instead of opening Quartz.
if (grDevices::dev.cur() == 1L) {   # 1 = null device (no real device open)
  tryCatch(grDevices::pdf(NULL), error = function(e) invisible(NULL))
}

# - App colour palette (mirrors FitVerse brand) -
FV_NAVY  <- "#1e3a5f"
FV_BLUE  <- "#3d7ebf"
FV_LIGHT <- "#f4f7fb"


# - UI helper functions (must be defined before ui.R / app.R UI block) -

# Thin wrapper: use shinycssloaders if available, otherwise return ui_elem as-is
shinycssloaders_wrapper <- function(ui_elem, message = "Loading...") {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(ui_elem, type = 6,
                                 color = "#1e3a5f",
                                 caption = message)
  } else {
    ui_elem
  }
}

# Render a small labelled stat tile
stat_card_html <- function(label, value) {
  tags$div(
    class = "fv-stat-item",
    tags$div(class = "fv-stat-label", label),
    tags$div(class = "fv-stat-value", value)
  )
}

# Format p-value with significance stars (base R only)
fmt_pval <- function(p) {
  if (is.na(p)) return("--")
  stars <- if      (p < 0.001) "***"
            else if (p < 0.01 ) "**"
            else if (p < 0.05 ) "*"
            else if (p < 0.10 ) "."
            else                ""
  sprintf("%.4f %s", p, stars)
}

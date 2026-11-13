# ============================================================
# FitVerse: Parametric Distribution Fitting and Analysis
# ============================================================
# File       : distributions_registry.R
# Description: Central registry of all 52 probability distributions
#              supported by FitVerse.  Each entry stores:
#                - Display name, R distribution prefix
#                - Parameter names, display labels, and bounds
#                - Starting-value function for optimisation
#                - Support type (determines data pre-checks)
#                - Which packages supply the d/p/q/r functions
#                - Method support flags (MLE / MOM / LMOM)
#                - lmomco type code for L-moment fitting
#                - PDF formula (plain text + plotmath string)
#                - Distribution family tag
#
# Authors:
#   Karuna G. Reddy <karuna.reddy@auckland.ac.nz>
#   University of Auckland, New Zealand
#
#   M. G. M. Khan <mgm.khan@usp.ac.fj>
#   University of the South Pacific, Fiji
#
# Package : FitVerse
# Version : 1.0.1
# Date    : 2026-06-17
# License : GPL (>= 3)
# ============================================================


# -- Registry definition ------------------------------------------------------
#
# Field reference
# ---------------
#  display_name   Human-readable name shown in all outputs
#  r_name         Distribution prefix for d/p/q/r functions (e.g. "norm")
#  params         Parameter names as expected by R's d-function
#  param_labels   Greek-letter (Unicode) labels for plot annotations
#  lower / upper  Named vectors of optimisation bounds (same order as params)
#  start_fun      function(x) -> named list of starting values from data
#  support        "real" | "positive" | "0to1" | "bounded" | "threshold"
#  package        R package that provides the d/p/q/r functions
#  supports_mle   Logical - MLE available?
#  supports_mom   Logical - Method of Moments available?
#  supports_lmom  Logical - L-Moments available via lmomco?
#  lmomco_type    lmomco type string, or NA_character_ if unsupported
#  formula_text   Plain-text PDF formula (Unicode) for console/title display
#  plotmath_str   String parseable as a plotmath expression for plot annotation
#  family         Descriptive family tag
#  description    One-sentence description

FITVERSE_DIST_REGISTRY <- list(

  # ============================================================
  # SYMMETRIC / UNBOUNDED  (entries 1-6)
  # ============================================================

  # -- 1. Normal -------------------------------------------------------------
  normal = list(
    display_name  = "Normal",
    r_name        = "norm",
    params        = c("mean", "sd"),
    param_labels  = c("mu", "sigma"),
    lower         = c(mean = -Inf, sd = 1e-9),
    upper         = c(mean =  Inf, sd =  Inf),
    start_fun     = function(x) list(mean = mean(x), sd = stats::sd(x)),
    support       = "real",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "nor",
    formula_text  = "f(x) = (1/(sigmasqrt(2pi))) exp(-(x-mu)^2/(2sigma^2))",
    plotmath_str  = "f(x)==frac(1,sigma*sqrt(2*pi))*exp(-(x-mu)^2/(2*sigma^2))",
    family        = "Symmetric",
    description   = "The classic bell-shaped distribution defined by mean mu and standard deviation sigma."
  ),

  # -- 2. Logistic -----------------------------------------------------------
  logistic = list(
    display_name  = "Logistic",
    r_name        = "logis",
    params        = c("location", "scale"),
    param_labels  = c("mu", "s"),
    lower         = c(location = -Inf, scale = 1e-9),
    upper         = c(location =  Inf, scale =  Inf),
    start_fun     = function(x) list(location = mean(x),
                                     scale    = stats::sd(x) * sqrt(3) / pi),
    support       = "real",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "glo",   # generalised logistic; shape kappa fixed to 0
    formula_text  = "f(x) = exp(-(x-mu)/s) / [s(1+exp(-(x-mu)/s))^2]",
    plotmath_str  = "f(x)==frac(exp(-(x-mu)/s),s*(1+exp(-(x-mu)/s))^2)",
    family        = "Symmetric",
    description   = "Symmetric distribution similar to the normal but with slightly heavier tails."
  ),

  # -- 3. Skew-Normal --------------------------------------------------------
  skewnormal = list(
    display_name  = "Skew-Normal",
    r_name        = "sn",
    params        = c("xi", "omega", "alpha"),
    param_labels  = c("xi", "omega", "alpha"),
    lower         = c(xi = -Inf, omega = 1e-9, alpha = -Inf),
    upper         = c(xi =  Inf, omega =  Inf, alpha =  Inf),
    start_fun     = function(x) list(xi = mean(x), omega = stats::sd(x), alpha = 0),
    support       = "real",
    package       = "sn",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = (2/omega)phi((x-xi)/omega)Phi(alpha(x-xi)/omega)",
    plotmath_str  = "f(x)==frac(2,omega)*phi((x-xi)/omega)*Phi(alpha*(x-xi)/omega)",
    family        = "Asymmetric",
    description   = "Generalises the normal distribution with an additional skewness parameter alpha."
  ),

  # -- 4. Cauchy -------------------------------------------------------------
  cauchy = list(
    display_name  = "Cauchy",
    r_name        = "cauchy",
    params        = c("location", "scale"),
    param_labels  = c("x0", "gamma"),
    lower         = c(location = -Inf, scale = 1e-9),
    upper         = c(location =  Inf, scale =  Inf),
    start_fun     = function(x) list(location = stats::median(x),
                                     scale    = stats::IQR(x) / 2),
    support       = "real",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = FALSE,   # mean and variance undefined
    supports_lmom = FALSE,   # L-moments poorly defined
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = 1 / (pigamma[1+((x-x0)/gamma)^2])",
    plotmath_str  = "f(x)==frac(1,pi*gamma*(1+((x-x[0])/gamma)^2))",
    family        = "Heavy-tailed",
    description   = "Pathological heavy-tailed distribution; mean and variance do not exist."
  ),

  # -- 5. Laplace ------------------------------------------------------------
  laplace = list(
    display_name  = "Laplace",
    r_name        = "laplace",
    params        = c("mu", "b"),
    param_labels  = c("mu", "b"),
    lower         = c(mu = -Inf, b = 1e-9),
    upper         = c(mu =  Inf, b =  Inf),
    start_fun     = function(x) {
      mu0 <- stats::median(x)
      list(mu = mu0, b = mean(abs(x - mu0)))
    },
    support       = "real",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = exp(-|x-mu|/b) / (2b)",
    plotmath_str  = "f(x)==frac(1,2*b)*exp(-abs(x-mu)/b)",
    family        = "Symmetric",
    description   = "Double exponential distribution; heavier tails than normal; robust to outliers."
  ),

  # -- 6. Student-t (Location-Scale) -----------------------------------------
  student_t = list(
    display_name  = "Student-t (Location-Scale)",
    r_name        = "student_t",
    params        = c("nu", "mu", "sigma"),
    param_labels  = c("nu", "mu", "sigma"),
    lower         = c(nu = 1e-9, mu = -Inf, sigma = 1e-9),
    upper         = c(nu =  Inf, mu =  Inf, sigma =  Inf),
    start_fun     = function(x) {
      nu0 <- 10   # start near normal; nu>2 required for finite variance
      list(nu = nu0, mu = mean(x), sigma = stats::sd(x) * sqrt((nu0 - 2) / nu0))
    },
    support       = "real",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = t_nu((x-mu)/sigma) / sigma  (location-scale Student-t)",
    plotmath_str  = "f(x)==frac(1,sigma)*t[nu]((x-mu)/sigma)",
    family        = "Symmetric",
    description   = "Location-scale Student-t; heavier tails than normal; nu->Inf recovers the normal."
  ),

  # ============================================================
  # RIGHT-SKEWED / POSITIVE SUPPORT  (entries 7-26)
  # ============================================================

  # -- 7. Log-Normal ---------------------------------------------------------
  lognormal = list(
    display_name  = "Log-Normal",
    r_name        = "lnorm",
    params        = c("meanlog", "sdlog"),
    param_labels  = c("mu", "sigma"),
    lower         = c(meanlog = -Inf, sdlog = 1e-9),
    upper         = c(meanlog =  Inf, sdlog =  Inf),
    start_fun     = function(x) list(meanlog = mean(log(x)), sdlog = stats::sd(log(x))),
    support       = "positive",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "ln3",   # 3-par log-normal; location fixed to 0 for 2-par
    formula_text  = "f(x) = (1/(xsigmasqrt(2pi))) exp(-(ln x-mu)^2/(2sigma^2)),  x>0",
    plotmath_str  = "f(x)==frac(1,x*sigma*sqrt(2*pi))*exp(-(log(x)-mu)^2/(2*sigma^2))",
    family        = "Right-skewed",
    description   = "Right-skewed distribution whose logarithm is normally distributed; x>0."
  ),

  # -- 8. Gamma --------------------------------------------------------------
  gamma = list(
    display_name  = "Gamma",
    r_name        = "gamma",
    params        = c("shape", "rate"),
    param_labels  = c("alpha", "beta"),
    lower         = c(shape = 1e-9, rate = 1e-9),
    upper         = c(shape =  Inf, rate =  Inf),
    start_fun     = function(x) {
      m <- mean(x); v <- stats::var(x)
      list(shape = m^2 / v, rate = m / v)
    },
    support       = "positive",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "gam",
    formula_text  = "f(x) = beta^alpha x^(alpha-1) exp(-betax) / Gamma(alpha),  x>0",
    plotmath_str  = "f(x)==frac(beta^alpha,Gamma(alpha))*x^{alpha-1}*exp(-beta*x)",
    family        = "Right-skewed",
    description   = "Flexible right-skewed distribution for positive data; generalises the exponential."
  ),

  # -- 9. Exponential --------------------------------------------------------
  exponential = list(
    display_name  = "Exponential",
    r_name        = "exp",
    params        = c("rate"),
    param_labels  = c("lambda"),
    lower         = c(rate = 1e-9),
    upper         = c(rate = Inf),
    start_fun     = function(x) list(rate = 1 / mean(x)),
    support       = "positive",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "exp",
    formula_text  = "f(x) = lambda exp(-lambdax),  x>=0",
    plotmath_str  = "f(x)==lambda*exp(-lambda*x)",
    family        = "Right-skewed",
    description   = "Memoryless distribution for inter-arrival times and failure rates."
  ),

  # -- 10. Weibull (2-Parameter) ---------------------------------------------
  weibull = list(
    display_name  = "Weibull (2-Parameter)",
    r_name        = "weibull",
    params        = c("shape", "scale"),
    param_labels  = c("k", "lambda"),
    lower         = c(shape = 1e-9, scale = 1e-9),
    upper         = c(shape =  Inf, scale =  Inf),
    start_fun     = function(x) {
      cv_val <- stats::sd(x) / mean(x)
      shape0 <- max(0.5, cv_val^(-1.086))
      list(shape = shape0, scale = mean(x) / gamma(1 + 1 / shape0))
    },
    support       = "positive",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "wei",
    formula_text  = "f(x) = (k/lambda)(x/lambda)^(k-1) exp(-(x/lambda)^k),  x>=0",
    plotmath_str  = "f(x)==frac(k,lambda)*(frac(x,lambda))^{k-1}*exp(-(x/lambda)^k)",
    family        = "Right-skewed",
    description   = "Workhorse of reliability analysis; k<1 decreasing, k=1 exponential, k>1 unimodal."
  ),

  # -- 11. Weibull (3-Parameter) ---------------------------------------------
  weibull3p = list(
    display_name  = "Weibull (3-Parameter)",
    r_name        = "weibull3p",
    params        = c("shape", "scale", "thres"),
    param_labels  = c("k", "lambda", "mu"),
    lower         = c(shape = 1e-9, scale = 1e-9, thres = -Inf),
    upper         = c(shape =  Inf, scale =  Inf, thres =  Inf),
    start_fun     = function(x) {
      thres0 <- min(x) - 0.01 * diff(range(x))
      xs     <- x - thres0
      cv_val <- stats::sd(xs) / mean(xs)
      shape0 <- max(0.5, cv_val^(-1.086))
      list(shape = shape0, scale = mean(xs) / gamma(1 + 1 / shape0), thres = thres0)
    },
    support       = "real",       # threshold can shift support freely
    package       = NULL,         # uses internal dweibull3p / pweibull3p
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "wei",        # parwei returns zeta (threshold) as well
    formula_text  = "f(x) = (k/lambda)((x-mu)/lambda)^(k-1) exp(-((x-mu)/lambda)^k),  x>=mu",
    plotmath_str  = "f(x)==frac(k,lambda)*(frac(x-mu,lambda))^{k-1}*exp(-((x-mu)/lambda)^k)",
    family        = "Right-skewed",
    description   = "Three-parameter Weibull with a location shift; widely used in reliability, hydrology, and survival analysis."
  ),

  # -- 12. Log-Logistic ------------------------------------------------------
  loglogistic = list(
    display_name  = "Log-Logistic",
    r_name        = "llogis",
    params        = c("shape", "scale"),
    param_labels  = c("beta", "theta"),
    lower         = c(shape = 1e-9, scale = 1e-9),
    upper         = c(shape =  Inf, scale =  Inf),
    start_fun     = function(x) list(shape = 3, scale = stats::median(x)),
    support       = "positive",
    package       = "actuar",
    supports_mle  = TRUE,
    supports_mom  = TRUE,    # closed-form MOM for shape > 2
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = (beta/theta)(x/theta)^(beta-1) / (1+(x/theta)^beta)^2,  x>0",
    plotmath_str  = "f(x)==frac((beta/theta)*(x/theta)^{beta-1},(1+(x/theta)^beta)^2)",
    family        = "Right-skewed",
    description   = "Right-skewed distribution with heavier tail than log-normal; common in survival analysis."
  ),

  # -- 13. Inverse Gamma -----------------------------------------------------
  invgamma = list(
    display_name  = "Inverse Gamma",
    r_name        = "invgamma",
    params        = c("shape", "scale"),
    param_labels  = c("alpha", "beta"),
    lower         = c(shape = 1e-9, scale = 1e-9),
    upper         = c(shape =  Inf, scale =  Inf),
    start_fun     = function(x) list(shape = 3, scale = mean(x) * 2),
    support       = "positive",
    package       = "actuar",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = beta^alpha x^(-alpha-1) exp(-beta/x) / Gamma(alpha),  x>0",
    plotmath_str  = "f(x)==frac(beta^alpha*x^{-alpha-1}*exp(-beta/x),Gamma(alpha))",
    family        = "Right-skewed",
    description   = "Distribution of the reciprocal of a Gamma variable; used in Bayesian analysis."
  ),

  # -- 14. Inverse Weibull ---------------------------------------------------
  invweibull = list(
    display_name  = "Inverse Weibull",
    r_name        = "invweibull",
    params        = c("shape", "scale"),
    param_labels  = c("gamma", "theta"),
    lower         = c(shape = 1e-9, scale = 1e-9),
    upper         = c(shape =  Inf, scale =  Inf),
    start_fun     = function(x) list(shape = 2, scale = stats::median(x)),
    support       = "positive",
    package       = "actuar",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = gammatheta^gamma x^(-gamma-1) exp(-(theta/x)^gamma),  x>0",
    plotmath_str  = "f(x)==gamma*theta^gamma*x^{-gamma-1}*exp(-(theta/x)^gamma)",
    family        = "Right-skewed",
    description   = "Reciprocal-Weibull model; used in reliability and failure-time applications."
  ),

  # -- 15. Paralogistic ------------------------------------------------------
  paralogistic = list(
    display_name  = "Paralogistic",
    r_name        = "paralogis",
    params        = c("shape", "scale"),
    param_labels  = c("alpha", "theta"),
    lower         = c(shape = 1e-9, scale = 1e-9),
    upper         = c(shape =  Inf, scale =  Inf),
    start_fun     = function(x) list(shape = 2, scale = mean(x)),
    support       = "positive",
    package       = "actuar",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = alpha^2(x/theta)^alpha / (x(1+(x/theta)^alpha)^(alpha+1)),  x>0",
    plotmath_str  = "f(x)==frac(alpha^2*(x/theta)^alpha,x*(1+(x/theta)^alpha)^{alpha+1})",
    family        = "Right-skewed",
    description   = "Burr XII special case (alpha = gamma); popular in actuarial loss modelling."
  ),

  # -- 16. Pearson Type III --------------------------------------------------
  pearson3 = list(
    display_name  = "Pearson Type III",
    r_name        = "pearson3",
    params        = c("shape", "scale", "loc"),
    param_labels  = c("alpha", "beta", "zeta"),
    lower         = c(shape = 1e-9, scale = -Inf, loc = -Inf),
    upper         = c(shape =  Inf, scale =  Inf, loc =  Inf),
    start_fun     = function(x) {
      mu  <- mean(x); sig <- stats::sd(x); n <- length(x)
      gam <- if (sig == 0) 0.1 else sum(((x - mu) / sig)^3) / n
      if (abs(gam) < 1e-4) gam <- 0.1
      list(shape = 4 / gam^2, scale = sig * gam / 2, loc = mu - 2 * sig / gam)
    },
    support       = "real",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "pe3",
    formula_text  = "f(x) = |1/beta|/Gamma(alpha) . ((x-zeta)/beta)^(alpha-1) exp(-(x-zeta)/beta),  (x-zeta)/beta>0",
    plotmath_str  = "f(x)==frac(abs(1/beta),Gamma(alpha))*((x-zeta)/beta)^{alpha-1}*exp(-(x-zeta)/beta)",
    family        = "Right-skewed",
    description   = "Shifted/scaled Gamma; widely used in hydrology for flood frequency analysis."
  ),

  # -- 17. Log-Pearson Type III ----------------------------------------------
  lpearson3 = list(
    display_name  = "Log-Pearson Type III",
    r_name        = "lpearson3",
    params        = c("shape", "scale", "loc"),
    param_labels  = c("alpha", "beta", "zeta"),
    lower         = c(shape = 1e-9, scale = -Inf, loc = -Inf),
    upper         = c(shape =  Inf, scale =  Inf, loc =  Inf),
    start_fun     = function(x) {
      if (any(x <= 0)) return(list(shape = 2, scale = 0.5, loc = 0))
      lx  <- log(x); mu <- mean(lx); sig <- stats::sd(lx); n <- length(lx)
      gam <- if (sig == 0) 0.1 else sum(((lx - mu) / sig)^3) / n
      if (abs(gam) < 1e-4) gam <- 0.1
      list(shape = 4 / gam^2, scale = sig * gam / 2, loc = mu - 2 * sig / gam)
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "pe3",
    formula_text  = "log X ~ Pearson III(alpha, beta, zeta);  f(x) = f_P3(log x)/x,  x>0",
    plotmath_str  = "f(x)==f[P3](log(x))/x",
    family        = "Right-skewed",
    description   = "Applies Pearson III to log(X); standard US Bulletin 17C method for flood frequency."
  ),

  # -- 18. Inverse Gaussian --------------------------------------------------
  invgaussian = list(
    display_name  = "Inverse Gaussian",
    r_name        = "invgaussian",
    params        = c("mu", "lambda"),
    param_labels  = c("mu", "lambda"),
    lower         = c(mu = 1e-9, lambda = 1e-9),
    upper         = c(mu =  Inf, lambda =  Inf),
    start_fun     = function(x) {
      mu0     <- mean(x)
      lambda0 <- length(x) / sum(1 / x - 1 / mu0)
      list(mu = mu0, lambda = max(lambda0, 1e-6))
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,   # lmomco::parwig is not a public export
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = sqrt(lambda/(2pix^3)) exp(-lambda(x-mu)^2/(2mu^2x)),  x>0",
    plotmath_str  = "f(x)==sqrt(lambda/(2*pi*x^3))*exp(-lambda*(x-mu)^2/(2*mu^2*x))",
    family        = "Right-skewed",
    description   = "Wald distribution; arises as the first-passage time of Brownian motion with positive drift."
  ),

  # -- 19. Generalised Gamma -------------------------------------------------
  gengamma = list(
    display_name  = "Generalised Gamma",
    r_name        = "gengamma",
    params        = c("p", "d", "b"),
    param_labels  = c("p", "d", "b"),
    lower         = c(p = 1e-9, d = 1e-9, b = 1e-9),
    upper         = c(p =  Inf, d =  Inf, b =  Inf),
    start_fun     = function(x) {
      m <- mean(x); v <- stats::var(x)
      d0 <- m^2 / v
      list(p = 1, d = d0, b = m / d0)   # p=1 reduces to Gamma
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = (p/Gamma(d/p)) (x/b)^(d-1) exp(-(x/b)^p) / b,  x>0",
    plotmath_str  = "f(x)==frac(p,Gamma(d/p))*(x/b)^{d-1}*exp(-(x/b)^p)/b",
    family        = "Right-skewed",
    description   = "Three-parameter generalisation of the Gamma; nests Gamma (p=1), Weibull (d=p), and log-normal (limiting)."
  ),

  # -- 20. Gompertz ----------------------------------------------------------
  gompertz = list(
    display_name  = "Gompertz",
    r_name        = "gompertz",
    params        = c("alpha", "beta"),
    param_labels  = c("alpha", "beta"),
    lower         = c(alpha = 1e-9, beta = 1e-9),
    upper         = c(alpha =  Inf, beta =  Inf),
    start_fun     = function(x) list(alpha = 1 / mean(x), beta = 0.5),
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = alpha exp(betax) exp(-alpha/beta (exp(betax)-1)),  x>=0",
    plotmath_str  = "f(x)==alpha*exp(beta*x)*exp(-alpha/beta*(exp(beta*x)-1))",
    family        = "Right-skewed",
    description   = "Actuarial and demographic hazard model with exponentially increasing hazard rate."
  ),

  # -- 21. Birnbaum-Saunders -------------------------------------------------
  birnbaum_saunders = list(
    display_name  = "Birnbaum-Saunders",
    r_name        = "birnbaum_saunders",
    params        = c("alpha", "beta"),
    param_labels  = c("alpha", "beta"),
    lower         = c(alpha = 1e-9, beta = 1e-9),
    upper         = c(alpha =  Inf, beta =  Inf),
    start_fun     = function(x) {
      nu_tilde <- mean(sqrt(x))
      mu_tilde <- mean(1 / sqrt(x))
      beta0    <- nu_tilde / mu_tilde
      alpha0   <- sqrt(nu_tilde * mu_tilde - 1)
      list(alpha = max(alpha0, 1e-4), beta = max(beta0, 1e-6))
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = phi(z/alpha)(sqrt(x/beta)+sqrt(beta/x)) / (2alphax),  z=sqrt(x/beta)-sqrt(beta/x)",
    plotmath_str  = "f(x)==phi(z/alpha)*(sqrt(x/beta)+sqrt(beta/x))/(2*alpha*x)",
    family        = "Right-skewed",
    description   = "Fatigue life distribution derived from fracture mechanics; widely used in reliability engineering."
  ),

  # -- 22. Rayleigh ----------------------------------------------------------
  rayleigh = list(
    display_name  = "Rayleigh",
    r_name        = "rayleigh",
    params        = c("sigma"),
    param_labels  = c("sigma"),
    lower         = c(sigma = 1e-9),
    upper         = c(sigma = Inf),
    start_fun     = function(x) list(sigma = sqrt(mean(x^2) / 2)),
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = (x/sigma^2) exp(-x^2/(2sigma^2)),  x>0",
    plotmath_str  = "f(x)==(x/sigma^2)*exp(-x^2/(2*sigma^2))",
    family        = "Right-skewed",
    description   = "Special case of Weibull (shape=2); models wind speed, wave height, and 2-D random walk distances."
  ),

  # -- 23. Half-Normal -------------------------------------------------------
  halfnormal = list(
    display_name  = "Half-Normal",
    r_name        = "halfnormal",
    params        = c("sigma"),
    param_labels  = c("sigma"),
    lower         = c(sigma = 1e-9),
    upper         = c(sigma = Inf),
    start_fun     = function(x) list(sigma = sqrt(mean(x^2))),
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = sqrt(2/pi)/sigma exp(-x^2/(2sigma^2)),  x>=0",
    plotmath_str  = "f(x)==sqrt(2/pi)/sigma*exp(-x^2/(2*sigma^2))",
    family        = "Right-skewed",
    description   = "Absolute value of a zero-mean normal; used for measurement error and non-negative residuals."
  ),

  # -- 24. Exponentiated Weibull ---------------------------------------------
  expweibull = list(
    display_name  = "Exponentiated Weibull",
    r_name        = "expweibull",
    params        = c("shape1", "scale", "shape2"),
    param_labels  = c("alpha", "lambda", "beta"),
    lower         = c(shape1 = 1e-4, scale = 1e-9, shape2 = 1e-4),
    upper         = c(shape1 = Inf,  scale = Inf,   shape2 = Inf),
    start_fun     = function(x) list(shape1 = 1.5, scale = mean(x) * 0.9, shape2 = 1.0),
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "F(x) = (1 - exp(-(x/lambda)^alpha))^beta",
    plotmath_str  = "F(x)==(1-exp(-(x/lambda)^alpha))^beta",
    family        = "Right-skewed",
    description   = "Three-parameter Weibull extension via CDF exponentiation; supports bathtub and unimodal hazard rates."
  ),

  # -- 25. Gamma (3-Parameter) -----------------------------------------------
  gamma3 = list(
    display_name  = "Gamma (3-Parameter)",
    r_name        = "gamma3",
    params        = c("shape", "scale", "thres"),
    param_labels  = c("alpha", "beta", "gamma"),
    lower         = c(shape = 1e-4, scale = 1e-9, thres = -Inf),
    upper         = c(shape = Inf,  scale = Inf,   thres = Inf),
    start_fun     = function(x) {
      m  <- mean(x); v <- stats::var(x)
      sk <- mean((x - m)^3) / v^1.5
      sh <- pmax(4 / sk^2, 0.1)
      sc <- sqrt(v / sh)
      th <- min(m - sh * sc, min(x) - 1e-3 * stats::sd(x))
      list(shape = sh, scale = sc, thres = th)
    },
    support       = "threshold",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = dgamma(x-gamma; alpha, beta),  x>gamma  (equiv. Pearson III)",
    plotmath_str  = "f(x)==dgamma(x-gamma,alpha,beta)",
    family        = "Right-skewed",
    description   = "Gamma with a location threshold; equivalent to Pearson Type III. Used when data has a known lower bound."
  ),

  # -- 26. Log-Normal (3-Parameter) ------------------------------------------
  lnorm3 = list(
    display_name  = "Log-Normal (3-Parameter)",
    r_name        = "lnorm3",
    params        = c("meanlog", "sdlog", "thres"),
    param_labels  = c("mu", "sigma", "gamma"),
    lower         = c(meanlog = -Inf, sdlog = 1e-9, thres = -Inf),
    upper         = c(meanlog =  Inf, sdlog = Inf,   thres = Inf),
    start_fun     = function(x) {
      th0 <- min(x) - 0.1 * stats::sd(x)
      y   <- log(x - th0)
      list(meanlog = mean(y), sdlog = stats::sd(y), thres = th0)
    },
    support       = "threshold",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = dlnorm(x-gamma; mu, sigma),  x>gamma",
    plotmath_str  = "f(x)==dlnorm(x-gamma,mu,sigma)",
    family        = "Right-skewed",
    description   = "Log-Normal shifted by a lower threshold; common in environmental science and pollution modelling."
  ),

  # ============================================================
  # HEAVY-TAILED  (entries 27-29)
  # ============================================================

  # -- 27. Pareto ------------------------------------------------------------
  pareto = list(
    display_name  = "Pareto",
    r_name        = "pareto",
    params        = c("shape", "scale"),
    param_labels  = c("alpha", "theta"),
    lower         = c(shape = 1e-9, scale = 1e-9),
    upper         = c(shape =  Inf, scale =  Inf),
    start_fun     = function(x) list(shape = 3, scale = mean(x) * 2),
    support       = "positive",
    package       = "actuar",
    supports_mle  = TRUE,
    supports_mom  = TRUE,   # requires shape > 2 for finite variance
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = alphatheta^alpha / x^(alpha+1),  x>=theta",
    plotmath_str  = "f(x)==frac(alpha*theta^alpha,x^{alpha+1})",
    family        = "Heavy-tailed",
    description   = "Power-law tail distribution; used for income, wealth, and insurance loss modelling."
  ),

  # -- 28. Burr XII ----------------------------------------------------------
  burr = list(
    display_name  = "Burr XII",
    r_name        = "burr",
    params        = c("shape1", "shape2", "scale"),
    param_labels  = c("alpha", "gamma", "theta"),
    lower         = c(shape1 = 1e-9, shape2 = 1e-9, scale = 1e-9),
    upper         = c(shape1 =  Inf, shape2 =  Inf, scale =  Inf),
    start_fun     = function(x) list(shape1 = 2, shape2 = 2, scale = stats::median(x)),
    support       = "positive",
    package       = "actuar",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = alphagamma(x/theta)^(gamma-1) / (theta(1+(x/theta)^gamma)^(alpha+1)),  x>0",
    plotmath_str  = "f(x)==frac(alpha*gamma*(x/theta)^{gamma-1},theta*(1+(x/theta)^gamma)^{alpha+1})",
    family        = "Heavy-tailed",
    description   = "Highly flexible three-parameter distribution encompassing many skewed families."
  ),

  # -- 29. Lomax (Pareto Type II) --------------------------------------------
  lomax = list(
    display_name  = "Lomax (Pareto Type II)",
    r_name        = "lomax",
    params        = c("alpha", "lambda"),
    param_labels  = c("alpha", "lambda"),
    lower         = c(alpha = 1e-9, lambda = 1e-9),
    upper         = c(alpha =  Inf, lambda =  Inf),
    start_fun     = function(x) {
      m <- mean(x); v <- stats::var(x)
      alpha0  <- max(2 * v / (v - m^2), 2.01)
      list(alpha = alpha0, lambda = m * (alpha0 - 1))
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = (alpha/lambda)(1+x/lambda)^(-(alpha+1)),  x>=0",
    plotmath_str  = "f(x)==frac(alpha,lambda)*(1+x/lambda)^{-(alpha+1)}",
    family        = "Heavy-tailed",
    description   = "Pareto Type II distribution; zero-shifted Pareto used in reliability and actuarial science."
  ),

  # ============================================================
  # EXTREME-VALUE  (entries 30-33)
  # ============================================================

  # -- 30. Gumbel ------------------------------------------------------------
  gumbel = list(
    display_name  = "Gumbel",
    r_name        = "gumbel",
    params        = c("loc", "scale"),
    param_labels  = c("mu", "beta"),
    lower         = c(loc = -Inf, scale = 1e-9),
    upper         = c(loc =  Inf, scale =  Inf),
    start_fun     = function(x) {
      sc <- stats::sd(x) * sqrt(6) / pi
      list(loc = mean(x) - 0.5772156649 * sc, scale = sc)
    },
    support       = "real",
    package       = "evd",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "gum",
    formula_text  = "f(x) = (1/beta) exp(-(z+e^{-z})),  z=(x-mu)/beta",
    plotmath_str  = "f(x)==frac(1,beta)*exp(-(z+exp(-z)))",
    family        = "Extreme-value",
    description   = "Type I extreme-value distribution; arises as the limit of sample maxima."
  ),

  # -- 31. Generalised Extreme Value (GEV) -----------------------------------
  gev = list(
    display_name  = "Generalised Extreme Value",
    r_name        = "gev",
    params        = c("loc", "scale", "shape"),
    param_labels  = c("mu", "sigma", "xi"),
    lower         = c(loc = -Inf, scale = 1e-9, shape = -Inf),
    upper         = c(loc =  Inf, scale =  Inf, shape =  Inf),
    start_fun     = function(x) {
      sc <- stats::sd(x) * sqrt(6) / pi
      list(loc = mean(x) - 0.5772 * sc, scale = sc, shape = 0)
    },
    support       = "real",
    package       = "evd",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = TRUE,
    lmomco_type   = "gev",
    formula_text  = "f(x) = (1/sigma)[1+xi((x-mu)/sigma)]^(-1/xi-1) exp(-[1+xi((x-mu)/sigma)]^(-1/xi))",
    plotmath_str  = "f(x)==frac(1,sigma)*(1+xi*(x-mu)/sigma)^{-1/xi-1}*exp(-(1+xi*(x-mu)/sigma)^{-1/xi})",
    family        = "Extreme-value",
    description   = "Unifies Gumbel (xi=0), Frechet (xi>0) and Weibull (xi<0) extreme-value families."
  ),

  # -- 32. Generalised Pareto (GPD) ------------------------------------------
  gpd = list(
    display_name  = "Generalised Pareto",
    r_name        = "gpd",
    params        = c("loc", "scale", "shape"),
    param_labels  = c("mu", "sigma", "xi"),
    lower         = c(loc = -Inf, scale = 1e-9, shape = -Inf),
    upper         = c(loc =  Inf, scale =  Inf, shape =  Inf),
    start_fun     = function(x) {
      eps <- 1e-4 * (max(x) - min(x) + 1)
      loc <- min(x) - eps
      y   <- x - loc
      m   <- mean(y); v <- stats::var(y)
      xi  <- if (v > 0) max(-0.4, 0.5 * (1 - m^2 / v)) else 0
      list(loc = loc, scale = max(m * (1 - xi), 1e-3), shape = xi)
    },
    support       = "real",       # lower bounded by loc
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = TRUE,
    lmomco_type   = "gpa",
    formula_text  = "f(x) = (1/sigma)(1+xi(x-mu)/sigma)^(-1/xi-1),  x>=mu  (xi>=0)",
    plotmath_str  = "f(x)==frac(1,sigma)*(1+xi*(x-mu)/sigma)^{-1/xi-1}",
    family        = "Extreme-value",
    description   = "Limiting distribution of exceedances over a high threshold; backbone of Peaks-Over-Threshold analysis."
  ),

  # -- 33. Generalised Logistic (GLO) ----------------------------------------
  glo = list(
    display_name  = "Generalised Logistic",
    r_name        = "glo",
    params        = c("loc", "scale", "shape"),
    param_labels  = c("xi", "alpha", "kappa"),
    lower         = c(loc = -Inf, scale = 1e-9, shape = -Inf),
    upper         = c(loc =  Inf, scale =  Inf, shape =  Inf),
    start_fun     = function(x) {
      list(loc   = stats::median(x),
           scale = stats::IQR(x) * sqrt(3) / pi,
           shape = 0)
    },
    support       = "real",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = TRUE,
    lmomco_type   = "glo",
    formula_text  = "F(x) = 1/(1+(1-kappa(x-xi)/alpha)^{1/kappa}),  kappa!=0",
    plotmath_str  = "F(x)==frac(1,1+(1-kappa*(x-xi)/alpha)^{1/kappa})",
    family        = "Extreme-value",
    description   = "Symmetric fat-tailed distribution; standard in UK flood frequency (WINFAP)."
  ),

  # ============================================================
  # BOUNDED / FINITE SUPPORT  (entries 34-39)
  # ============================================================

  # -- 34. Beta --------------------------------------------------------------
  beta = list(
    display_name  = "Beta",
    r_name        = "beta",
    params        = c("shape1", "shape2"),
    param_labels  = c("alpha", "beta"),
    lower         = c(shape1 = 1e-9, shape2 = 1e-9),
    upper         = c(shape1 =  Inf, shape2 =  Inf),
    start_fun     = function(x) {
      m <- mean(x); v <- stats::var(x)
      fac <- max(m * (1 - m) / v - 1, 0.01)
      list(shape1 = m * fac, shape2 = (1 - m) * fac)
    },
    support       = "0to1",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = x^(alpha-1)(1-x)^(beta-1) / B(alpha,beta),  0<x<1",
    plotmath_str  = "f(x)==frac(x^{alpha-1}*(1-x)^{beta-1},B(alpha,beta))",
    family        = "Bounded",
    description   = "Flexible distribution for proportions and probabilities; support (0,1)."
  ),

  # -- 35. Uniform -----------------------------------------------------------
  uniform = list(
    display_name  = "Uniform",
    r_name        = "unif",
    params        = c("min", "max"),
    param_labels  = c("a", "b"),
    lower         = c(min = -Inf, max = -Inf),
    upper         = c(min =  Inf, max =  Inf),
    start_fun     = function(x) {
      rng <- diff(range(x))
      list(min = min(x) - 0.05 * rng, max = max(x) + 0.05 * rng)
    },
    support       = "bounded",
    package       = "stats",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = 1/(b-a),  a<=x<=b",
    plotmath_str  = "f(x)==frac(1,b-a)",
    family        = "Bounded",
    description   = "Equal-probability distribution on the closed interval [a, b]."
  ),

  # -- 36. Triangular --------------------------------------------------------
  triangular = list(
    display_name  = "Triangular",
    r_name        = "triang",
    params        = c("min", "max", "mode"),
    param_labels  = c("a", "b", "c"),
    lower         = c(min = -Inf, max = -Inf, mode = -Inf),
    upper         = c(min =  Inf, max =  Inf, mode =  Inf),
    start_fun     = function(x) {
      list(min  = min(x),
           max  = max(x),
           mode = as.numeric(stats::density(x)$x[which.max(stats::density(x)$y)]))
    },
    support       = "bounded",
    package       = "mc2d",
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = 2(x-a)/((b-a)(c-a)) for a<=x<=c; 2(b-x)/((b-a)(b-c)) for c<x<=b",
    plotmath_str  = "f(x)==frac(2*(x-a),(b-a)*(c-a))*I(x<=c)+frac(2*(b-x),(b-a)*(b-c))*I(x>c)",
    family        = "Bounded",
    description   = "Simple bounded distribution defined by minimum, maximum, and mode."
  ),

  # -- 37. Right Triangular --------------------------------------------------
  rtriangle = list(
    display_name  = "Right Triangular",
    r_name        = "rtriangle",
    params        = c("min", "max"),
    param_labels  = c("a", "b"),
    lower         = c(min = -Inf, max = -Inf),
    upper         = c(min =  Inf, max =  Inf),
    start_fun     = function(x) list(min = min(x), max = max(x)),
    support       = "bounded",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = 2(b-x)/(b-a)^2,  a<=x<=b  (mode fixed at a)",
    plotmath_str  = "f(x)==frac(2*(b-x),(b-a)^2)",
    family        = "Bounded",
    description   = "Right-triangular distribution with mode fixed at the minimum; special case of the triangular family."
  ),

  # -- 38. PERT --------------------------------------------------------------
  pert = list(
    display_name  = "PERT",
    r_name        = "pert",
    params        = c("min", "mode", "max"),
    param_labels  = c("a", "b", "c"),
    lower         = c(min = -Inf, mode = -Inf, max = -Inf),
    upper         = c(min =  Inf, mode =  Inf, max =  Inf),
    start_fun     = function(x) {
      list(min  = min(x),
           mode = as.numeric(stats::density(x)$x[which.max(stats::density(x)$y)]),
           max  = max(x))
    },
    support       = "bounded",
    package       = "mc2d",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "Beta-shaped: f(x) prop.to (x-a)^(alpha1-1)(c-x)^(alpha2-1), alpha from mode",
    plotmath_str  = "f(x) %prop% (x-a)^{alpha[1]-1}*(c-x)^{alpha[2]-1}",
    family        = "Bounded",
    description   = "Smooth beta-shaped distribution used in risk analysis and project scheduling."
  ),

  # -- 39. Kumaraswamy -------------------------------------------------------
  kumaraswamy = list(
    display_name  = "Kumaraswamy",
    r_name        = "kumaraswamy",
    params        = c("shape1", "shape2"),
    param_labels  = c("a", "b"),
    lower         = c(shape1 = 1e-9, shape2 = 1e-9),
    upper         = c(shape1 =  Inf, shape2 =  Inf),
    start_fun     = function(x) {
      m <- mean(x); v <- stats::var(x)
      list(shape1 = max(0.5, m * (1 - m) / v - 1),
           shape2 = max(0.5, (1 - m) * (m * (1 - m) / v - 1) / m))
    },
    support       = "0to1",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = ab.x^(a-1)(1-x^a)^(b-1),  0<x<1",
    plotmath_str  = "f(x)==a*b*x^{a-1}*(1-x^a)^{b-1}",
    family        = "Bounded",
    description   = "Flexible two-parameter distribution on (0,1); tractable closed-form CDF unlike the Beta."
  ),

  # ============================================================
  # EXTENDED FAMILIES  (entries 40-52)
  # ============================================================

  # -- 40. Generalised Beta of the Second Kind (GB2) -------------------------
  gb2 = list(
    display_name  = "GB2 (Generalised Beta of the Second Kind)",
    r_name        = "gb2",
    params        = c("shape1", "scale", "shape2", "shape3"),
    param_labels  = c("p", "b", "a", "q"),
    lower         = c(shape1 = 1e-9, scale = 1e-9, shape2 = 1e-9, shape3 = 1e-9),
    upper         = c(shape1 =  Inf, scale =  Inf, shape2 =  Inf, shape3 =  Inf),
    start_fun     = function(x) {
      # GB2 median ~ scale for typical param values; use median to avoid
      # the large-mean bias when the tail is heavy.
      list(shape1 = 2, scale = stats::median(x), shape2 = 2, shape3 = 2)
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = |a|x^(ap-1) / (b^(ap)B(p,q)(1+(x/b)^a)^(p+q)),  x>0",
    plotmath_str  = "f(x)==frac(abs(a)*x^{a*p-1},b^{a*p}*B(p,q)*(1+(x/b)^a)^{p+q})",
    family        = "Heavy-tailed",
    description   = "Four-parameter generalisation nesting Burr XII, Dagum, log-logistic, and Pareto as special cases."
  ),

  # -- 41. Kappa-4 (Hosking 1994) -------------------------------------------
  kappa4 = list(
    display_name  = "Kappa-4",
    r_name        = "kap4",
    params        = c("xi", "alpha", "kappa", "h"),
    param_labels  = c("xi", "alpha", "kappa", "h"),
    lower         = c(xi = -Inf, alpha = 1e-9, kappa = -Inf, h = -Inf),
    upper         = c(xi =  Inf, alpha =  Inf, kappa =  Inf, h =  Inf),
    start_fun     = function(x) {
      # Best MLE start: use L-moment estimates (LMOM and MLE usually give similar
      # results for kappa4, so LMOM is an excellent starting point for the optimizer).
      tryCatch({
        if (requireNamespace("lmomco", quietly = TRUE)) {
          lmom    <- lmomco::lmoms(x, nmom = 4)
          par_obj <- lmomco::parkap(lmom)
          if (!is.null(par_obj) && all(is.finite(par_obj$para)) &&
              par_obj$para["alpha"] > 0) {
            return(list(xi    = unname(par_obj$para["xi"]),
                        alpha = unname(par_obj$para["alpha"]),
                        kappa = unname(par_obj$para["kappa"]),
                        h     = unname(par_obj$para["h"])))
          }
        }
        sc <- stats::sd(x) * sqrt(6) / pi
        list(xi = mean(x) - 0.5772 * sc, alpha = sc, kappa = 0.05, h = 0.05)
      }, error = function(e) {
        sc <- stats::sd(x) * sqrt(6) / pi
        list(xi = mean(x) - 0.5772 * sc, alpha = sc, kappa = 0.05, h = 0.05)
      })
    },
    support       = "real",
    package       = "lmomco",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = TRUE,
    lmomco_type   = "kap",
    formula_text  = "Q(F;xi,alpha,kappa,h) = xi + alpha*(1-((1-F^h)/h)^kappa)/kappa",
    plotmath_str  = "Q(F)==xi+frac(alpha,kappa)*(1-((1-F^h)/h)^kappa)",
    family        = "Extreme-value",
    description   = "Four-parameter family (Hosking 1994) encompassing GEV, GPD, logistic and uniform; ideal for L-moment fitting."
  ),

  # -- 42. Lindley -----------------------------------------------------------
  lindley = list(
    display_name  = "Lindley",
    r_name        = "lindley",
    params        = c("theta"),
    param_labels  = c("theta"),
    lower         = c(theta = 1e-9),
    upper         = c(theta = Inf),
    start_fun     = function(x) {
      xbar  <- mean(x)
      disc  <- (xbar - 1)^2 + 8 * xbar
      theta0 <- max((-(xbar - 1) + sqrt(disc)) / (2 * xbar), 0.01)
      list(theta = theta0)
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = theta^2/(1+theta) (1+x) exp(-theta*x),  x>0",
    plotmath_str  = "f(x)==frac(theta^2,1+theta)*(1+x)*exp(-theta*x)",
    family        = "Right-skewed",
    description   = "One-parameter mixture of exponential and gamma; widely used in reliability and survival analysis."
  ),

  # -- 43. Generalised Normal (Exponential Power) ----------------------------
  gnorm = list(
    display_name  = "Generalised Normal",
    r_name        = "gnorm",
    params        = c("mu", "sigma", "beta"),
    param_labels  = c("mu", "sigma", "beta"),
    lower         = c(mu = -Inf, sigma = 1e-9, beta = 1e-9),
    upper         = c(mu =  Inf, sigma =  Inf, beta =  Inf),
    start_fun     = function(x) {
      list(mu = mean(x), sigma = stats::sd(x), beta = 2.0)
    },
    support       = "real",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = beta/(2*sigma*Gamma(1/beta)) exp(-(|x-mu|/sigma)^beta)",
    plotmath_str  = "f(x)==frac(beta,2*sigma*Gamma(1/beta))*exp(-(abs(x-mu)/sigma)^beta)",
    family        = "Symmetric",
    description   = "Exponential power family: beta=2 is Normal, beta=1 is Laplace; models leptokurtic and platykurtic data."
  ),

  # -- 44. Dagum (Burr III) --------------------------------------------------
  dagum = list(
    display_name  = "Dagum",
    r_name        = "dagum",
    params        = c("shape1", "scale", "shape2"),
    param_labels  = c("p", "b", "a"),
    lower         = c(shape1 = 1e-9, scale = 1e-9, shape2 = 1e-9),
    upper         = c(shape1 =  Inf, scale =  Inf, shape2 =  Inf),
    start_fun     = function(x) list(shape1 = 1, scale = stats::median(x), shape2 = 2),
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = (a*p/x)(x/b)^(a*p)/(1+(x/b)^a)^(p+1),  x>0",
    plotmath_str  = "f(x)==frac(a*p,x)*(x/b)^{a*p}/(1+(x/b)^a)^{p+1}",
    family        = "Heavy-tailed",
    description   = "Three-parameter income distribution; supports unimodal and monotone decreasing density shapes."
  ),

  # -- 45. Johnson SU --------------------------------------------------------
  johnsonsu = list(
    display_name  = "Johnson SU",
    r_name        = "johnsonsu",
    params        = c("gamma", "delta", "xi", "lambda"),
    param_labels  = c("gamma", "delta", "xi", "lambda"),
    lower         = c(gamma = -Inf, delta = 1e-9, xi = -Inf, lambda = 1e-9),
    upper         = c(gamma =  Inf, delta =  Inf, xi =  Inf, lambda =  Inf),
    start_fun     = function(x) {
      # For JohnsonSU with gamma=0, delta=1: the IQR ~ 2*sinh(0.6745)*lambda.
      # sinh(0.6745) ~ 0.7163, so IQR ~ 1.4326*lambda => lambda ~ IQR/1.4326.
      # This avoids the over-estimate from sd(x) (which is inflated by the heavy tails).
      iqr    <- diff(stats::quantile(x, c(0.25, 0.75)))
      lam0   <- max(iqr / 1.4326, 1e-9)
      list(gamma = 0, delta = 1, xi = stats::median(x), lambda = lam0)
    },
    support       = "real",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = delta/[lambda*sqrt(2pi(1+z^2))] exp(-0.5*(gamma+delta*arcsinh(z))^2),  z=(x-xi)/lambda",
    plotmath_str  = "f(x)==frac(delta,lambda*sqrt(2*pi*(1+z^2)))*exp(-0.5*(gamma+delta*asinh(z))^2)",
    family        = "Asymmetric",
    description   = "Flexible four-parameter transformation family covering a wide range of skewness and kurtosis combinations."
  ),

  # -- 46. Asymmetric Laplace ------------------------------------------------
  alaplace = list(
    display_name  = "Asymmetric Laplace",
    r_name        = "alaplace",
    params        = c("mu", "sigma", "kappa"),
    param_labels  = c("mu", "sigma", "kappa"),
    lower         = c(mu = -Inf, sigma = 1e-9, kappa = 1e-9),
    upper         = c(mu =  Inf, sigma =  Inf, kappa =  Inf),
    start_fun     = function(x) {
      mu0 <- stats::median(x)
      list(mu = mu0, sigma = mean(abs(x - mu0)), kappa = 1)
    },
    support       = "real",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = kappa/(sigma(1+kappa^2)) exp(-kappa(x-mu)/sigma) x>=mu;  kappa/(sigma(1+kappa^2)) exp((x-mu)/(kappa*sigma)) x<mu",
    plotmath_str  = "f(x)==frac(kappa,sigma*(1+kappa^2))*exp(ifelse(x>=mu,-kappa*(x-mu)/sigma,(x-mu)/(kappa*sigma)))",
    family        = "Asymmetric",
    description   = "Asymmetric double exponential; arises in quantile regression and financial return modelling."
  ),

  # -- 47. Exponentiated Exponential -----------------------------------------
  expexp = list(
    display_name  = "Exponentiated Exponential",
    r_name        = "expexp",
    params        = c("alpha", "lambda"),
    param_labels  = c("alpha", "lambda"),
    lower         = c(alpha = 1e-9, lambda = 1e-9),
    upper         = c(alpha =  Inf, lambda =  Inf),
    start_fun     = function(x) {
      m  <- mean(x)
      cv <- stats::sd(x) / max(m, 1e-9)
      # For expexp(alpha, lambda): CV decreases as alpha increases.
      # Rough MOM: alpha ~ 1/CV^2 (approaches exponential CV=1 at alpha=1).
      alpha0  <- max(1 / cv^2, 0.5)
      # E[X] = (digamma(alpha+1) + euler_gamma) / lambda
      euler_g <- 0.5772156649
      g       <- tryCatch(digamma(alpha0 + 1) + euler_g, error = function(e) 1)
      lambda0 <- if (is.finite(g) && g > 0) g / m else 1 / m
      list(alpha = alpha0, lambda = max(lambda0, 1e-9))
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = alpha*lambda*exp(-lambda*x)*(1-exp(-lambda*x))^(alpha-1),  x>0",
    plotmath_str  = "f(x)==alpha*lambda*exp(-lambda*x)*(1-exp(-lambda*x))^{alpha-1}",
    family        = "Right-skewed",
    description   = "Power-raised exponential CDF; bridges exponential (alpha=1) and more flexible skewed shapes."
  ),

  # -- 48. Normal Inverse Gaussian (NIG) -------------------------------------
  nig = list(
    display_name  = "Normal Inverse Gaussian",
    r_name        = "nig",
    params        = c("mu", "delta", "alpha", "beta"),
    param_labels  = c("mu", "delta", "alpha", "beta"),
    lower         = c(mu = -Inf, delta = 1e-9, alpha = 1e-9, beta = -Inf),
    upper         = c(mu =  Inf, delta =  Inf, alpha =  Inf, beta =  Inf),
    start_fun     = function(x) {
      m  <- mean(x)
      v  <- max(stats::var(x), 1e-9)
      # Method-of-moments for NIG (assuming beta=0):
      #   Var = delta/alpha,  excess kurtosis = 3/(delta*alpha)
      # => delta = sqrt(3*v/ek), alpha = sqrt(3/(v*ek))
      s4 <- mean((x - m)^4)
      ek <- max(s4 / v^2 - 3, 0.2)   # NIG always has positive excess kurtosis
      list(mu    = m,
           delta = max(sqrt(3 * v / ek), 1e-4),
           alpha = max(sqrt(3 / (v * ek)), 1e-4),
           beta  = 0)
    },
    support       = "real",
    package       = "GeneralizedHyperbolic",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = alpha*delta*K1(alpha*sqrt(delta^2+(x-mu)^2)) / (pi*sqrt(delta^2+(x-mu)^2)) exp(delta*gamma+beta*(x-mu))",
    plotmath_str  = "f(x)==frac(alpha*delta*K[1](alpha*sqrt(delta^2+(x-mu)^2)),pi*sqrt(delta^2+(x-mu)^2))*e^{delta*gamma+beta*(x-mu)}",
    family        = "Symmetric",
    description   = "Heavy-tailed semi-heavy distribution; widely used in financial return modelling and turbulence."
  ),

  # -- 49. Wakeby ------------------------------------------------------------
  wakeby = list(
    display_name  = "Wakeby",
    r_name        = "wakeby",
    params        = c("xi", "alpha", "beta", "gamma", "delta"),
    param_labels  = c("xi", "alpha", "beta", "gamma", "delta"),
    lower         = c(xi = -Inf, alpha = 0,    beta = 0,    gamma = 0,    delta = 0),
    upper         = c(xi =  Inf, alpha = Inf,  beta = Inf,  gamma = Inf,  delta = Inf),
    start_fun     = function(x) {
      list(xi = min(x), alpha = mean(x), beta = 1, gamma = 0.1, delta = 0.1)
    },
    support       = "real",
    package       = "lmomco",
    supports_mle  = FALSE,
    supports_mom  = FALSE,
    supports_lmom = TRUE,
    lmomco_type   = "wak",
    formula_text  = "Q(F) = xi + alpha*(1-(1-F)^beta)/beta - gamma*(1-(1-F)^(-delta))/delta",
    plotmath_str  = "Q(F)==xi+frac(alpha,beta)*(1-(1-F)^beta)-frac(gamma,delta)*(1-(1-F)^{-delta})",
    family        = "Extreme-value",
    description   = "Five-parameter quantile-based distribution; the most flexible L-moment family for flood frequency analysis."
  ),

  # -- 50. Generalised Hyperbolic --------------------------------------------
  ghyp = list(
    display_name  = "Generalised Hyperbolic",
    r_name        = "ghyp",
    params        = c("lambda", "alpha", "beta", "delta", "mu"),
    param_labels  = c("lambda", "alpha", "beta", "delta", "mu"),
    lower         = c(lambda = -Inf, alpha = 1e-9, beta = -Inf, delta = 1e-9, mu = -Inf),
    upper         = c(lambda =  Inf, alpha =  Inf, beta =  Inf, delta =  Inf, mu =  Inf),
    start_fun     = function(x) {
      m  <- mean(x)
      v  <- max(stats::var(x), 1e-9)
      # Use excess kurtosis to estimate delta and alpha (NIG approximation):
      # ek = 3/(delta*alpha), v = delta/alpha => delta=sqrt(3v/ek), alpha=sqrt(3/(v*ek))
      s4 <- mean((x - m)^4)
      ek <- max(s4 / v^2 - 3, 0.2)
      d0 <- max(sqrt(3 * v / ek), 1e-4)
      a0 <- max(sqrt(3 / (v * ek)), 1e-4)
      list(lambda = -0.5, alpha = a0, beta = 0, delta = d0, mu = m)
    },
    support       = "real",
    package       = "GeneralizedHyperbolic",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) proportional to (delta^2+(x-mu)^2)^(lambda/2-1/4) K_{lambda-1/2}(alpha*sqrt(delta^2+(x-mu)^2)) exp(beta*(x-mu))",
    plotmath_str  = "f(x) %prop% (delta^2+(x-mu)^2)^{lambda/2-1/4}*K[lambda-1/2]*exp(beta*(x-mu))",
    family        = "Symmetric",
    description   = "Five-parameter superfamily nesting NIG (lambda=-1/2), hyperbolic (lambda=1), and variance-gamma (delta->0)."
  ),

  # -- 51. Truncated Normal --------------------------------------------------
  truncnorm = list(
    display_name  = "Truncated Normal",
    r_name        = "truncnorm",
    params        = c("mean", "sd", "a", "b"),
    param_labels  = c("mu", "sigma", "a", "b"),
    lower         = c(mean = -Inf, sd = 1e-9, a = -Inf, b = -Inf),
    upper         = c(mean =  Inf, sd =  Inf, a =  Inf, b =  Inf),
    start_fun     = function(x) {
      list(mean = mean(x), sd = stats::sd(x),
           a = min(x) - 0.1 * stats::sd(x),
           b = max(x) + 0.1 * stats::sd(x))
    },
    support       = "real",
    package       = "truncnorm",
    supports_mle  = TRUE,
    supports_mom  = FALSE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = phi((x-mu)/sigma) / (sigma*(Phi((b-mu)/sigma)-Phi((a-mu)/sigma))),  a<=x<=b",
    plotmath_str  = "f(x)==frac(phi((x-mu)/sigma),sigma*(Phi((b-mu)/sigma)-Phi((a-mu)/sigma)))",
    family        = "Symmetric",
    description   = "Normal distribution restricted to [a, b]; useful when data has a known physical lower or upper bound."
  ),

  # -- 52. Nakagami ----------------------------------------------------------
  nakagami = list(
    display_name  = "Nakagami",
    r_name        = "nakagami",
    params        = c("m", "Omega"),
    param_labels  = c("m", "Omega"),
    lower         = c(m = 0.5, Omega = 1e-9),
    upper         = c(m = Inf, Omega = Inf),
    start_fun     = function(x) {
      Omega0 <- mean(x^2)
      m0     <- max(0.5, mean(x)^2 / (Omega0 - mean(x)^2 + 1e-9))
      list(m = m0, Omega = Omega0)
    },
    support       = "positive",
    package       = NULL,
    supports_mle  = TRUE,
    supports_mom  = TRUE,
    supports_lmom = FALSE,
    lmomco_type   = NA_character_,
    formula_text  = "f(x) = 2*m^m/(Gamma(m)*Omega^m) x^(2m-1) exp(-m*x^2/Omega),  x>0",
    plotmath_str  = "f(x)==frac(2*m^m,Gamma(m)*Omega^m)*x^{2*m-1}*exp(-m*x^2/Omega)",
    family        = "Right-skewed",
    description   = "Nakagami-m fading channel model; generalises Rayleigh (m=1) and one-sided Normal (m=0.5)."
  )

)  # end FITVERSE_DIST_REGISTRY


# -- Public helper: list all distributions ------------------------------------

#' List all distributions supported by FitVerse
#'
#' Returns a summary data frame of every distribution in the registry,
#' showing which estimation methods are available for each.
#'
#' @return A data frame with columns Key, Name, Family, Package, MLE, MOM, LMOM.
#' @export
#' @examples
#' fitverse_distributions()
fitverse_distributions <- function(method = NULL) {
  rows <- lapply(names(FITVERSE_DIST_REGISTRY), function(k) {
    e <- FITVERSE_DIST_REGISTRY[[k]]
    data.frame(
      Key     = k,
      Name    = e$display_name,
      Family  = e$family,
      Package = ifelse(is.null(e$package), "internal", e$package),
      MLE     = ifelse(e$supports_mle,  "Yes", "-"),
      MOM     = ifelse(e$supports_mom,  "Yes", "-"),
      LMOM    = ifelse(e$supports_lmom, "Yes", "-"),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  if (!is.null(method)) {
    method <- toupper(method)
    if (!method %in% c("MLE", "MOM", "LMOM"))
      stop("'method' must be one of \"MLE\", \"MOM\", or \"LMOM\".", call. = FALSE)
    df <- df[df[[method]] == "Yes", , drop = FALSE]
    rownames(df) <- NULL
  }
  df
}


# -- Internal helpers ----------------------------------------------------------

#' Retrieve a distribution entry by key, with error on unknown key
#' @param dist_key Character key (e.g. "normal", "gamma").
#' @return List of distribution metadata.
#' @keywords internal
get_dist_entry <- function(dist_key) {
  e <- FITVERSE_DIST_REGISTRY[[dist_key]]
  if (is.null(e))
    stop(sprintf(
      "Unknown distribution key '%s'.\nCall fitverse_distributions() to see all valid keys.",
      dist_key), call. = FALSE)
  e
}


#' Filter distribution keys to those compatible with the observed data
#'
#' Distributions whose support is violated (e.g. positive-only distribution
#' applied to data with negatives) are silently excluded.
#'
#' @param x    Numeric vector of finite data values.
#' @param keys Character vector of distribution keys to check.
#' @return Filtered character vector.
#' @keywords internal
filter_by_support <- function(x, keys) {
  keys[vapply(keys, function(k) {
    support <- FITVERSE_DIST_REGISTRY[[k]]$support
    if (support == "positive"  && any(x <= 0)) return(FALSE)
    if (support == "0to1"      && (any(x <= 0) || any(x >= 1))) return(FALSE)
    if (support == "threshold" && length(unique(x)) < 3) return(FALSE)
    TRUE
  }, logical(1))]
}

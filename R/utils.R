#' Calculates an empirical p-value based on the data
#'
#' This function takes a vector of statistics and calculates
#' the empirical p-value, that is, how many fall on the other
#' side of zero.  It calculates a two-tailed p-value.
#'
#' @param x a data vector to operate on
#' @param na.rm Logical whether to remove NA values. Defaults to \code{TRUE}
#' @return a named vector with the number of values falling at
#'   or below zero, above zero, and the empirical p-value.
#' @author Joshua F. Wiley <josh@@elkhartgroup.com>
#' @export
#' @keywords utilities
#' @examples
#'
#' empirical_pvalue(rnorm(100))
empirical_pvalue <- function(x, na.rm = TRUE) {
  x <- as.integer(x <= 0)
  tmp <- table(factor(x, levels = 1:0, labels = c("<= 0", "> 0")))
  m <- mean(x, na.rm = na.rm)
  pval2 <- 2 * min(m, 1 - m)
  out <- c(as.vector(tmp), pval2)
  names(out) <- c(names(tmp), "p-value")

  out
}

#' Change directory
#'
#' The function takes a path and changes the current working directory
#' to the path. If the directory specified in the path does not
#' currently exist, it will be created.
#'
#' The function has been designed to be platform independent,
#' although it has had limited testing. Path creation is done using
#' \code{file.path}, the existence of the directory is checked using
#' \code{file.exists} and the directory created with \code{dir.create}.
#' Only the first argument, is required.  The other optional arguments
#' are handy when one wants to create many similar directories with a common base.
#'
#' @param base a character string with the base path to the directory. This is required.
#' @param pre an optional character string with the prefix to add to
#'   the base path. Non character strings will be coerced to character class.
#' @param num an optional character string, prefixed by \code{pre}.
#'   Non character strings will be coerced to character class.
#' @return NULL, changes the current working directory
#' @keywords utilities
#' @export
#' @examples
#' \dontrun{
#' # an example just using the base
#' cd("~/testdir")
#'
#' # an example using the optional arguments
#' base <- "~/testdir"
#' pre <- "test_"
#'
#' cd(base, pre, 1)
#' cd(base, pre, 2)
#' }
cd <- function(base, pre, num) {
  stopifnot(is.character(base))
  if (!missing(pre) & !missing(num)) {
    pre <- as.character(pre)
    num <- as.character(num)
    newdir <- file.path(base, paste0(pre, num))
  } else {
    newdir <- file.path(base)
  }
  if (!file.exists(newdir)) {
    dir.create(newdir)
  }
  setwd(newdir)
  return(invisible(NULL))
}

#' Convert a correlation matrix and standard deviations to a covariance matrix
#'
#' This is a simple function designed to convert a correlation matrix
#' (standardized covariance matrix) back to a covariance matrix.
#' It is the opposite of \code{cov2cor}.
#'
#' @param V an n x n correlation matrix.  Should be numeric, square, and symmetric.
#' @param sigma an n length vector of the standard deviations. The length of the
#'   vector must match the number of columns in the correlation matrix.
#' @return an n x n covariance matrix
#' @seealso \code{\link{cov2cor}}
#' @export
#' @examples
#' # using a built in dataset
#' cor2cov(cor(longley), sapply(longley, sd))
#'
#' # should match the above covariance matarix
#' cov(longley)
#' all.equal(cov(longley), cor2cov(cor(longley), sapply(longley, sd)))
cor2cov <- function(V, sigma) {
  p <- (d <- dim(V))[1L]
  if (!is.numeric(V) || length(d) != 2L || p != d[2L])
      stop("'V' is not a square numeric matrix")
  if (length(sigma) != p)
      stop("'sigma' is not a vector comformable as the standard deviations of 'V'")
  if (any(diag(V) != 1))
      warning("diag(.) contained non 1 entries.  Did you pass a correlation matrix?")
  sigma * V * rep(sigma, each = p)
}

#' Return a non-missing correlation matrix
#'
#' Given a square, symmetric matrix (such as a correlation matrix)
#' this function tries to drop the fewest possible number of variables
#' to return a (square, symmetric) matrix with no missing cells.
#'
#' The assumption that x is square and symmetric comes because it is
#' assumed that the number of missing cells for a given column are identical
#' to that of the corresponding row.  \code{corOK} finds the column with the
#' most missing values, and drops that (and its corresponding row), and continues
#' on in like manner until the matrix has no missing values.  Although this was
#' intended for a correlation matrix, it could be used on other types of matrices.
#' Note that because \code{corOK} uses an iterative method, it can be slow when many
#' columns/rows need to be removed. For the intended use (correlation matrices) there
#' probably should not be many missing.  As a sanity check and to prevent tediously long
#' computations, the maximum number of iterations can be set.
#'
#' @param x a square, symmetric matrix or object coercable to such (such as a data frame).
#' @param maxiter a number indicating the maximum number of iterations,
#'   currently as a sanity check. See details.
#' @return A list with two elements
#'   \item{x}{The complete non missing matrix.}
#'   \item{keep.indices}{A vector of the columns and rows from the
#'     original matrix to be kept (i.e., that are nonmissing).}
#' @keywords utils
#' @export
#' @examples
#' cormat <- cor(iris[, -5])
#' # set missing
#' cormat[cbind(c(1,2), c(2,1))] <- NA
#'
#' # print
#' cormat
#'
#' # return complete
#' corOK(cormat)
#'
#' # using maximum iterations
#' corOK(cormat, maxiter=0)
#'
#' # clean up
#' rm(cormat)
corOK <- function(x, maxiter = 100) {
  n <- as.character(1:ncol(x))
  i <- 0L
  keep <- 1:ncol(x)

  lmat <- is.na(x)

  while (any(lmat[keep, keep]) && i < maxiter) {
    nmiss <- colSums(lmat[keep, keep])
    index <- which.max(nmiss)
    keep <- keep[!keep %in% as.integer(n[keep][index])]
    i <- i + 1L
  }

  if (i == maxiter) {
    warning("Maximum iterations exceeded.\n",
      "Currently kept indices will be returned.\n",
      "Try increasing maxiter or check why so many correlations are missing.")
    return(keep)
  }

  list(x = x[keep, keep], keep.indices = keep)
}


#' Coerces vectors to missing
#'
#' Given a vector, convert it to missing (NA) values,
#' where the class of the missing matches the input class.
#' Currently supports character, logical, integer, factor, numeric,
#' times (from \pkg{chron}), Date, POSIXct, POSIXlt, and
#' zoo (from \pkg{zoo}).
#'
#' @param x A vector to convert to missing (NA)
#' @return a vector the same length as the input with missing values of the same class
#' @keywords utils
#' @export
#' @examples
#' str(as.na(1L:5L))
#' str(as.na(rnorm(5)))
#' str(as.na(c(TRUE, FALSE)))
#' str(as.na(as.Date("2017-01-01")))
as.na <- function(x) {
  n <- length(x)
  if (inherits(x, "character")) {
    use <- NA_character_
  } else if (inherits(x, "logical")) {
    use <- NA
  } else if (inherits(x, "integer")) {
    use <- NA_integer_
  } else if (inherits(x, "factor")) {
    use <- factor(NA, levels = levels(x))
  } else if (inherits(x, "numeric")) {
    use <- NA_real_
  } else if (inherits(x, "times")) { ## from chron package
    use <- structure(rep(NA, n), format = "h:m:s", class = "times")
    return(use) ## force return as rep() does not play nicely with chron times
  } else if (inherits(x, "Date")) {
    use <- structure(NA_real_, class = "Date")
  } else if (inherits(x, "POSIXct")) {
    use <- structure(NA_real_, class = c("POSIXct", "POSIXt"))
  } else if (inherits(x, "POSIXlt")) {
    use <- structure(list(sec = NA_real_, min = NA_integer_, hour = NA_integer_,
                          mday = NA_integer_, mon = NA_integer_, year = NA_integer_,
                          wday = NA_integer_, yday = NA_integer_, isdst = -1L, zone = "",
                          gmtoff = NA_integer_), .Names = c("sec", "min", "hour", "mday",
                            "mon", "year", "wday", "yday", "isdst", "zone", "gmtoff"),
                     class = c("POSIXlt", "POSIXt"),
                     tzone = c("", "AEST", "AEDT"))
  } else if (inherits(x, "zoo")) { ## from zoo package
    use <- structure(rep(NA, n), index = rep(NA_integer_, n), class = "zoo")
    return(use) ## force return as rep() does not play nicely with zoo
  } else {
    stop(sprintf("Unknown class of type %s", class(x)))
  }
  rep(use, n)
}


#' Create a lagged variable
#'
#' Given a variable, create a k lagged version,
#' optionally do it by a grouping factor, such as an ID.
#'
#' @param x the variable to lag
#' @param k the length to lag it
#' @param by a variable to lag by. Must be sorted.
#' @return a vector of the lagged values
#' @keywords utils
#' @export
#' @examples
#' lagk(1:4, 1)
lagk <- function(x, k = 1, by) {
  m <- missing(by)
  pad <- rep(NA, k)
  if (missing(by)) {
    n <- length(x)
    if (k >= n) {
      out <- rep(NA, n)
    } else {
      out <- c(pad, x[-((n - k + 1):n)])
    }
  } else {
    if (!is.factor(by)) {
      by <- factor(by, levels = unique(by))
    }
    stopifnot(!is.unsorted(by))
    out <- unlist(tapply(x, by, function(xs) {
      n <- length(xs)
      if (k >= n) {
        tmpout <- rep(NA, n)
      } else {
        tmpout <- c(pad, xs[-((n - k + 1):n)])
      }
      return(tmpout)
    }))
  }
  return(out)
}


#' Shift a time variable to have a new center (zero point)
#'
#' Given a vector, shift the values to have a new center, but keeping the same
#' minimum and maximum.  Designed to work with time values where
#' the minimum indicates the same time as the maximum (e.g.,
#' 24:00:00 is the same as 00:00:00).
#'
#' @param x the time scores to shift
#' @param center A value (between the minimum and maximum) to center
#'   the time scores. Defaults to 0, which has no effect.
#' @param min The theoretical minimum of the time scores.
#'   Defaults to 0.
#' @param max the theoretical maximum of the time scores.
#'   Defaults to 1.
#' @param inverse A logical value, whether to \sQuote{unshift}
#'   the time scores.  Defaults to \code{FALSE}.
#' @return A vector of shifted time scores, recentered as specified.
#' @export
#' @examples
#' ## example showing centering at 11am (i.e., 11am becomes new 0)
#' plot((1:24)/24, timeshift((1:24)/24, 11/24))
#'
#' ## example showing the inverse, note that 24/24 becomes 0
#' plot((1:24)/24, timeshift(timeshift((1:24)/24, 11/24), 11/24, inverse = TRUE))
timeshift <- function(x, center = 0, min = 0, max = 1, inverse = FALSE) {
  stopifnot(center >= min && center <= max)
  stopifnot(all(x >= min))
  stopifnot(all(x <= max))

  if (isTRUE(inverse)) { ## need to fix this
    ifelse(x >= (max - center),
           (x + center) - max,
           x + center)
  } else if (identical(inverse, FALSE)) {
    ifelse(x < center,
           (max - (center - x)),
           x - center)
  } else {
    stop("invalid inverse option, must be TRUE/FALSE")
  }
}


#' Update R and install my core package set
#'
#' @param x A character vector of any additional packages to be installed
#' @param repo The repository to be used. Defaults to getOption("repos")
#' @return NULL, called for its side effect.
#' @keywords utils
#' @importFrom utils install.packages installed.packages update.packages
#' @importFrom devtools install_github
#' @export
#' @examples
#' # updateInstall()
updateInstall <- function(x, repo) {
  repos <- getOption("repos")

  if (!missing(repo)) {
    repos["CRAN"] <- repo
  } else if (!"CRAN" %in% names(repos)) {
    repos["CRAN"] <- "https://cloud.r-project.org/"
  } else if (!nzchar(repos[["CRAN"]])) {
    repos["CRAN"] <- "https://cloud.r-project.org/"
  }

  ## first update existing packages
  update.packages(repos = repos, ask = FALSE)

  packages <- list(
    ## various utilities (profiling, testing, connected to internet)
    utils = c("RCurl", "httr", "httpuv", "sendmailR",
              "rJava", "gsubfn", "rbenchmark", "digest",
              "evaluate", "profr", "proftools", "devtools", "testthat", "roxygen2",
              "codetools", "pkgmaker", "RUnit", "checkpoint", "tidyr"),

    ## packages related to data management
    data = c("plyr", "dplyr", "magrittr", "reshape", "reshape2", "stringr", "data.table", "doBy"),

    ## graphics helper packages
    gHelpers = c("RColorBrewer", "munsell", "dichromat", "memoise", "scales"),

    ## a few packages related to programming, Rcpp and inline most useful
    program = c("inline", "Rcpp", "RcppEigen", "RcppArmadillo", "RcppParallel",
                "BH", "bitops", "proto", "SoDA", "opencpu"),

    ## high performance computing
    hpc = c("iterators", "foreach", "doParallel", "rngtools", "doRNG",
            "doSNOW", # "doMC", ## not available for Windows
            "batch", "BatchJobs", "harvestr", "biglm", "biglars", "speedglm"),

    ## power
    power = c("pwr", "TrialSize", "powerSurvEpi"),

    ## various optimizers
    optim = c("minqa", "optimx", "BB"),

    ## connect to other data formats, or work with data
    db = c("RODBC", "DBI", "RSQLite", "rjson", "jsonlite",
           "sqldf", "XML", "xlsx", "ff"),

    ## packages for creating reports or nice formats
    ## (knitr is my current favorite)
    output = c("R2HTML", "hwriter", "brew", "Hmisc", "xtable",
               "formatR", "knitr", "rmarkdown", "pander", "texreg", "stargazer"),

    ## packages for robust analyses (robustbase is key)
    robust = c("robustbase", "robust", "sandwich", "quantreg"),

    ## graphics related packages
    graphics = c("ggplot2", "rgl", "scatterplot3d", "vcd",
                 "gridExtra", "gridSVG", "gtable", "cowplot", "latticeExtra", "vcdExtra",
                 "ape", "plotrix", "gplots",
                 "mcmcplots", "GGally", "HistogramTools", "ggRandomForests"),

    ## modeling packages (mixed models, additives, misc)
    modeling = c("lme4", "pedigreemm", "pedigree", "gee", "gam", "gamm4", "VGAM",
                 "mlogit", "pscl", "coxme", "arm", "bbmle", "truncreg", "JM", "PTE", "msm", "poLCA"),
    ## bayesian modeling
    bayesian = c("bayesm", "bayesSurv", "MCMCglmm", "MCMCpack", "mcmc",
                 "coda", "rjags", "R2WinBUGS", "BRugs", "R2jags", "boa", "emdbook",
                 "rstan"),
    ## model helper packages (come convenient tests)
    model.helpers = c("car", "multcomp", "lmtest", "effects", "rms", "aod",
                      "kinship2", "mlmRev", "numDeriv", "MplusAutomation", "pROC",
                      "pbkrtest", "lmerTest", "lsmeans", "multcompView"),

    ## packages for dealing with missing data
    missing = c("mice", "mi", "mitools", "Amelia"),

    ## a few survey packages
    survey = c("survey", "sampling"),

    ## tools for meta analysis
    metaanal = c("rmeta", "meta", "metasens", "coin", "metafor"),


    ## packages for sem or related
    psychometrics = c("psych", "GPArotation", "psy", "Lambda4", "systemfit",
                      "sem", "lavaan", "lavaan.survey",
                      "OpenMx", "ctsem", "metaSEM",
                      "sirt", "mirt",
                      "semTools", "ifaTools", "simsem"),


    ## packages for econometic analyeses and working with time series
    time.econ = c("zoo", "xts", "chron", "lubridate", "timeSeries",
                  "forecast", "quantmod", "portfolio", "stockPortfolio"),

    ## teaching and sos for finding functions and help
    teaching = c("sos", "TeachingDemos", "CCA", "moments"),

    ## machine learning packages
    machine = c("caret", "gbm", "ipred", "rpart", "partykit", "C50", "kernlab",
                "AppliedPredictiveModeling", "nnet",
                "lasso2", "lars", "tree", "randomForest",
                "missForest", "randomForestSRC", "e1071", "vegan",
                "cluster", "EMCluster", "bartMachine",
                "RSNNS", "deepnet", "darch", "h2o"),

    ## text mining
    text = c("tm", "wordcloud", "tm.plugin.webmining", "topicmodels",
             "SnowballC", "qdap", "qdapDictionaries"),

    ## fun packages
    fun = c("sudoku", "fortunes"))

  if (.Platform$OS.type == "unix") {
    packages$utils <- c(packages$utils, "RAppArmor")
  }

  if (!missing(x)) {
    packages$UserSpecified <- x
  }

  ## get unique packages, prevents duplication
  packages <- unique(unlist(packages))

  ## only install packages that are missing
  missing <- which(! packages %in% installed.packages()[, 1])
  packages <- packages[missing]

  results <- install.packages(packages, repos = repos,
    quiet=TRUE, dependencies = c("Depends", "Imports"))

  ## which still missing
  missing <- which(! packages %in% installed.packages()[, 1])

  if (length(missing) > 0) {
    message("Some packages did not successully install. ",
            "Trying to compile from source.")
    ## try installing from source
    install.packages(packages[missing], repos = repos,
      dependencies=TRUE, type="source", , quiet=TRUE)
  }

  if (length(missing) > 0) {
    message("Some packages did not successully install. ",
            "Trying to compile from source from alternate repo.")
    ## try installing from source
    install.packages(packages[missing], repos = repos,
      dependencies=TRUE, type="source", , quiet=TRUE)
  }

  if (length(missing) > 0) {
    missing <- which(! packages %in% installed.packages()[, 1])
    message("The following packages were not successfully installed: \n",
      paste(packages[missing], collapse = "\n"))
  }

  ## Bioconductor hdf5 reader package
  biocLite <- function(x) x; rm(biocLite) ## make Rcmd check happy
  source("https://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
  biocLite("Rgraphviz")

  update.packages(repos = repos, ask = FALSE)

  ## install_github("michaelhallquist/MplusAutomation")
  install_github("jwiley/postMCMCglmm")
  ## install_github("jwiley/JWileymisc")
  ## install_github("jwiley/score-project/pscore")
  ## install_github("ElkhartGroup/varian")
  install_github("ramnathv/rCharts")

  return(NULL)
}

## clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("DV", "Predicted"))

#' Calculates the R2 from lmer models
#'
#' For pseudo R2 by cluster, the squared correlation between observed
#' and predicted values for each cluster unit is returned.  For the overall model,
#' the marginal and conditional R2 are calculated as described in the references.
#'
#' @param model A model estimated by \code{lmer}.
#' @param modelsum The saved model summary (i.e., \code{summary(model)}).
#' @param cluster A logical whether to calculate individual pseudo R2 values by
#'   cluster unit (if \code{TRUE}) or the marginal and conditional R2 for the
#'   overall model (if \code{FALSE}, the default).
#' @return a named vector with the marginal and conditional R2 values,
#'   if \code{CLUSTER = FALSE}, otherwise, a data table with the pseudo R2
#'   for each cluster unit.
#' @references For estimating the marginal and conditional R-squared values,
#'   see: Nakagawa, S. and Schielzeth, H. (2013). A general and simple method
#'   for obtaining R2 from generalized linear mixed-effects models.
#'   Methods in Ecology and Evolution, 4(2), 133-142. as well as:
#'   Johnson, P. C. (2014). Extension of Nakagawa & Schielzeth's R2GLMM to
#'   random slopes models. Methods in Ecology and Evolution, 5(9), 944-946.
#' @keywords utils
#' @export
#' @importFrom stats model.matrix model.frame cor var
#' @importFrom stats model.frame
#' @importFrom nlme VarCorr fixef
#' @examples
#'
#' \dontrun{
#' data(aces_daily)
#' m1 <- lme4::lmer(NegAff ~ STRESS + (1 + STRESS | UserID),
#'   data = aces_daily)
#'
#' R2LMER(m1, summary(m2))
#'
#' rm(m1)
#' }
R2LMER <- function(model, modelsum, cluster = FALSE) {
  idvars <- names(modelsum$ngrps)

  if (!isTRUE(cluster)) {
  X <- model.matrix(model)
  n <- nrow(X)
  var.fe <- var(as.vector(X %*% fixef(model))) * (n - 1) / n
  var.re <- sum(sapply(VarCorr(model)[idvars], function(Sigma) {
    xvar <- rownames(Sigma)
    xvar <- sapply(xvar, function(v) colnames(X)[colnames(X) %flipIn% v])
    Z <- X[, xvar, drop = FALSE]
    sum(diag(crossprod(Z %*% Sigma, Z))) / n
  }))
  var.e <- modelsum$sigma^2
  var.total <- var.fe + var.re + var.e
  c("MarginalR2" = var.fe / var.total,
    "ConditionalR2" = (var.fe + var.re) / var.total)
  } else {
    tmpd <- cbind(data.table(
      DV = model.frame(model)[, 1],
      Predicted = fitted(model)),
      as.data.table(
        model.frame(model)[, idvars, drop = FALSE]))

    do.call(rbind, lapply(idvars, function(n) {
      out <- tmpd[, .(
        IDVariable = n,
        R2 = cor(DV, Predicted)^2), by = get(n)]
      setnames(out, old = "get", new = "ID")
      return(out)
    }))
  }
}

#' Compare two lmer models
#'
#' This function provides fit statistics and effect sizes for
#' model comparisons.  The models must be nested.
#'
#' @param m1 A model estimated by \code{lmer}.
#' @param m2 A model estimated by \code{lmer}.
#' @return a data table with the fit indices for each model
#' and comparing models to each other.
#' @references For estimating the marginal and conditional R-squared values,
#'   see: Nakagawa, S. and Schielzeth, H. (2013). A general and simple method
#'   for obtaining R2 from generalized linear mixed-effects models.
#'   Methods in Ecology and Evolution, 4(2), 133-142. as well as:
#'   Johnson, P. C. (2014). Extension of Nakagawa & Schielzeth's R2GLMM to
#'   random slopes models. Methods in Ecology and Evolution, 5(9), 944-946.
#' @keywords utils
#' @export
#' @importFrom Matrix summary
#' @importFrom stats AIC BIC logLik anova
#' @examples
#'
#' \dontrun{
#' data(aces_daily)
#' m1 <- lme4::lmer(NegAff ~ STRESS + (1 + STRESS | UserID),
#'   data = aces_daily)
#' m2 <- lme4::lmer(NegAff ~ STRESS + (1 | UserID),
#'   data = aces_daily)
#'
#' compareLMER(m1, m2)
#'
#' rm(m1, m2)
#' }
compareLMER <- function(m1, m2) {
  m1sum <- summary(m1)
  m2sum <- summary(m2)
  df1 <- attr(m1sum$logLik, "df")
  df2 <- attr(m2sum$logLik, "df")

  if (identical(df1, df2)) {
    stop("One model must be nested within the other")
  } else if (df1 < df2) {
    ## do nothing
  } else if (df1 > df2) {
    df3 <- df1
    m3 <- m1
    m3sum <- m1sum

    m1 <- m2
    m1sum <- m2sum
    df1 <- df2

    m2 <- m3
    df2 <- df3
    m2sum <- m3sum

    rm(df3, m3, m3sum)
  }

  test <- anova(m1, m2, test = "LRT")
  R21 <- R2LMER(m1, m1sum)
  R22 <- R2LMER(m2, m2sum)

  data.table(
    Model = c("Model 1", "Model 2", "Difference"),
    AIC = c(AIC(m1), AIC(m2), AIC(m2) - AIC(m1)),
    BIC = c(BIC(m1), BIC(m2), BIC(m2) - BIC(m1)),
    DF = c(df1, df2, df2 - df1),
    logLik = c(logLik(m1), logLik(m2), logLik(m2) - logLik(m1)),
    MarginalR2 = c(
      R21[["MarginalR2"]], R22[["MarginalR2"]],
      R22[["MarginalR2"]] - R21[["MarginalR2"]]),
    MarginalF2 = c(
      R21[["MarginalR2"]] / (1 - R21[["MarginalR2"]]),
      R22[["MarginalR2"]] / (1 - R22[["MarginalR2"]]),
      (R22[["MarginalR2"]] - R21[["MarginalR2"]]) /
      (1 - R22[["MarginalR2"]])),
    ConditionalR2 = c(
      R21[["ConditionalR2"]], R22[["ConditionalR2"]],
      R22[["ConditionalR2"]] - R21[["ConditionalR2"]]),
    ConditionalF2 = c(
      R21[["ConditionalR2"]] / (1 - R21[["ConditionalR2"]]),
      R22[["ConditionalR2"]] / (1 - R22[["ConditionalR2"]]),
      (R22[["ConditionalR2"]] - R21[["ConditionalR2"]]) /
      (1 - R22[["ConditionalR2"]])),
    Chi2 = c(NA_real_, NA_real_, test[, "Chisq"][2]),
    P = c(NA_real_, NA_real_, test[, "Pr(>Chisq)"][2]))
}


## clear R CMD CHECK notes
if(getRversion() >= "2.15.1") utils::globalVariables(c("var1", "var2", "sdcor", "Type",
                                                       "FE", "RE", "Terms", "Formula"))

#' drop1 for both fixed and random effects
#'
#' This function extends the current \code{drop1} method for
#' \code{merMod} class objects from the lme4 package. Where
#' the default method to be able to drop both fixed and random
#' effects at once.
#'
#' At the moment, the function is aimed to \code{lmer} models
#' and has very few features for \code{glmer} or \code{nlmer}
#' models. The primary motivation was to provide a way to
#' provide an overall test of whether a variable
#' \dQuote{matters}.  In multilevel data, a variable may be
#' included in both the fixed and random effects. To provide
#' an overall test of whether it matters requires jointly testing
#' the fixed and random effects. This also is needed to provide
#' an overall effect size.
#'
#' The function works by generating a formula with one specific
#' variable or \dQuote{term} removed at all levels. A model is then
#' fit on this reduced formula and compared to the full model passed
#' in. This is a complex operation for mixed effects models for several
#' reasons. Firstly, \code{R} has no default mechanism for dropping
#' terms from both the fixed and random portions. Secondly,
#' mixed effects models do not accomodate all types of models. For example,
#' if a model includes only a random slope with no random intercept,
#' if the random slope was dropped, there would be no more random effects,
#' and at that point, \code{lmer} or \code{glmer} will not run the model.
#' It is theoretically possible to instead fit the model using
#' \code{lm} or \code{glm} but this becomes more complex for certain
#' model comparisons and calculations and is not currently implemented.
#' Marginal and conditional R2 values are calculated for each term,
#' and these are used also to calculate something akin to an
#' f-squared effect size.
#'
#' This is a new function and it is important to carefully evaluate
#' the results and check that they are accurate and that they are
#' sensible. Check accuracy by viewing the model formulae for each
#' reduced model and checking that those are indeed accurate.
#' In terms of checking whether a result is sensible or not,
#' there is a large literature on the difficulty interpretting
#' main effect tests in the presence of interactions. As it is
#' challenging to detect all interactions, especially ones that are
#' made outside of \code{R} formulae, all terms are tested. However,
#' it likely does not make sense to report results from dropping a
#' main effect but keeping the interaction term, so present
#' and interpret these with caution.
#'
#' @param obj A \code{merMod} class object, the fitted result of
#'   \code{lmer}.
#' @param method A character vector indicating the types of confidence
#'   intervals to calculate. One of \dQuote{Wald}, \dQuote{profile}, or
#'   \dQuote{boot}.
#' @param \ldots Additional arguments passed to \code{confint}
#' @importFrom lme4 isGLMM isNLMM isLMM isREML nobars findbars
#' @importFrom stats family formula nobs update
#' @importFrom lmerTest lsmeansLT
#' @export
#' @examples
#'
#' \dontrun{
#' data(aces_daily)
#' m1 <- lme4::lmer(NegAff ~ STRESS + (1 + STRESS | UserID),
#'   data = aces_daily)
#' m2 <- lme4::lmer(NegAff ~ STRESS + I(STRESS^2) + (1 + STRESS | UserID),
#'   data = aces_daily)
#' testm1 <- detailedTests(m1, method = "profile")
#' testm2 <- detailedTests(m2, method = "profile")
#' testm2b <- detailedTests(m2, method = "boot", nsim = 100)
#' }
detailedTests <- function(obj, method = c("Wald", "profile", "boot"), ...) {
  if (isGLMM(obj) || isNLMM(obj)) {
    stop("GLMMs and NLMMs are not currently supported")
  }
  if (!isLMM(obj)) {
    stop("Only LMMs fit with lmer() are currently supported")
  }
  method <- match.arg(method)

  cis <- confint(obj, method = method, oldNames = FALSE, ...)
  cis2 <- data.table(
    Term = rownames(cis),
    LL = cis[,1],
    UL = cis[,2])

  res <- as.data.table(as.data.frame(VarCorr(obj)))
  res[, Term := ifelse(grp == "Residual",
                       "sigma",
                ifelse(
                  is.na(var2),
                  sprintf("sd_%s|%s", var1, grp),
                  sprintf("cor_%s.%s|%s", var2, var1, grp)))]
  res <- res[, .(Term = Term, Est = sdcor)]

  fes <- data.table(
    Term = names(fixef(obj)),
    Est = as.numeric(fixef(obj)))

  all <- merge(
    rbind(
      cbind(res, Type = "RE"),
      cbind(fes, Type = "FE")),
    cis2,
    by = "Term", all = TRUE)

  out.res <- all[Type == "RE"]
  out.fes <- all[Type == "FE"]

  objsum <- summary(obj)

  if ("Pr(>|t|)" %in% colnames(objsum$coefficients)) {
    fe.p <- data.table(
      Term = rownames(objsum$coefficients),
      Pval = objsum$coefficients[, "Pr(>|t|)"])
  } else {
    fe.p <- data.table(
      Term = rownames(objsum$coefficients),
      Pval = (1 - pnorm(abs(objsum$coefficients[, "t value"]))) * 2)
  }

  out.fes <- merge(out.fes, fe.p, by = "Term", all = TRUE)

  ## check if linear mixed model is fit with REML
  ## and if so refit it with ML
  if (isLMM(obj) && isREML(obj)) {
    message(paste0(
      "Parameters and CIs are based on REML, \n",
      "but detailedTests requires ML not REML fit for comparisons, \n",
      "and these are used in effect sizes. Refitting."))
  }
  obj <- update(obj, data = model.frame(obj), REML = FALSE)

  ngrps <- ngrps(obj)
  out.misc <- data.table(
    Type = ifelse(isREML(obj), "REML", "ML"),
    AIC = AIC(obj),
    BIC = BIC(obj),
    logLik = logLik(obj),
    DF = attr(logLik(obj), "df"),
    as.data.table(t(R2LMER(obj, summary(obj)))),
    N_Obs = nobs(obj),
    as.data.table(t(ngrps)))

  setnames(out.misc,
           old = names(ngrps),
           new = paste0("N_", names(ngrps)))


  ## get formula
  f <- formula(obj)

  ## fixed effects
  fe <- nobars(f)
  fe.terms <- terms(fe)
  fe.labs <- labels(fe.terms)
  fe.intercept <- if(identical(attr(fe.terms, "intercept"), 1L)) "1" else "0"

  ## random effects
  re <- lapply(findbars(f), deparse)

  re.group <- lapply(re, function(v) {
    gsub("(^.*)\\|(.*$)", "\\2", v)
  })

  re.terms <- lapply(re, function(v) {
    v <- gsub("(^.*)(\\|.*$)", "\\1", v)
    v <- sprintf("dv ~ %s", v)
    terms(as.formula(v))
  })
  re.labs <- lapply(re.terms, labels)
  re.intercept <- lapply(re.terms, function(x) {
    if(identical(attr(x, "intercept"), 1L)) "1" else "0"
  })

  ## all terms from fixed and random effects
  all.labs <- unique(c(fe.labs, unlist(re.labs)))
  tmp <- vector("character")
  for (i in seq_along(all.labs)) {
    tmp <- c(
      tmp,
      all.labs[match(TRUE, all.labs %flipIn% all.labs[i])])
  }
  all.labs <- unique(tmp)

  labs.levels <- data.table(
    Terms = all.labs,
    FE = as.integer(vapply(all.labs,
      function(v) any(unlist(fe.labs) %flipIn% v),
      FUN.VALUE = NA)),
    RE = as.integer(vapply(all.labs,
      function(v) any(unlist(re.labs) %flipIn% v),
      FUN.VALUE = NA)))
  labs.levels[, Type := paste0(FE, RE)]
  labs.levels <- labs.levels[,
    .(Type = if(Type == "11") c("11", "01") else Type),
    by = Terms]
  labs.levels[, FE := substr(Type, 1, 1) == "1"]
  labs.levels[, RE := substr(Type, 2, 2) == "1"]

  ## formula from reduced models, dropping one term at a time
  out.f <- unlist(lapply(seq_along(labs.levels$Terms), function(i) {
    use.fe.labs <- fe.labs[!((fe.labs %flipIn% labs.levels$Terms[i]) & labs.levels$FE[i])]
    use.re.labs <- lapply(re.labs, function(x) {
      if (length(x)) {
        x[!((x %flipIn%  labs.levels$Terms[i]) & labs.levels$RE[i])]
      } else {
        x
      }
    })

    fe.built <- sprintf("%s ~ %s%s%s",
                        as.character(f)[2],
                        fe.intercept,
                        if (length(use.fe.labs)) " + " else "",
                        paste(use.fe.labs, collapse = " + "))

    re.built <- lapply(seq_along(re), function(i) {
      if (re.intercept[[i]] == "0" && !length(use.re.labs[[i]])) {
        vector("character", 0L)
      } else {
        sprintf("(%s%s%s |%s)",
                re.intercept[[i]],
                if (length(use.re.labs[[i]])) " + " else "",
                paste(use.re.labs[[i]], collapse = " + "),
                re.group[[i]])
      }
    })

    re.built <- paste(unlist(re.built), collapse = " + ")

    if (nzchar(re.built)) {
      all.built <- paste(c(fe.built, re.built), collapse = " + ")
    } else {
      all.built <- NA_character_
    }
    return(all.built)
  }))

  labs.levels[, Formula := out.f]

  testm <- lapply(out.f, function(f) {
    if (!is.na(f)) {
      if (isLMM(obj)) {
        lmer(as.formula(f), data = model.frame(obj), REML = FALSE)
      } else if (isGLMM(obj)) {
        glmer(as.formula(f), data = model.frame(obj),
              family = family(obj))
      }
    } else {
      NA
    }
  })

  out.tests <- do.call(rbind, lapply(seq_along(testm), function(i) {
    objreduced <- testm[[i]]
    v <- labs.levels$Terms[[i]]

    if (!isTRUE(inherits(objreduced, "merMod"))) {
      tmp <- data.table(
        Variable = v,
        AIC = NA_real_,
        BIC = NA_real_,
        DF = NA_integer_,
        logLik = NA_real_,
        MarginalR2 = NA_real_,
        MarginalF2 = NA_real_,
        ConditionalR2 = NA_real_,
        ConditionalF2 = NA_real_,
        Chi2 = NA_real_,
        P = NA_real_)
    } else {
      tmp <- compareLMER(obj, objreduced)[3]
      setnames(tmp, old = "Model", new = "Variable")
      tmp$Variable <- v
    }
    return(tmp)
  }))

  out.tests <- cbind(out.tests, labs.levels[, -(1:2)])
  out.tests[, Type := factor(paste0(FE, RE),
                             levels = c("FALSETRUE", "TRUEFALSE", "TRUETRUE"),
                             labels = c("Random", "Fixed", "Fixed + Random"))]

  list(
    FixedEffects = out.fes,
    RandomEffects = out.res,
    EffectSizes = out.tests,
    OverallModel = out.misc)
}


#' @name logicals
#' @rdname logicals
#'
#' @title Several logical range comparison helpers
#'
#' @param e1 A number of vector to be evaluated
#' @param e2 A vector of one or two numbers used to denote the
#'   limits for logical comparison.
#'
#' @return A logical vector of the same length as \code{e1} or for
#'  those functions prefaced with \dQuote{s} the subsetted vector.
NULL

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gele% c(2, 4)
#' 1:5 %gele% c(4, 2) # order does not matter uses min / max
`%gele%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 >= min(e2) & e1 <= max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gel% c(2, 4)
#' 1:5 %gel% c(4, 2) # order does not matter uses min / max
`%gel%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 >= min(e2) & e1 < max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gle% c(2, 4)
#' 1:5 %gle% c(4, 2) # order does not matter uses min / max
`%gle%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 > min(e2) & e1 <= max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %gl% c(2, 4)
#' 1:5 %gl% c(4, 2) # order does not matter uses min / max
`%gl%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1 > min(e2) & e1 < max(e2)
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sgele% c(2, 4)
#' 1:5 %sgele% c(4, 2) # order does not matter uses min / max
`%sgele%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1[e1 >= min(e2) & e1 <= max(e2)]
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sgel% c(2, 4)
#' 1:5 %sgel% c(4, 2) # order does not matter uses min / max
`%sgel%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1[e1 >= min(e2) & e1 < max(e2)]
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sgle% c(2, 4)
#' 1:5 %sgle% c(4, 2) # order does not matter uses min / max
`%sgle%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))

  e1[e1 > min(e2) & e1 <= max(e2)]
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sgl% c(2, 4)
#' 1:5 %sgl% c(4, 2) # order does not matter uses min / max
`%sgl%` <- function(e1, e2) {
  stopifnot(identical(length(e2), 2L))
  stopifnot(!anyNA(e2))
  e1[e1 > min(e2) & e1 < max(e2)]
}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sge% 2
#' 1:5 %sge% 4
`%sge%` <- function(e1, e2) {e1[e1 >= e2]}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sg% 2
#' 1:5 %sg% 4
`%sg%` <- function(e1, e2) {e1[e1 > e2]}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sle% 2
#' 1:5 %sle% 4
`%sle%` <- function(e1, e2) {e1[e1 <= e2]}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sl% 2
#' 1:5 %sl% 4
`%sl%` <- function(e1, e2) {e1[e1 < e2]}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %nin% c(2, 99)
#' c("jack", "jill", "john", "jane") %nin% c("jill", "jane", "bill")
`%nin%` <- function(e1, e2) {!(e1 %in% e2)}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %sin% c(2, 99)
#' c("jack", "jill", "john", "jane") %sin% c("jill", "jane", "bill")
`%sin%` <- function(e1, e2) {e1[e1 %in% e2]}

#' @rdname logicals
#' @export
#' @examples
#'
#' 1:5 %snin% c(2, 99)
#' c("jack", "jill", "john", "jane") %snin% c("jill", "jane", "bill")
`%snin%` <- function(e1, e2) {e1[e1 %nin% e2]}

#' @rdname logicals
#' @export
## compare two strings where flips around
## a colon do not matter, used for interactions
`%flipIn%` <- function(e1, e2) {
  .flipMatch <- function(e1, e2) {
    e1 <- unlist(strsplit(e1, ":"))
    e2 <- unlist(strsplit(e2, ":"))
    all(e1 %in% e2) && all(e2 %in% e1)
  }
  sapply(e1, function(v) {
    .flipMatch(v, e2)
  })
}

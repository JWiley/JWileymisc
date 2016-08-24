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

#' Estimate the first and second moments
#'
#' This function relies on the \pkg{lavaan} package to use the
#' Expectation Maximization (EM) algorithm to estimate the first and
#' second moments (means and [co]variances) when there is missing data.
#'
#' @param data A data frame or an object coercable to a data frame.
#'   The means and covariances of all variables are estimated.
#' @param \dots Additional arguments passed on to the \code{estimate.moments.EM}
#'   function in \pkg{lavaan}. Note this is not an exported function.
#' @return A list containing the esimates from the EM algorithm.
#'   \item{mu}{A named vector of the means.}
#'   \item{sigma}{The covariance matrix.}
#' @seealso \code{\link{SEMSummary}}
#' @keywords multivariate
#' @importFrom lavaan lavCor lavInspect
#' @author Suggested by Yves Rosseel author of the lavaan package on which this depends
#' @export
#' @examples
#' # sample data
#' Xmiss <- as.matrix(iris[, -5])
#' # make 25% missing completely at random
#' set.seed(10)
#' Xmiss[sample(length(Xmiss), length(Xmiss) * .25)] <- NA
#' Xmiss <- as.data.frame(Xmiss)
#'
#' # true means and covariance
#' colMeans(iris[, -5])
#' # covariance with n - 1 divisor
#' cov(iris[, -5])
#'
#' # means and covariance matrix using list wise deletion
#' colMeans(na.omit(Xmiss))
#' cov(na.omit(Xmiss))
#'
#' # means and covariance matrix using EM
#' moments(Xmiss)
#' # clean up
#' rm(Xmiss)
moments <- function(data, ...) {

  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  ## from Yves
  fit <- lavCor(data, output = "fit", missing = "ml")
  out <- lavInspect(fit, "sampstat.h1")

  class(out$cov) <- "matrix"
  class(out$mean) <- "numeric"
  names(out) <- c("sigma", "mu")

  return(out)
}

#' Summary Statistics for a SEM Analysis
#'
#' This function is designed to calculate the descriptive statistics and
#' summaries that are often reported on raw data when the main analyses
#' use structural equation modelling.
#'
#' This function calculates a variety of relevant statistics on the raw
#' data used in a SEM analysis.  Because it is meant for SEM style data,
#' for now it expects all variables to be numeric.  In the future I may
#' try to expand it to handle factor variables somehow.
#'
#' Both the formula and data arguments are required.  The formula should
#' be the right hand side only.  The most common way to use it would be with
#' variable names separated by \sQuote{+s}.  For convenience, a \sQuote{.} is
#' expanded to mean \dQuote{all variables in the data set}.  For a large number
#' of variables or when whole datasets are being analyzed, this can be considerably
#' easier to write.  Also it facilitates column indexing by simply passing a subset
#' of the data (e.g., \code{data[, 1:10]}) and using the \sQuote{.} expansion to
#' analyze the first 10 columns.  The examples section demonstrate this use.
#'
#' Also noteworthy is that \code{SEMSummary} is not really meant to be used
#' on its own.  It is the computational workhorse, but it is meant to be used
#' with a styling or printing method to produce simple output.
#' \code{APAStyler} has methods for \code{SEMSummary} output.
#'
#' There are several new ways to handle missing data now
#' including listwise deletion, pairwise deletion, and using the EM
#' algorithm, the default.
#'
#' @param formula A formula of the variables to be used in the analysis.
#'   See the \sQuote{details} section for more information.
#' @param data A data frame, matrix, or list containing the variables
#'   used in the formula.  This is a required argument.
#' @param use A character vector of how to handle missing data. Defaults to \dQuote{fiml}.
#' @return A list with S3 class \dQuote{SEMSummary}
#'   \item{names}{A character vector containing the variable names.}
#'   \item{n}{An integer vector of the length of each variable used
#'     (this includes available and missing data).}
#'   \item{nmissing}{An integer vector of the number of missing values in each variable.}
#'   \item{mu}{A vector of the arithmetic means of each variable (on complete data).}
#'   \item{stdev}{A numeric vector of the standard deviations of each variable (on complete data).}
#'   \item{Sigma}{The numeric covariance matrix for all variables.}
#'   \item{sSigma}{The numeric correlation matrix for all variables.}
#'   \item{coverage}{A numeric matrix giving the percentage (technically decimal)
#'     of information available for each pairwise covariance/correlation.}
#'   \item{pvalue}{The two-sided p values for the correlation matrix. Pairwise present N
#'     used to calculate degrees of freedom.}
#' @seealso \code{\link{APAStyler}}
#' @keywords multivariate
#' @importFrom stats terms
#' @export
#' @examples
#' ## Example using the built in iris dataset
#' s <- SEMSummary(~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)
#' s # show output ... not very nice
#'
#' ## Prettier output from SEMSummary
#' APAStyler(s)
#'
#' #### Subset the dataset and use the . expansion ####
#'
#' ## summary for all variables in mtcars data set
#' ## with 11 variables, this could be a pain to write out
#' SEMSummary(~ ., data = mtcars)
#'
#' ## . expansion is also useful when we know column positions
#' ## but not necessarily names
#' SEMSummary(~ ., data = mtcars[, c(1, 2, 3, 9, 10, 11)])
#'
#' ## clean up
#' rm(s)
#'
#' #' # sample data
#' Xmiss <- as.matrix(iris[, -5])
#' # make 25% missing completely at random
#' set.seed(10)
#' Xmiss[sample(length(Xmiss), length(Xmiss) * .25)] <- NA
#' Xmiss <- as.data.frame(Xmiss)
#'
#' SEMSummary(~ ., data = Xmiss, use = "fiml")
#'
#' ## clean up
#' rm(Xmiss)
SEMSummary <- function(formula, data,
  use = c("fiml", "pairwise.complete.obs", "complete.obs")) {
  env <- environment(formula)

  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  tmp <- unlist(strsplit(paste(deparse(formula), collapse = ""), "\\|"))
  formula <- as.formula(tmp[1], env = env)

  if (length(tmp) > 1) {
    condition <- as.formula(paste0("~ ", tmp[2]), env = env)
    vars <- attr(terms(condition, data = data), "variables")
    vnames <- as.character(vars)[-1L]
    if (length(vnames) < 1) stop("You must specify at least 1 variables to condition the formula")

    grouping <- interaction(eval(vars, data, env), drop = TRUE)

    output <- by(data[, -which(colnames(data) %in% vnames)], grouping, FUN = function(d) SEMSummary.fit(formula, d, use = use))
    output$Levels <- levels(grouping)

    class(output) <- "SEMSummary.list"
  } else {
    output <- SEMSummary.fit(formula, data, use = use)
  }

  return(output)
}

#' Summary Statistics for a SEM Analysis
#'
#' This is a low level fitting function, for SEMSummary.
#'
#' @param formula A formula of the variables to be used in the analysis.
#'   See the \sQuote{details} section for more information.
#' @param data A data frame, matrix, or list containing the variables
#'   used in the formula.  This is a required argument.
#' @param use A character vector of how to handle missing data. Defaults to \dQuote{fiml}.
#' @return A list with S3 class \dQuote{SEMSummary}
#'   \item{names}{A character vector containing the variable names.}
#'   \item{n}{An integer vector of the length of each variable used
#'     (this includes available and missing data).}
#'   \item{nmissing}{An integer vector of the number of missing values in each variable.}
#'   \item{mu}{A vector of the arithmetic means of each variable (on complete data).}
#'   \item{stdev}{A numeric vector of the standard deviations of each variable (on complete data).}
#'   \item{Sigma}{The numeric covariance matrix for all variables.}
#'   \item{sSigma}{The numeric correlation matrix for all variables.}
#'   \item{coverage}{A numeric matrix giving the percentage (technically decimal)
#'     of information available for each pairwise covariance/correlation.}
#'   \item{pvalue}{The two-sided p values for the correlation matrix. Pairwise present N
#'     used to calculate degrees of freedom.}
#' @seealso \code{\link{SEMSummary}}
#' @keywords multivariate
#' @importFrom stats cov cov2cor pt
SEMSummary.fit <- function(formula, data,
  use = c("fiml", "pairwise.complete.obs", "complete.obs")) {

  use <- match.arg(use)

  vars <- attr(terms(formula, data = data), "variables")
  vnames <- as.character(vars)[-1L]
  if (length(vnames) < 2) stop("You must specify at least 2 variables to use this function")
  env <- environment(formula)

  X <- eval(vars, data, env)
  names(X) <- vnames
  X <- as.data.frame(X)

  rm(data)
  gc()

  if (all(!is.na(X)) && use == "fiml") {
    use <- "complete.obs"
  }

  res <- switch(use,
    fiml = {moments(X)},
    pairwise.complete.obs = {
      list(mu = colMeans(X, na.rm = TRUE),
        sigma = cov(X, use = "pairwise.complete.obs"))
    },
    complete.obs = {
      list(mu = colMeans(na.omit(X)),
        sigma = cov(na.omit(X)))
    }
  )

  mu <- res$mu
  Sigma <- res$sigma
  stdev <- sqrt(diag(Sigma))
  sSigma <- cov2cor(Sigma)

  n <- nrow(X)
  L <- is.na(X)
  nmiss <- colSums(L)
  i <- which(upper.tri(Sigma), arr.ind = TRUE)
  pairmiss <- apply(i, 1L, function(j) {
    sum(L[, j[1]] | L[, j[2]])
  })
  pvalue <- coverage <- matrix(NA, nrow = ncol(X), ncol = ncol(X))
  diag(coverage) <- (n - nmiss)/n
  coverage[i] <- (n - pairmiss)/n
  coverage[i[, c(2, 1)]] <- (n - pairmiss)/n
  dimnames(coverage) <- dimnames(Sigma)

  df <- (coverage * n) - 2
  statistic <- sqrt(df) * sSigma / sqrt(1 - sSigma^2)
  p <- pt(statistic, df)
  pvalue[] <- 2 * pmin(p, 1 - p)
  diag(pvalue) <- NA
  dimnames(pvalue) <- dimnames(Sigma)

  names(nmiss) <- names(mu) <- names(stdev) <- names(X)

  output <- list(names = vnames, n = n, nmissing = nmiss, mu = mu, stdev = stdev,
    Sigma = Sigma, sSigma = sSigma, coverage = coverage, pvalue = pvalue)
  class(output) <- "SEMSummary"

  return(output)
}



#' EGL Table function makes nice tables
#'
#' Give a dataset and a list of variables, or just the data
#' in the vars.  For best results, convert categorical
#' variables into factors.
#'
#' @param vars Either an index (numeric or character) of
#'   variables to access from the \code{data} argument,
#'   or the data to be described itself.
#' @param g A variable used tou group/separate the data prior
#'   to calculating descriptive statistics.
#' @param data optional argument of the dataset containing
#'   the variables to be described.
#' @param strict Logical, whether to strictly follow the
#'   type of each variable, or to assume categorical if
#'   the number of unique values is less than or equal to 3.
#' @param parametric Logical whether to use parametric tests in the
#'   case of multiple groups to test for differences.  Only applies to
#'   continuous variables. If \code{TRUE}, the default, uses one-way ANOVA,
#'   and a F test. If \code{FALSE}, uses the Kruskal-Wallis test.
#' @param simChisq Logical whether to estimate p-values for chi-square test
#'   for categorical data when there are multiple groups, by simulation.
#'   Defaults to \code{FALSE}. Useful when there are small cells as will
#'   provide a more accurate test in extreme cases, similar to Fisher Exact
#'   Test but generalizing to large dimension of tables.
#' @param sims Integer for the number of simulations to be used to estimate
#'   p-values for the chi-square tests for categorical variables when
#'   there are multiple groups.
#' @return A data frame of the table.
#' @keywords utils
#' @export
#' @import data.table
#' @importFrom stats sd aov chisq.test kruskal.test quantile xtabs
#' @examples
#' egltable(iris)
#' egltable(colnames(iris)[1:4], "Species", iris)
#' egltable(iris, parametric = FALSE)
#' egltable(colnames(iris)[1:4], "Species", iris,
#'   parametric = FALSE)
#' egltable(colnames(iris)[1:4], "Species", iris,
#'   parametric = c(TRUE, TRUE, FALSE, FALSE))
#' egltable(colnames(iris)[1:4], "Species", iris,
#'   parametric = c(TRUE, TRUE, FALSE, FALSE), simChisq=TRUE)
egltable <- function(vars, g, data, strict=TRUE, parametric = TRUE, simChisq = FALSE, sims = 1e6) {
  if (!missing(data)) {
    if (is.data.table(data)) {
    } else {
      dat <- as.data.frame(data[, vars, drop=FALSE], stringsAsFactors=FALSE)
    }
    if (!missing(g)) {
      if (length(g) == 1) {
        g <- data[[g]]
      }
    }
  } else {
    dat <- as.data.frame(vars, stringsAsFactors=FALSE)
  }

  if (missing(g)) {
    g <- rep(1, nrow(dat))
  }

  g <- as.factor(g)

  if (identical(length(parametric), 1L)) {
    if (isTRUE(parametric)) {
      parametric <- rep(TRUE, length(vars))
    } else {
      parametric <- rep(FALSE, length(vars))
    }
  }

  vnames <- colnames(dat)

  k <- ncol(dat)

  contvars.index <- unlist(lapply(dat, function(x) {
      (is.integer(x) | is.numeric(x)) &
        ((length(unique(x)) > 3) | strict)
  }))

  catvars.index <- which(!contvars.index)
  contvars.index <- which(contvars.index)

  if (length(contvars.index)) {
    if (length(unique(parametric[contvars.index])) > 1) {
    multi <- TRUE
    } else {
      multi <- FALSE
    }
  } else {
    multi <- FALSE
  }


  tmpout <- by(dat, g, function(d) {
    tmpres <- NULL
    reslab <- ""

    if (length(contvars.index)) {
      tmpcont <- lapply(contvars.index, function(v) {
        n <- vnames[v]
        if (parametric[v]) {
          ## use parametric tests
          data.frame(
            Variable = sprintf("%s%s", n, c("", ", M (SD)")[multi+1]),
            Res = sprintf("%0.2f (%0.2f)", mean(d[[n]], na.rm=TRUE), sd(d[[n]], na.rm=TRUE)),
            stringsAsFactors=FALSE)
        } else {
          data.frame(
            Variable = sprintf("%s%s", n, c("", ", Mdn (IQR)")[multi+1]),
            Res = sprintf("%0.2f (%0.2f)", median(d[[n]], na.rm=TRUE),
                          abs(diff(quantile(d[[n]], c(.25, .75), na.rm = TRUE)))),
            stringsAsFactors=FALSE)
        }
      })

      names(tmpcont) <- vnames[contvars.index]
      tmpres <- c(tmpres, tmpcont)

      reslab <- paste0(reslab, c(ifelse(parametric[contvars.index[1]], "M (SD)", "Mdn (IQR)"), "See Rows")[multi+1])
    }

    if (length(catvars.index)) {
      tmpcat <- lapply(vnames[catvars.index], function(n) {
         x <- table(d[, n])
        data.frame(
          Variable = c(n, paste0("  ", names(x))),
          Res = c("", sprintf("%d (%2.1f)", x, prop.table(x) * 100)),
          stringsAsFactors=FALSE)
      })

      names(tmpcat) <- vnames[catvars.index]
      tmpres <- c(tmpres, tmpcat)

      reslab <- paste0(reslab, ifelse(nzchar(reslab), "/N (%)", "N (%)"))
    }

    tmpres <- lapply(tmpres[vnames], function(d) {
      colnames(d) <- c("Vars", reslab)
      return(d)
      })

    return(tmpres)
  }, simplify=FALSE)


  if (length(levels(g)) > 1) {
    tmpout <- lapply(seq_along(vnames), function(v) {
      out <- do.call(cbind.data.frame, lapply(1:length(levels(g)), function(i) {
        d <- tmpout[[i]][[v]]
        colnames(d)[2] <- paste(levels(g)[i], colnames(d)[2], sep = " ")
        if (i == 1) {
          return(d)
        } else {
          return(d[, -1, drop = FALSE])
        }
      }))

      if (length(contvars.index)) {
        if (v %in% contvars.index) {
          if (parametric[v]) {
            tests <- summary(aov(dv ~ g, data = data.frame(dv = dat[[v]], g = g)))[[1]]
            out <- cbind.data.frame(out,
                             Test = c(sprintf("F(%d, %d) = %0.2f, %s", tests[1, "Df"], tests[2, "Df"], tests[1, "F value"],
                                              formatPval(tests[1, "Pr(>F)"], 3, 3)),
                                      rep("", nrow(out) - 1)),
                             stringsAsFactors = FALSE)
          } else {
            tests <- kruskal.test(dv ~ g, data = data.frame(dv = dat[[v]], g = g))
            out <- cbind.data.frame(out,
                             Test = c(sprintf("KW chi-square = %0.2f, df = %d, %s",
                                            tests$statistic, tests$parameter,
                                            formatPval(tests$p.value, 3, 3)),
                                      rep("", nrow(out) - 1)),
                             stringsAsFactors = FALSE)
          }
        }
      }

      if (length(catvars.index)) {
        if (v %in% catvars.index) {
          tests <- chisq.test(xtabs(~ dv + g, data = data.frame(dv = dat[[v]], g = g)),
                              correct = FALSE,
                              simulate.p.value = simChisq, B = sims)
          out <- cbind.data.frame(out,
                           Test = c(sprintf("Chi-square = %0.2f, %s, %s",
                                          tests$statistic,
                                          ifelse(simChisq, "simulated", sprintf("df = %d", tests$parameter)),
                                          formatPval(tests$p.value, 3, 3)),
                                    rep("", nrow(out) - 1)),
                           stringsAsFactors = FALSE)
        }
      }

      return(out)
    })
  } else {
    tmpout <- tmpout[[1]]
  }

  out <- do.call(rbind.data.frame, c(tmpout, stringsAsFactors = FALSE))
  rownames(out) <- NULL
  colnames(out)[1] <- ""

  return(out)
}



#' Winsorize at specified percentiles
#'
#' Simple function winsorizes data at the specified percentile.
#'
#' @param d A vector, matrix, or data frame to be winsorized
#' @param percentile The percentile bounded by [0, 1] to winsorize data at.
#'   If a data frame or matrix is provided for the data, this should have the
#'   same length as the number of columns, or it will be repeated for all.
#' @param values If values are specified, use these instead of calculating by percentiles.
#'   Should be a data frame with columns named \dQuote{low}, and \dQuote{high}.
#'   If a data frame or matrix is provided for the data, there should be as many rows
#'   for values to winsorize at as there are columns in the data.
#' @param na.rm A logical whether to remove NAs.
#' @return winsorized data. Attributes are included to list the exact values
#'   (for each variable, if a data frame or matrix) used to winsorize
#'   at the lower and upper ends.
#' @importFrom stats quantile
#' @export
#' @examples
#' dev.new(width = 10, height = 5)
#' par(mfrow = c(1, 2))
#' hist(as.vector(eurodist), main = "Eurodist")
#' hist(winsorizor(as.vector(eurodist), .05), main = "Eurodist with lower and upper\n5% winsorized")
winsorizor <- function(d, percentile, values, na.rm = TRUE) {
    if (!missing(percentile)) {
      stopifnot(percentile >= 0 && percentile <= 1)
    } else if (missing(percentile)) {
      percentile <- NA_real_
    }

    stopifnot(is.vector(d) || is.matrix(d) || is.data.frame(d))

    f <- function(x, percentile, values, na.rm) {
          if (!missing(values)) {
            low <- values[, "low"]
            high <- values[, "high"]
            if (missing(percentile)) {
              percentile <- NA_real_
            }
          } else {
            low <- quantile(x, probs = 0 + percentile, na.rm = na.rm)
            high <- quantile(x, probs = 1 - percentile, na.rm = na.rm)
          }

          out <- pmin(pmax(x, low), high)

          new.attr <- data.frame(low = low, high = high, percentile = percentile)
          rownames(new.attr) <- NULL

          attributes(out) <- c(attributes(x), winsorizedValues = list(new.attr))

          return(out)
    }

    if (is.vector(d)) {
        out <- f(d, percentile = percentile, values = values, na.rm = na.rm)
    } else if (is.matrix(d) || is.data.frame(d)) {
        if (length(percentile) == 1) {
          percentile <- rep(percentile, ncol(d))
        }

        if (missing(values)) {
          tmp <- lapply(1:ncol(d), function(i) {
            f(d[, i], percentile = percentile[i], na.rm = na.rm)
          })
        } else {
          tmp <- lapply(1:ncol(d), function(i) {
            f(d[, i], percentile = percentile[i], values = values[i, ], na.rm = na.rm)
          })
        }

        all.attr <- do.call(rbind, lapply(tmp, function(x) attr(x, "winsorizedValues")))
        all.attr$variable <- colnames(d)
        rownames(all.attr) <- NULL
        out <- as.data.frame(lapply(tmp, as.vector))

        if (is.matrix(d)) {
            out <- as.matrix(out)
        }

        attributes(out) <- c(attributes(d), winsorizedValues = list(all.attr))
    }

    return(out)
}

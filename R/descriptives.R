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
  fit <- lavCor(data,
                meanstructure = TRUE,
                output = "fit", missing = "ml")
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



#' Function makes nice tables
#'
#' Give a dataset and a list of variables, or just the data
#' in the vars.  For best results, convert categorical
#' variables into factors.  Provides a table of estimated descriptive
#' statistics optionally by group levels.
#'
#' @param vars Either an index (numeric or character) of
#'   variables to access from the \code{data} argument,
#'   or the data to be described itself.
#' @param g A variable used tou group/separate the data prior
#'   to calculating descriptive statistics.
#' @param idvar A character string indicating the variable name
#'   of the ID variable.  Not currently used, but will eventually
#'   support \code{egltable} supporting repeated measures data.
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
#'
#' diris <- as.data.table(iris)
#' egltable("Sepal.Length", g = "Species", data = diris)
egltable <- function(vars, g, idvar, data, strict=TRUE, parametric = TRUE, simChisq = FALSE, sims = 1e6) {
  if (!missing(data)) {
    if (is.data.table(data)) {
      dat <- data[, vars, with=FALSE]
    } else {
      dat <- as.data.table(data[, vars, drop=FALSE])
    }
    if (!missing(g)) {
      if (length(g) == 1) {
        g <- data[[g]]
      }
    }
    if (!missing(idvar)) {
      ids <- data[[idvar]]
    }
  } else {
    dat <- as.data.table(vars)
  }

  if (missing(g)) {
    g <- rep(1, nrow(dat))
  }

  g <- droplevels(as.factor(g))

  if (identical(length(parametric), 1L)) {
    if (isTRUE(parametric)) {
      parametric <- rep(TRUE, length(vars))
    } else {
      parametric <- rep(FALSE, length(vars))
    }
  }

  vnames <- names(dat)

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


  tmpout <- lapply(levels(g), function(gd) {
    d <- dat[which(g == gd)]
    tmpres <- NULL
    reslab <- ""

    if (length(contvars.index)) {
      tmpcont <- lapply(contvars.index, function(v) {
        n <- vnames[v]
        if (parametric[v]) {
          ## use parametric tests
          data.table(
            Variable = sprintf("%s%s", n, c("", ", M (SD)")[multi+1]),
            Res = sprintf("%0.2f (%0.2f)", mean(d[[n]], na.rm=TRUE), sd(d[[n]], na.rm=TRUE)))
        } else {
          data.table(
            Variable = sprintf("%s%s", n, c("", ", Mdn (IQR)")[multi+1]),
            Res = sprintf("%0.2f (%0.2f)", median(d[[n]], na.rm=TRUE),
                          abs(diff(quantile(d[[n]], c(.25, .75), na.rm = TRUE)))))
        }
      })

      names(tmpcont) <- vnames[contvars.index]
      tmpres <- c(tmpres, tmpcont)

      reslab <- paste0(reslab, c(ifelse(parametric[contvars.index[1]],
                                        "M (SD)", "Mdn (IQR)"), "See Rows")[multi+1])
    }

    if (length(catvars.index)) {
      tmpcat <- lapply(vnames[catvars.index], function(n) {
         x <- table(d[[n]])
        data.table(
          Variable = c(n, paste0("  ", names(x))),
          Res = c("", sprintf("%d (%2.1f)", x, prop.table(x) * 100)))
      })

      names(tmpcat) <- vnames[catvars.index]
      tmpres <- c(tmpres, tmpcat)

      reslab <- paste0(reslab, ifelse(nzchar(reslab),
                                      "/N (%)", "N (%)"))
    }

    tmpres <- lapply(tmpres[vnames], function(d) {
      setnames(d, old = names(d), c("Vars", reslab))
      return(d)
      })

    return(tmpres)
  })


  if (length(levels(g)) > 1) {
    tmpout <- lapply(seq_along(vnames), function(v) {
      out <- do.call(cbind, lapply(1:length(levels(g)), function(i) {
        d <- tmpout[[i]][[v]]
        setnames(d, old = names(d)[2], paste(levels(g)[i], names(d)[2], sep = " "))
        if (i == 1) {
          return(d)
        } else {
          return(d[, -1, with = FALSE])
        }
      }))

      if (length(contvars.index)) {
        if (v %in% contvars.index) {
          if (parametric[v]) {
            tests <- summary(aov(dv ~ g, data = data.table(dv = dat[[v]], g = g)))[[1]]
            out <- cbind(out,
                         Test = c(sprintf("F(%d, %d) = %0.2f, %s",
                                          tests[1, "Df"], tests[2, "Df"], tests[1, "F value"],
                                          formatPval(tests[1, "Pr(>F)"], 3, 3, includeP=TRUE)),
                                  rep("", nrow(out) - 1)))
          } else {
            tests <- kruskal.test(dv ~ g, data = data.frame(dv = dat[[v]], g = g))
            out <- cbind(out,
                         Test = c(sprintf("KW chi-square = %0.2f, df = %d, %s",
                                          tests$statistic, tests$parameter,
                                          formatPval(tests$p.value, 3, 3, includeP=TRUE)),
                                  rep("", nrow(out) - 1)))
          }
        }
      }

      if (length(catvars.index)) {
        if (v %in% catvars.index) {
          tests <- chisq.test(xtabs(~ dv + g, data = data.frame(dv = dat[[v]], g = g)),
                              correct = FALSE,
                              simulate.p.value = simChisq, B = sims)
          out <- cbind(out,
                       Test = c(sprintf("Chi-square = %0.2f, %s, %s",
                                        tests$statistic,
                                        ifelse(simChisq, "simulated", sprintf("df = %d", tests$parameter)),
                                        formatPval(tests$p.value, 3, 3, includeP=TRUE)),
                                rep("", nrow(out) - 1)))
        }
      }

      return(out)
    })
  } else {
    tmpout <- tmpout[[1]]
  }

  out <- do.call(rbind, tmpout)
  setnames(out, old = names(out)[1], "")

  return(out)
}

#' Winsorize at specified percentiles
#'
#' Simple function winsorizes data at the specified percentile.
#'
#' @param d A vector, matrix, data frame, or data table to be winsorized
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
#'
#' dat <- data.table(x = 1:5)
#' dat[, y := scale(1:5)]
#' winsorizor(dat$y, .01)
#'
#' ## make a copy of the data table
#' winsorizor(dat, .01)
#'
#' winsorizor(mtcars, .01)
#'
#' winsorizor(matrix(1:9, 3), .01)
#'
#' rm(dat) # clean up
winsorizor <- function(d, percentile, values, na.rm = TRUE) {
    if (!missing(percentile)) {
      stopifnot(percentile >= 0 && percentile <= 1)
    } else if (missing(percentile)) {
      percentile <- NA_real_
    }

  if (!is.vector(d) && !is.matrix(d) && !is.data.frame(d) && !is.data.table(d)) {
    if (is.atomic(d) && is.null(dim(d))) {
      warning("atomic type with no dimensions, coercing to a numeric vector. To remove this warning, try wrapping the data in as.numeric() or otherwise coercing to a vector prior to passing to winsorizor().")
      d <- as.numeric(d)
    }
  }

    stopifnot(is.vector(d) || is.matrix(d) || is.data.frame(d) || is.data.table(d))
    dismatrix <- is.matrix(d)

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
        d <- f(d, percentile = percentile, values = values, na.rm = na.rm)
    } else if (is.matrix(d) || is.data.frame(d) || is.data.table(d)) {
        if (length(percentile) == 1) {
          percentile <- rep(percentile, ncol(d))
        }

        if (is.data.table(d)) {
          d <- copy(d)
          if (missing(values)) {
            for (i in 1:ncol(d)) {
              v <- names(d)[i]
              d[, (v) := f(get(v), percentile = percentile[i], na.rm = na.rm)]
            }
          } else {
            for (i in 1:ncol(d)) {
              v <- names(d)[i]
              d[, (v) := f(get(v), percentile = percentile[i], values = values[i, ], na.rm = na.rm)]
            }
          }

          all.attr <- do.call(rbind, lapply(1:ncol(d), function(i) attr(d[[i]], "winsorizedValues")))
          all.attr$variable <- colnames(d)
          rownames(all.attr) <- NULL

          for (v in names(d)) {
            d[, (v) := as.vector(get(v))]
          }

        } else {

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
          d <- as.data.frame(lapply(tmp, as.vector))
          colnames(d) <- all.attr$variable

          if (dismatrix) {
            d <- as.matrix(d)
          }

        }


        attributes(d) <- c(attributes(d), winsorizedValues = list(all.attr))
    }

    return(d)
}


#' Mean decomposition of a variable by group(s)
#'
#' This function decomposes a variable in a long data set by grouping
#' factors, such as by ID.
#'
#' @param formula A formula of the variables to be used in the analysis.
#'   Should have the form: variable ~ groupingfactors.
#' @param data A data table or data frame containing the variables
#'   used in the formula.  This is a required argument.
#' @return A list of data tables with the means or residuals
#' @keywords multivariate
#' @importFrom stats terms
#' @export
#' @examples
#' meanDecompose(mpg ~ vs, data = mtcars)
#' meanDecompose(mpg ~ vs + cyl, data = mtcars)
#'
#' ## Example plotting the results
#' tmp <- meanDecompose(Sepal.Length ~ Species, data = iris)
#' do.call(plot_grid, c(lapply(names(tmp), function(x) {
#'   testdistr(tmp[[x]]$X, plot = FALSE, varlab = x)$Density
#' }), ncol = 1))
#'
#' rm(tmp)
meanDecompose <- function(formula, data) {
  v <- as.character(attr(terms(formula), "variables"))[-1]

  if (!is.data.table(data)) {
    data <- as.data.table(data)[, v, with = FALSE]
  } else {
    data <- data[, v, with = FALSE]
  }

  out <- vector("list", length = length(v))

  vres <- paste0(v[1], "_residual")
  stopifnot(!any(vres %in% v))

  data[, (vres) := get(v[1])]

  vfinal <- vector("character", length = length(v))

  for (i in 2:length(v)) {
    vname <- paste0(v[1], "_", v[i])
    data[, (vname) := mean(get(vres), na.rm = TRUE), by = c(v[2:i])]
    data[, (vres) := get(vres) - get(vname)]
    out[[i - 1]] <- data[, .(X = get(vname)[1]), by = c(v[2:i])]
    vfinal[i - 1] <- paste0(v[1], " by ", paste(v[2:i], collapse = " & "))
  }
  out[[length(v)]] <- data[, .(X = get(vres))]
  vfinal[length(v)] <- paste0(v[1], " by ", "residual")

  names(out) <- vfinal

  return(out)
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("vcov", "grp"))

#' Intraclass Correlation Coefficient (ICC) from Mixed Models
#'
#' This function estimates the ICC from mixed effects models
#' estimated using \pkg{lme4}.
#'
#' @param dv A character string giving the variable name of
#'   the dependent variable.
#' @param id A character vector of length one or more giving
#'   the ID variable(s).  Can be more than one.
#' @param data A data.table containing the variables
#'   used in the formula.  This is a required argument.
#'   If a data.frame, it will silently coerce to a data.table.
#'   If not a data.table or data.frame, it will attempt to coerce,
#'   with a message.
#' @param family A character vector giving the family to use
#'   for the model.  Currently only supports
#'   \dQuote{gaussian} or \dQuote{binomial}.
#' @return A data table of the ICCs
#' @references For details, see
#' Campbell, M. K., Mollison, J., & Grimshaw, J. M. (2001).
#' Cluster trials in implementation research: estimation of
#' intracluster correlation coefficients and sample size.
#' \emph{Statistics in Medicine, 20}(3), 391-399.
#' @keywords multivariate
#' @importFrom lme4 lmer glmer
#' @importFrom nlme VarCorr
#' @importFrom stats binomial
#' @export
#' @examples
#' iccMixed("mpg", "cyl", mtcars)
#' iccMixed("mpg", "cyl", as.data.table(mtcars))
#' iccMixed("mpg", "cyl", as.data.table(mtcars), family = "gaussian")
#' iccMixed("mpg", c("cyl", "am"), as.data.table(mtcars))
#' iccMixed("am", "cyl", as.data.table(mtcars), family = "binomial")
iccMixed <- function(dv, id, data, family = c("gaussian", "binomial")) {
  if (!is.data.table(data)) {
    if (is.data.frame(data)) {
      data <- as.data.table(data)
    } else {
      message("Attempting to coerce data to a data.table")
      data <- as.data.table(data)
    }
  }
  stopifnot(all(c(dv, id) %in% names(data)))
  stopifnot(is.character(dv))
  stopifnot(all(is.character(id)))
  stopifnot(identical(length(dv), 1L))
  stopifnot(length(id) >= 1L)

  d <- copy(data[, c(dv, id), with = FALSE])

  f <- sprintf("%s ~ 1 + %s", dv, paste(paste0("(1 | ", id, ")"), collapse = " + "))

  family <- match.arg(family)

  ## constant estimate of residual variance for logistic model
  ## on the 'latent variable' scale
  res.binom <- (pi^2) / 3

  m <- switch(family,
              gaussian = lmer(formula = as.formula(f), data = d, REML = TRUE),
              binomial = glmer(formula = as.formula(f), data = d, family = binomial())
              )

  est <- as.data.table(as.data.frame(VarCorr(m)))[, .(grp, vcov)]

  if (identical(family, "binomial")) {
    est <- rbind(est, est[1])
    est[nrow(est), c("grp", "vcov") := .("Residual", res.binom)]
  }

  est[, .(Var = grp, Sigma = vcov, ICC = vcov / sum(vcov))]
}




#' Estimate the effective sample size from longitudinal data
#'
#' This function estimates the (approximate) effective sample
#' size.
#'
#' @param n The number of unique/indepedent units of observation
#' @param k The (average) number of observations per unit
#' @param icc The estimated ICC.  If missing, will
#'   estimate (and requires that the family argument be
#'   correctly specified).
#' @param dv A character string giving the variable name of
#'   the dependent variable.
#' @param id A character vector of length one giving
#'   the ID variable.
#' @param data A data.table containing the variables
#'   used in the formula.  This is a required argument.
#'   If a data.frame, it will silently coerce to a data.table.
#'   If not a data.table or data.frame, it will attempt to coerce,
#'   with a message.
#' @param family A character vector giving the family to use
#'   for the model.  Currently only supports
#'   \dQuote{gaussian} or \dQuote{binomial}.
#' @return A data.table including the effective sample size.
#' @references For details, see
#' Campbell, M. K., Mollison, J., & Grimshaw, J. M. (2001).
#' Cluster trials in implementation research: estimation of
#' intracluster correlation coefficients and sample size.
#' \emph{Statistics in Medicine, 20}(3), 391-399.
#' @keywords multivariate
#' @export
#' @examples
#' ## example where n, k, and icc are estimated from the data
#' ## provided, partly using iccMixed function
#' nEffective(dv = "mpg", id = "cyl", data = mtcars)
#'
#' ## example where n, k, and icc are known (or being 'set')
#' ## useful for sensitivity analyses
#' nEffective(n = 60, k = 10, icc = .6)
nEffective <- function(n, k, icc, dv, id, data, family = c("gaussian", "binomial")) {
  if (any(missing(n), missing(k), missing(icc))) {
    if (!is.data.table(data)) {
      if (is.data.frame(data)) {
        data <- as.data.table(data)
      } else {
        message("Attempting to coerce data to a data.table")
        data <- as.data.table(data)
      }
    }
    stopifnot(all(c(dv, id) %in% names(data)))
    stopifnot(is.character(dv))
    stopifnot(all(is.character(id)))
    stopifnot(identical(length(dv), 1L))
    stopifnot(identical(length(id), 1L))

    d <- copy(data[, c(dv, id), with = FALSE])

    if (missing(icc)) {
      icc <- iccMixed(dv = dv, id = id, data = data, family = family)$ICC[1]
    }

    if (missing(n)) {
      n <- length(unique(data[[id]]))
    }

    if (missing(k)) {
      k <- nrow(data) / n
    }
  }

  neff <- (n * k) / ((1 + (k - 1) * icc))

  data.table(
    Type = c("Effective Sample Size", "Independent Units", "Total Observations"),
    N = c(neff, n, n * k))
}

#' Function to calculate the mean and deviations from mean
#'
#' Tiny helper function to calculate the mean and
#' deviations from the mean, both returned as a list.
#' Works nicely with data.table to calculate a between and
#' within variable.
#'
#' @param x A vector, appropriate for the \code{mean}
#'   function.
#' @param na.rm A logical, whether to remove missing
#'   or not.  Defaults to \code{TRUE}.
#' @return A list of the mean (first element) and deviations
#'   from the mean (second element).
#' @export
#' @examples
#' ## simple example showing what it does
#' meanDeviations(1:10)
#'
#' ## example use case, applied to a data.table
#' d <- as.data.table(iris)
#' d[, c("BSepal.Length", "WSepal.Length") := meanDeviations(Sepal.Length),
#'   by = Species]
#' str(d)
meanDeviations <- function(x, na.rm = TRUE) {
  m <- mean(x, na.rm = na.rm)
  list(m, x - m)
}


#' Calculate a rounded five number summary
#'
#' Numbers are the minimum, 25th percentile, median,
#' 75th percentile, and maximum, of the non missing data.
#' Values returned are either the significant digits or rounded values,
#' whichever ends up resulting in the fewest total digits.
#'
#' @param x The data to have the summary calculated on
#' @param round The number of digits to try rounding
#' @param sig The number of significant digits to try
#' @return The rounded or significant digit five number summary
#' @importFrom stats fivenum
#' @examples
#' JWileymisc:::roundedfivenum(rnorm(1000))
#' JWileymisc:::roundedfivenum(mtcars$hp)
roundedfivenum <- function(x, round = 2, sig = 3) {
  x <- fivenum(x[!is.na(x)])
  if(max(nchar(signif(x, sig))) < max(nchar(round(x, round)))) {
    signif(x, sig)
  } else {
    round(x, round)
  }
}


#' Estimate the effective sample size from longitudinal data
#'
#' This function estimates the (approximate) effective sample
#' size.
#'
#' @param xvar A character string giving the variable name of
#'   the variable to calculate autocorrelations on.
#' @param timevar A character string giving the variable name of
#'   the time variable.
#' @param idvar A character string giving the variable name of
#'   the ID variable.  Can be missing if only one time series
#'   provided, in which case one will be created.
#' @param data A data.table containing the variables
#'   used in the formula.  This is a required argument.
#'   If a data.frame, it will silently coerce to a data.table.
#'   If not a data.table or data.frame, it will attempt to coerce,
#'   with a message.
#' @param lag.max An integer of the maximum lag to estimate. Must be
#'   equal to or greater than the number of observations
#'   for all IDs in the dataset.
#' @param na.function A character string giving the name of the function
#'   to use to address any missing data.  Functions come from the
#'   \pkg{zoo} package, and must be one of:
#'   \dQuote{na.approx}, \dQuote{na.spline}, \dQuote{na.locf}.
#' @param ... Additional arguments passed to \code{zoo}.
#' @return A data.table of the estimated autocorrelations by ID and lag
#' @references For details, see
#' Campbell, M. K., Mollison, J., & Grimshaw, J. M. (2001).
#' Cluster trials in implementation research: estimation of
#' intracluster correlation coefficients and sample size.
#' \emph{Statistics in Medicine, 20}(3), 391-399.
#' @keywords multivariate
#' @importFrom zoo zoo na.approx na.spline na.locf
#' @importFrom stats acf
#' @export
#' @examples
#' ## example 1
#' dat <- data.table(
#'   x = sin(1:30),
#'   time = 1:30,
#'   id = 1)
#' acfByID("x", "time", "id", data = dat)
#'
#' ## example 2
#' dat2 <- data.table(
#'   x = c(sin(1:30), sin((1:30)/10)),
#'   time = c(1:30, 1:30),
#'   id = rep(1:2, each = 30))
#' dat2$x[4] <- NA
#'
#' res <- acfByID("x", "time", "id", data = dat2, na.function = "na.approx")
#'
#' ggplot(res, aes(factor(Lag), AutoCorrelation)) +
#'   geom_boxplot()
#'
#' ## clean up
#' rm(dat, dat2, res)
acfByID <- function(xvar, timevar, idvar, data, lag.max = 10L, na.function = c("na.approx", "na.spline", "na.locf"), ...) {
  if (!is.data.table(data)) {
    if (is.data.frame(data)) {
      data <- as.data.table(data)
    } else {
      message("Attempting to coerce data to a data.table")
      data <- as.data.table(data)
    }
  }

  stopifnot(is.integer(lag.max))
  stopifnot(is.character(xvar))
  stopifnot(is.character(timevar))

  stopifnot(all(c(xvar, timevar) %in% names(data)))
  stopifnot(identical(length(xvar), 1L))
  stopifnot(identical(length(timevar), 1L))

  na.function <- match.arg(na.function)
  na.function <- switch(na.function,
                        na.approx = na.approx,
                        na.spline = na.spline,
                        na.locf = na.locf)

  if (!missing(idvar)) {
    stopifnot(is.character(idvar))
    stopifnot(idvar %in% names(data))
    stopifnot(identical(length(idvar), 1L))

    d <- copy(data[, c(xvar, timevar, idvar), with = FALSE])
  } else {
    d <- copy(data[, c(xvar, timevar), with = FALSE])
    idvar <- "ID"
    while(idvar %in% names(d)) {
      idvar <- paste0("TMP_", idvar)
    }
    d[, (idvar) := 1L]
  }


  d[, .(
    Variable = xvar,
    Lag = 0:lag.max,
    AutoCorrelation = acf(na.function(zoo(get(xvar), order.by = get(timevar))),
                          lag.max = lag.max, plot = FALSE, ...)$acf[, 1, 1]),
    by = idvar]
}


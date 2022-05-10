#' Calculate a Circular Mean
#'
#' Function to calculate circular mean
#'
#' @param x Numeric or integer values
#' @param max The theoretical maximum (e.g., if degrees, 360)
#' @param na.rm A logical value indicating whether to remove missing values.
#'   Defaults to \code{TRUE}.
#' @return A numeric value with the circular mean.
#' @export
#' @examples
#' meanCircular(c(22:23, 1:2), max = 24)
#' meanCircular(c(12, 24), max = 24)
#' meanCircular(c(6, 7, 23), max = 24)
#' meanCircular(c(6, 7, 21), max = 24)
#' meanCircular(c(6, 21), max = 24)
#' meanCircular(c(6, 23), max = 24)
#' meanCircular(c(.91, .96, .05, .16), max = 1)
#' meanCircular(c(6, 7, 8, 9), max = 24)
#' meanCircular(1:3, max = 24)
#' meanCircular(21:23, max = 24)
#' meanCircular(c(16, 17, 18, 19), max = 24)
#' meanCircular(c(355, 5, 15), max = 360)
#'
meanCircular <- function(x, max, na.rm = TRUE) {
  if(!(is.integer(x) || is.numeric(x))) {
    stop(sprintf("x must be class integer or numeric but was %s",
                 paste(class(x), collapse = "; ")))
  }

  if (na.rm) {
    x <- x[!is.na(x)]
    if (!length(x)) {
      return(NA_real_)
    }
  } else if (anyNA(x)) {
    return(NA_real_)
  }

  if (any(x < 0)) {
    stop("For cicular means, cannot have negative numbers")
  }
  if (any(x > max)) {
    stop("For cicular means, cannot have values above the theoretical maximum")
  }

  scale <- 360 / max
  rad <- x * scale * (pi / 180)
  mc <- mean.default(cos(rad))
  ms <- mean.default(sin(rad))

  if (ms >= 0) {
    out <- atan2(ms, mc)
  } else if (ms < 0) {
    out <- atan2(ms, mc) + 2 * pi
  }

  ## if (ms > 0 && mc > 0) {
  ##   out <- atan2(ms, mc)
  ## } else if (mc <= 0) {
  ##   out <- atan2(ms, mc) + pi
  ## } else if (ms < 0 && mc > 0) {
  ##   out <- atan2(ms, mc) + 2 * pi
  ## }

  ## radians to degrees and unscale to the inputs
  out <- out * (180 / pi) / scale
  if (out == max) {
    out <- 0
  }
  return(out)
}

#' Calculate the Circular Difference
#'
#' @param x Numeric or integer values
#' @param y Numeric or integer values
#' @param max the theoretical maximum (e.g., if degrees, 360; if hours, 24; etc.).
#' @return A value with the circular difference. This will always be positive if defined.
#' @export
#' @examples
#' diffCircular(330, 30, max = 360)
#' diffCircular(22, 1, max = 24)
#' diffCircular(c(22, 23, 21, 22), c(1, 1, 23, 14), max = 24)
diffCircular <- function(x, y, max) {
    if (!(is.integer(x) || is.numeric(x))) {
        stop(sprintf("x must be class integer or numeric but was %s", 
            paste(class(x), collapse = "; ")))
    }
    if (!(is.integer(y) || is.numeric(y))) {
        stop(sprintf("y must be class integer or numeric but was %s", 
            paste(class(y), collapse = "; ")))
    }

    stopifnot(identical(length(x), length(y)))
    
    if (any(x < 0 | y < 0, na.rm = TRUE)) {
      stop("For cicular means, cannot have negative numbers")
    }
    if (any(x > max | y > max, na.rm = TRUE)) {
      stop("For cicular means, cannot have values above the theoretical maximum")
    }

    scale <- 360/max
    torad <- scale * (pi / 180)
    toorig <- (180 / pi) / scale
    radx <- x * torad
    rady <- y * torad
    acos(cos(radx - rady)) * toorig 
}


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

#' Calculate Phi or Cramer's V effect size
#'
#' Simple function to calculate effect sizes for frequency tables.
#'
#' @param x A frequency table, such as from \code{xtabs()}.
#' @return A numeric value with Phi for 2 x 2 tables or Cramer's V
#'   for tables larger than 2 x 2.
#' @importFrom stats ftable
#' @importFrom MASS loglm
#' @export
#' @examples
#' cramerV(xtabs(~ am + vs, data = mtcars))
#' cramerV(xtabs(~ cyl + vs, data = mtcars))
#' cramerV(xtabs(~ cyl + am, data = mtcars))
cramerV <- function(x) {
  if (length(dim(x)) > 2L) {
    stop("Cannot calculate effect size on more than two dimensions")
  }

  x2 <- ftable(x)
  chi2 <- summary(MASS::loglm(~ 1 + 2, x2))$tests["Pearson", "X^2"]
  N <- sum(x2)
  k <- min(dim(x2))

  out <- sqrt(chi2 / (N * (k - 1)))

  if (identical(dim(x), c(2L, 2L))) {
    names(out) <- "Phi"
  } else {
    names(out) <- "Cramer's V"
  }
  return(out)
}

#' Calculate Standardized Mean Difference (SMD)
#'
#' Simple function to calculate effect sizes for mean differences.
#'
#' @param x A continuous variable
#' @param g A grouping variable, with two levels
#' @param index A character string: \dQuote{all} uses pooled variance,
#'   \dQuote{1} uses the first factor level variance,
#'   \dQuote{2} uses the second factor level variance.
#' @return The standardized mean difference.
#' @importFrom stats var
#' @export
#' @examples
#' smd(mtcars$mpg, mtcars$am)
#' smd(mtcars$mpg, mtcars$am, "all")
#' smd(mtcars$mpg, mtcars$am, "1")
#' smd(mtcars$mpg, mtcars$am, "2")
#'
#' smd(mtcars$hp, mtcars$vs)
#'
#' d <- data.table::as.data.table(mtcars)
#' d[, smd(mpg, vs)]
#' rm(d)
smd <- function(x, g, index = c("all", "1", "2")) {
  index <- match.arg(index)

  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]

  if (!is.factor(g)) {
    g <- factor(g)
  }
  k <- length(levels(g))

  if (!identical(k, 2L)) stop("Must have two groups")

  m <- as.vector(by(x, g, mean))
  v <- as.vector(by(x, g, var))
  n <- as.vector(by(x, g, length))

  useV <- switch(index,
    `all` = sum((n - 1) * v) / (sum(n) - k),
    `1` = v[1],
    `2` = v[2])

  out <- abs(diff(m) / sqrt(useV))

  names(out) <- "SMD"
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
#' @importFrom stats terms as.formula
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
#' ## sample data
#' Xmiss <- as.matrix(iris[, -5])
#' # make q0% missing completely at random
#' set.seed(10)
#' Xmiss[sample(length(Xmiss), length(Xmiss) * .10)] <- NA
#' Xmiss <- as.data.frame(Xmiss)
#'
#' SEMSummary(~ ., data = Xmiss, use = "fiml")
#'
#'
#' ## pairwise
#' APAStyler(SEMSummary(~ ., data = Xmiss, use = "pair"),
#'   type = "cor")
#'
#' ## same as cor()
#' cor(Xmiss, use = "pairwise.complete.obs")
#'
#' ## complete cases only
#' SEMSummary(~ ., data = Xmiss, use = "comp")
#'
#' ## clean up
#' rm(Xmiss)
SEMSummary <- function(formula, data,
  use = c("fiml", "pairwise.complete.obs", "complete.obs")) {
  env <- environment(formula)

  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  use <- match.arg(use)

  tmp <- unlist(strsplit(paste(deparse(formula), collapse = ""), "\\|"))
  formula <- as.formula(tmp[1], env = env)

  if (length(tmp) > 1) {
    condition <- as.formula(paste0("~ ", tmp[2]), env = env)
    vars <- attr(terms(condition, data = data), "variables")
    vnames <- as.character(vars)[-1L]

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
#' @importFrom stats cov cov2cor pt cor
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

  if (identical(use, "pairwise.complete.obs")) {
    sSigma <- cor(X, use = "pairwise.complete.obs")
  } else {
    sSigma <- cov2cor(Sigma)
  }

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


## clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("dv1", "dv2"))

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
#' @param data optional argument of the dataset containing
#'   the variables to be described.
#' @param idvar A character string indicating the variable name
#'   of the ID variable.  Not currently used, but will eventually
#'   support \code{egltable} supporting repeated measures data.
#' @param strict Logical, whether to strictly follow the
#'   type of each variable, or to assume categorical if
#'   the number of unique values is less than or equal to 3.
#' @param parametric Logical whether to use parametric tests in the
#'   case of multiple groups to test for differences.  Only applies to
#'   continuous variables. If \code{TRUE}, the default, uses one-way ANOVA,
#'   and a F test. If \code{FALSE}, uses the Kruskal-Wallis test.
#' @param paired Logical whether the data are paired or not. Defaults to
#'   \code{FALSE}. If \code{TRUE}, the grouping variable, \code{g},
#'   must have two levels and \code{idvar} must be specified. When used
#'   a paired t-test is used for parametric, continuous data and a
#'   Wilcoxon test for paired  non parametric, continuous data and a McNemar
#'   chi square test is used for categorical data.
#' @param simChisq Logical whether to estimate p-values for chi-square test
#'   for categorical data when there are multiple groups, by simulation.
#'   Defaults to \code{FALSE}. Useful when there are small cells as will
#'   provide a more accurate test in extreme cases, similar to Fisher Exact
#'   Test but generalizing to large dimension of tables.
#' @param sims Integer for the number of simulations to be used to estimate
#'   p-values for the chi-square tests for categorical variables when
#'   there are multiple groups. Defaults to one million (\code{1e6L}).
#' @return A data frame of the table.
#' @keywords utils
#' @export
#' @importFrom stats sd aov chisq.test kruskal.test quantile xtabs t.test
#' @importFrom stats wilcox.test mcnemar.test
#' @importFrom data.table is.data.table as.data.table setnames
#' @examples
#' egltable(iris)
#' egltable(colnames(iris)[1:4], "Species", data = iris)
#' egltable(iris, parametric = FALSE)
#' egltable(colnames(iris)[1:4], "Species", iris,
#'   parametric = FALSE)
#' egltable(colnames(iris)[1:4], "Species", iris,
#'   parametric = c(TRUE, TRUE, FALSE, FALSE))
#' egltable(colnames(iris)[1:4], "Species", iris,
#'   parametric = c(TRUE, TRUE, FALSE, FALSE), simChisq=TRUE)
#'
#' diris <- data.table::as.data.table(iris)
#' egltable("Sepal.Length", g = "Species", data = diris)
#'
#' tmp <- mtcars
#' tmp$cyl <- factor(tmp$cyl)
#' tmp$am <- factor(tmp$am, levels = 0:1)
#'
#' egltable(c("mpg", "hp"), "vs", tmp)
#' egltable(c("mpg", "hp"), "am", tmp)
#' egltable(c("am", "cyl"), "vs", tmp)
#'
#' tests <- with(sleep,
#'     wilcox.test(extra[group == 1],
#'            extra[group == 2], paired = TRUE))
#' str(tests)
#'
#' ## example with paired data
#' egltable(c("extra"), g = "group", data = sleep, idvar = "ID", paired = TRUE)
#'
#' ## what happens when ignoring pairing (p-value off)
#' # egltable(c("extra"), g = "group", data = sleep, idvar = "ID")
#'
#' ## paired categorical data example
#' ## using data on chick weights to create categorical data
#' tmp <- subset(ChickWeight, Time %in% c(0, 20))
#' tmp$WeightTertile <- cut(tmp$weight,
#'   breaks = quantile(tmp$weight, c(0, 1/3, 2/3, 1), na.rm = TRUE),
#'   include.lowest = TRUE)
#'
#' egltable(c("weight", "WeightTertile"), g = "Time",
#'   data = tmp,
#'   idvar = "Chick", paired = TRUE)
#'
#' rm(tmp)
egltable <- function(vars, g, data, idvar, strict=TRUE, parametric = TRUE,
                     paired = FALSE, simChisq = FALSE, sims = 1e6L) {
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
      if (anyNA(ids)) stop("cannot have missing IDs")
    }
  } else {
    dat <- as.data.table(vars)
  }

  if (missing(g)) {
    g <- rep(1, nrow(dat))
  }

  g <- droplevels(as.factor(g))

  if (isTRUE(paired)) {
    stopifnot(identical(length(unique(g)), 2L))
    stopifnot(isFALSE(missing(idvar)))
  }

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

  if (isTRUE(length(catvars.index)>0)) {
    for (n in vnames[catvars.index]) {
      if (isFALSE(is.factor(dat[[n]]))) {
        dat[[n]] <- factor(dat[[n]])
      }
    }
  }

  tmpout <- lapply(levels(g), function(gd) {
    d <- dat[which(g == gd)]
    tmpres <- NULL
    reslab <- ""

    if (isTRUE(length(contvars.index) > 0)) {
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

    if (isTRUE(length(catvars.index)>0)) {
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


  if (isTRUE(length(levels(g)) > 1)) {
    tmpout <- lapply(seq_along(vnames), function(v) {

      out <- do.call(cbind, lapply(1:length(levels(g)), function(i) {
        d <- tmpout[[i]][[v]]
        setnames(d, old = names(d)[2], paste(levels(g)[i], names(d)[2], sep = " "))
        if (isTRUE(i == 1)) {
          return(d)
        } else {
          return(d[, -1, with = FALSE])
        }
      }))

      if (isTRUE(length(contvars.index) > 0)) {
        if (isTRUE(v %in% contvars.index)) {
          if (isTRUE(parametric[v])) {
            if (isTRUE(length(levels(g)) > 2)) {
              tests <- summary(aov(dv ~ g, data = data.table(dv = dat[[v]], g = g)))[[1]]
              es <- tests[1, "Sum Sq"] / sum(tests[, "Sum Sq"])
              out <- cbind(out,
                           Test = c(sprintf("F(%d, %d) = %0.2f, %s, Eta-squared = %0.2f",
                                            tests[1, "Df"], tests[2, "Df"], tests[1, "F value"],
                                            formatPval(tests[1, "Pr(>F)"], 3, 3, includeP=TRUE),
                                            es),
                                    rep("", nrow(out) - 1)))
            } else if (isTRUE(length(levels(g)) == 2) && isFALSE(paired)) {

              tests <- t.test(dv ~ g, data = data.table(dv = dat[[v]], g = g),
                              var.equal=TRUE)
              dv = NULL # for R CMD check
              es <- data.table(dv = dat[[v]], g = g)[, smd(dv, g, "all")]
              out <- cbind(out,
                           Test = c(sprintf("t(df=%0.0f) = %0.2f, %s, d = %0.2f",
                                            tests$parameter[["df"]],
                                            tests$statistic[["t"]],
                                            formatPval(tests$p.value, 3, 3, includeP=TRUE),
                                            es),
                                    rep("", nrow(out) - 1)))
            } else if (isTRUE(length(levels(g)) == 2) && isTRUE(paired)) {
              ## paired t-test and cohen's D on difference scores
              widedat <- copy(reshape(data.table(
                dv = dat[[v]],
                g = as.integer(factor(g)),
                ID = ids),
                v.names = "dv",
                timevar = "g",
                idvar = "ID",
                direction = "wide", sep = ""))
              widedat[, diff := dv2 - dv1]

              tests <- t.test(widedat$dv2, widedat$dv1, paired = TRUE)
              es <- mean(widedat$diff, na.rm = TRUE) / sd(widedat$diff, na.rm = TRUE)
              out <- cbind(out,
                           Test = c(sprintf("t(df=%0.0f) = %0.2f, %s, d = %0.2f",
                                            tests$parameter[["df"]],
                                            tests$statistic[["t"]],
                                            formatPval(tests$p.value, 3, 3, includeP=TRUE),
                                            es),
                                    rep("", nrow(out) - 1)))

            }
          } else {
            if (isFALSE(paired)) {
            tests <- kruskal.test(dv ~ g, data = data.frame(dv = dat[[v]], g = g))
            out <- cbind(out,
                         Test = c(sprintf("KW chi-square = %0.2f, df = %d, %s",
                                          tests$statistic, tests$parameter,
                                          formatPval(tests$p.value, 3, 3, includeP=TRUE)),
                                  rep("", nrow(out) - 1)))
            } else if (isTRUE(length(levels(g)) == 2) && isTRUE(paired)) {
              ## non parametric paired wilcoxon test
              widedat <- copy(reshape(data.table(
                dv = dat[[v]],
                g = as.integer(factor(g)),
                ID = ids),
                v.names = "dv",
                timevar = "g",
                idvar = "ID",
                direction = "wide", sep = ""))

              tests <- wilcox.test(widedat$dv2, widedat$dv1, paired = TRUE)
              out <- cbind(out,
                           Test = c(sprintf("Wilcoxon Paired V = %0.2f, %s",
                                            tests$statistic,
                                            formatPval(tests$p.value, 3, 3, includeP=TRUE)),
                                    rep("", nrow(out) - 1)))
            }
          }
        }
      }

      if (isTRUE(length(catvars.index) > 0)) {
        if (isTRUE(v %in% catvars.index)) {

          if (isFALSE(paired)) {

          tabs <- xtabs(~ dv + g, data = data.frame(dv = dat[[v]], g = g))
          es <- cramerV(tabs)
          tests <- chisq.test(tabs,
                              correct = FALSE,
                              simulate.p.value = simChisq,
                              B = sims)
          out <- cbind(out,
                       Test = c(sprintf("Chi-square = %0.2f, %s, %s, %s",
                                        tests$statistic,
                                        ifelse(simChisq, "simulated", sprintf("df = %d", tests$parameter)),
                                        formatPval(tests$p.value, 3, 3, includeP=TRUE),
                                        sprintf("%s = %0.2f", names(es), es)
                                        ),
                                rep("", nrow(out) - 1)))
          } else if (isTRUE(length(levels(g)) == 2) && isTRUE(paired)) {
            ## mcnemar test for paired data
            widedat <- copy(reshape(data.table(
              dv = factor(dat[[v]], levels = unique(dat[[v]])),
              g = as.integer(factor(g)),
              ID = ids),
              v.names = "dv",
              timevar = "g",
              idvar = "ID",
              direction = "wide", sep = ""))
            tab <- xtabs(~ dv1 + dv2, data = widedat)

              tests <- mcnemar.test(tab)
              out <- cbind(out,
                           Test = c(sprintf("McNemar's Chi-square = %0.2f, df = %d, %s",
                                            tests$statistic, tests$parameter,
                                            formatPval(tests$p.value, 3, 3, includeP=TRUE)),
                                    rep("", nrow(out) - 1)))
          }
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
#' @importFrom data.table data.table := copy is.data.table
#' @importFrom extraoperators %age% %ale%
#' @export
#' @examples
#' dev.new(width = 10, height = 5)
#' par(mfrow = c(1, 2))
#' hist(as.vector(eurodist), main = "Eurodist")
#' hist(winsorizor(as.vector(eurodist), .05), main = "Eurodist with lower and upper\n5% winsorized")
#'
#' library(data.table)
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
      stopifnot(percentile %age% 0 && percentile %ale% 1)
    } else if (missing(percentile)) {
      percentile <- NA_real_
    }

  if (!is.vector(d) && !is.matrix(d) && !is.data.frame(d) && !is.data.table(d)) {
    if (is.atomic(d) && is.null(dim(d))) {
      warning(paste0(
        "Atomic type with no dimensions, coercing to a numeric vector.\n",
        "To remove this warning, try wrapping the data in as.numeric() or\n",
        "otherwise coercing to a vector prior to passing to winsorizor()."))
      d <- as.numeric(d)
    }
  }
    stopifnot(is.vector(d) || is.matrix(d) || is.data.frame(d) || is.data.table(d))
    dismatrix <- is.matrix(d)

    f <- function(x, percentile, values, na.rm) {
          if (!missing(values)) {
            low <- values[, "low"]
            high <- values[, "high"]
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
            for (i in seq_len(ncol(d))) {
              v <- names(d)[i]
              d[, (v) := f(get(v), percentile = percentile[i], na.rm = na.rm)]
            }
          } else {
            for (i in seq_len(ncol(d))) {
              v <- names(d)[i]
              d[, (v) := f(get(v), percentile = percentile[i], values = values[i, ], na.rm = na.rm)]
            }
          }

          all.attr <- do.call(rbind, lapply(seq_len(ncol(d)), function(i) attr(d[[i]], "winsorizedValues")))
          all.attr$variable <- colnames(d)
          rownames(all.attr) <- NULL

          for (v in names(d)) {
            d[, (v) := as.vector(get(v))]
          }

        } else {

          if (missing(values)) {
            tmp <- lapply(seq_len(ncol(d)), function(i) {
              f(d[, i], percentile = percentile[i], na.rm = na.rm)
            })
          } else {
            tmp <- lapply(seq_len(ncol(d)), function(i) {
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
  x <- as.numeric(quantile(x[!is.na(x)], breaks = c(0, .25, .5, .75, 1),
                type = 7))
  if(max(nchar(signif(x, sig))) < max(nchar(round(x, round)))) {
    signif(x, sig)
  } else {
    round(x, round)
  }
}

#' Calculate F and p-value from the R2
#'
#' @param r2 r squareds
#' @param numdf numerator degrees of freedom
#' @param dendf denominator degrees of freedom
#' @return a vector
#' @keywords internal
#' @importFrom stats pf
#' @examples
#' # make me!
f.r2 <- function(r2, numdf, dendf) {
  F <- (dendf/numdf) * (-r2/(r2 - 1))
  p <- pf(F, df1 = numdf, df2 = dendf, lower.tail = FALSE)
  c(F = F[[1]], NumDF = numdf, DenDF = dendf, p = p[[1]])
}

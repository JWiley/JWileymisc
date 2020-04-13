#' @title Test the distribution of a variable against a specific distribution
#'
#' @description Function designed to help examine distributions.
#' It also includes an option for assessing multivariate normality using the
#' (squared) Mahalanobis distance. A generic function, some methods, and
#' constructor (\code{as.testDistribution}) and function to check class
#' (\code{is.testDistribution}) also are provided.
#'
#' Note that for the \code{use} argument, several options are possible.
#' By default it is \dQuote{complete.obs}, which uses only cases with complete
#' data on all variables.
#' Another option is \dQuote{pairwise.complete.obs}, which uses
#' all available data for each variable indivdiually to estimate the means and
#' variances, and all pairwise complete observation pairs for each covariance. Because
#' the same cases are not used for all estimates, it is possible to obtain a covariance
#' matrix that is not positive definite (e.g., correlations > +1 or < -1).
#'
#' Finally, the last option is \dQuote{fiml}, which uses full information maximum likelihood
#' estimates of the means and covariance matrix.  Depending on the number of cases,
#' missing data patterns, and variables, this may be quite slow and computationally
#' demanding.
#'
#' The \code{robust} argument determines whether to use robust estimates or not
#' when calculating densities, etc.  By default it is \code{FALSE}, but if
#' \code{TRUE} and a univariate or multivariate normal distribution is tested,
#' then robust estimates of the means and covariance matrix (a variance if univariate)
#' will be used based on \code{covMcd} from the \pkg{robustbase} package.
#'
#' @param x The data as a single variable or vector to check the distribution unless
#'   the distribution is \dQuote{mvnormal} in which case it should be a data frame or
#'   data table.
#' @return A logical whether or not an object is of class
#'   \code{testDistribution} or an object of the same class.
#' @seealso \code{\link{SEMSummary}}
#' @export
#' @keywords multivariate
#' @rdname testDistribution
testDistribution <- function(x, ...) {
  UseMethod("testDistribution", x)
}

#' @importFrom data.table is.data.table as.data.table
#' @export
#' @rdname testDistribution
as.testDistribution <- function(x) {
  if (!is.testDistribution(x)) {
    if(!is.list(x)) {
      stop("Input must be a list or a testDistribution object")
    }
    x <- list(
      Data = x[[1]],
      Distribution = x[[2]],
      EVLimits = x[[3]],
      NOK = x[[4]],
      distr = x[[5]],
      na.rm = x[[6]],
      extremevalues = x[[7]],
      ev.perc = x[[8]],
      use = x[[9]],
      robust = x[[10]])
    class(x) <- "testDistribution"
  }

  ## checks that the object is not malformed
  stopifnot(identical(length(x), 10L))
  stopifnot(identical(names(x), c("Data", "Distribution", "EVLimits", "NOK",
                                  "distr", "na.rm", "extremevalues", "ev.perc", "use", "robust")))

  stopifnot(identical(length(x$Distribution), 5L))
  stopifnot(identical(length(x$EVLimits), 2L))
  stopifnot(identical(length(x$NOK), 1L))
  stopifnot(identical(length(x$distr), 1L))
  stopifnot(identical(length(x$na.rm), 1L))
  stopifnot(identical(length(x$extremevalues), 1L))
  stopifnot(identical(length(x$ev.perc), 1L))
  stopifnot(identical(length(x$use), 1L))
  stopifnot(identical(length(x$robust), 1L))

  stopifnot(is.list(x$Distribution))
  stopifnot(is.numeric(x$EVLimits))
  stopifnot(is.numeric(x$NOK))
  stopifnot(is.character(x$distr))
  stopifnot(is.logical(x$na.rm))
  stopifnot(is.character(x$extremevalues))
  stopifnot(is.numeric(x$ev.perc))
  stopifnot(is.character(x$use))
  stopifnot(is.logical(x$robust))

  return(x)
}

#' @export
#' @rdname testDistribution
is.testDistribution <- function(x) {
  inherits(x, "testDistribution")
}


#' @param distr A character string indicating the distribution to be tested.
#'   Currently one of: \dQuote{normal}, \dQuote{beta}, \dQuote{chisq} (chi-squared),
#'   \dQuote{f}, \dQuote{gamma}, \dQuote{nbinom} (negative binomial),
#'   \dQuote{poisson}, or \dQuote{mvnormal} for multivariate normal where Mahalanobis
#'   distances are calculated and compared against a Chi-squared distribution with
#'   degrees of freedom equal to the number of variables.
#' @param na.rm A logical value whether to omit missing values. Defaults to \code{TRUE}.
#' @param starts A named list of the starting values. Not required for all distributions.
#'   Passed on to \code{fitdistr} which fits the maximum likelihood estimates of the
#'   distribution parameters.
#' @param extremevalues A character vector whether to indicate extreme values.
#'   Should be \dQuote{no} to do nothing, \dQuote{empirical} to show extreme
#'   values based on the observed data percentiles, or \dQuote{theoretical}
#'   to show extreme values based on percentiles of the theoretical distribution.
#' @param ev.perc Percentile to use for extreme values.  For example if .01,
#'   then the lowest 1 percent and highest 1 percent will be labelled
#'   extreme values.  Defaults to the lowest and highest 0.5 percent.
#' @param use A character vector indicating how the moments
#'   (means and covariance matrix) should be estimated in the presence of
#'   missing data when \code{distr = mvnormal}.
#'   The default is to use complete observations, but
#'   full information maximum likelihood based on functions in
#'   \pkg{lavaan} is also available.  See details.
#' @param robust A logical whether to use robust estimation or not.
#'   Currently only applies to normally distributed data
#'   (univariate or multivariate).  Also, when \code{robust = TRUE},
#'   only complete observations are used (i.e., \code{use = "complete.obs"}).
#'   See details.
#' @param ... Additional arguments. If these include mu and sigma and the distribution
#'   is multivariate normal, then it will use the passed values instead of calculating
#'   the mean and covariances of the data.
#' @return A list with information about the distribution (parameter estimates,
#'   name, log likelihood (useful for comparing the fit of different distributions
#'   to the data), and a dataset with the sorted data and theoretical quantiles.
#' @importFrom MASS fitdistr
#' @importFrom stats dnorm qnorm dbeta qbeta dchisq qchisq
#' @importFrom stats df qf dgamma qgamma dnbinom qnbinom dpois qpois
#' @importFrom stats logLik ppoints
#' @importFrom stats mahalanobis qchisq ppoints
#' @importFrom stats lm resid offset
#' @importFrom data.table melt
#' @importFrom extraoperators %gele%
#' @importFrom robustbase covMcd
#' @method testDistribution default
#' @export
#' @rdname testDistribution
#' @examples
#'
#' \dontrun{
#'
#' ## example data
#' set.seed(1234)
#' d <- data.table::data.table(
#'   Ynorm = rnorm(200),
#'   Ybeta = rbeta(200, 1, 4),
#'   Ychisq = rchisq(200, 8),
#'   Yf = rf(200, 5, 10),
#'   Ygamma = rgamma(200, 2, 2),
#'   Ynbinom = rnbinom(200, mu = 4, size = 9),
#'   Ypois = rpois(200, 4))
#'
#' ## testing and graphing
#' testDistribution(d$Ybeta, "beta", starts = list(shape1 = 1, shape2 = 4))
#' testDistribution(d$Ychisq, "chisq", starts = list(df = 8))
#'
#' ## for chi-square distribution, extreme values only on
#' ## the right tail
#' testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
#'   extremevalues = "empirical", ev.perc = .1)
#' testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
#'   extremevalues = "theoretical", ev.perc = .1)
#'
#' testDistribution(d$Yf, "f", starts = list(df1 = 5, df2 = 10))
#' testDistribution(d$Ygamma, "gamma")
#' testDistribution(d$Ynbinom, "poisson")
#' testDistribution(d$Ynbinom, "nbinom")
#' testDistribution(d$Ypois, "poisson")
#'
#' ## compare log likelihood of two different distributions
#' testDistribution(d$Ygamma, "normal")$Distribution$LL
#' testDistribution(d$Ygamma, "gamma")$Distribution$LL
#'
#' testDistribution(d$Ynorm, "normal")
#' testDistribution(c(d$Ynorm, 10, 1000), "normal",
#'   extremevalues = "theoretical")
#' testDistribution(c(d$Ynorm, 10, 1000), "normal",
#'   extremevalues = "theoretical", robust = TRUE)
#'
#' testDistribution(mtcars, "mvnormal")
#'
#' ## for multivariate normal mahalanobis distance
#' ## which follows a chi-square distribution, extreme values only on
#' ## the right tail
#' testDistribution(mtcars, "mvnormal", extremevalues = "empirical",
#'   ev.perc = .1)
#' testDistribution(mtcars, "mvnormal", extremevalues = "theoretical",
#'   ev.perc = .1)
#'
#' rm(d) ## cleanup
#' }
testDistribution.default <- function(x,
  distr = c("normal", "beta", "chisq", "f", "gamma", "nbinom", "poisson", "mvnormal"),
  na.rm = TRUE, starts,
  extremevalues = c("no", "theoretical", "empirical"),
  ev.perc = .005,
  use = c("complete.obs", "pairwise.complete.obs", "fiml"),
  robust = FALSE, ...) {

  distr <- match.arg(distr)
  use <- match.arg(use)
  if (use != "complete.obs" & isTRUE(robust)) {
    use <- "complete.obs"
    message("use set to 'complete.obs' as robust = TRUE")
  }

  extremevalues <- match.arg(extremevalues)
  stopifnot(ev.perc %gele% c(0L, 1L))

  if (identical(distr, "mvnormal")) {
    if (anyNA(x)) {
      OK <- rowSums(is.na(x)) == 0
    } else if (!anyNA(x)) {
      use <- "complete.obs"
      OK <- rep(TRUE, nrow(x))
    }

    optargs <- list(...)
    if (all(c("mu", "sigma") %in% names(optargs))) {
      desc <- list(mu = optargs$mu,
                   sigma = optargs$sigma)
    } else {

      if (isTRUE(robust)) {
        tmp <- covMcd(x[OK, , drop=FALSE])
        desc <- list(mu = tmp$center, sigma = tmp$cov)
        rm(tmp)
      } else {
        desc <- switch(
          match.arg(use),
          fiml = {moments(x)},
          pairwise.complete.obs = {
            list(mu = colMeans(x, na.rm = TRUE),
                 sigma = cov(x, use = "pairwise.complete.obs"))
          },
          complete.obs = {
            list(mu = colMeans(x[OK,]),
                 sigma = cov(x[OK,]))
          })
      }
    }

    ## if any variances near zero, set to at least 1e-10
    if (any(diag(desc$sigma) < 1e-10)) {
      diag(desc$sigma) <- pmax(diag(desc$sigma), 1e-10)
    }
    ## if covariance matrix is singular, inflate diagonal (variances)
    ## 2.5 percent increase per iteration, up to 20 iterations
    i <- 1L
    while(isTRUE(tryCatch(solve(desc$sigma), error = function(e) TRUE)) && i <= 20) {
      diag(desc$sigma) <- diag(desc$sigma) * 1.025
      i <- i + 1L
    }

    starts <- list(df = ncol(x))
    x <- mahalanobis(x[OK,], desc$mu, desc$sigma)
  }

  if (missing(starts)) {
    base <- "starts must be a named list as below with start values for 'XX':\n%s"
    switch(distr,
           beta = stop(sprintf(base, "list(shape1 = XX, shape2 = XX)")),
           chisq = stop(sprintf(base, "list(df = XX)")),
           f = stop(sprintf(base, "list(df1 = XX, df2 = XX)")),
           "")
  }

  if (anyNA(x)) {
    if (na.rm) {
      if (!is.null(dim(x))) {
        OKindex <- rowSums(is.na(x)) == 0
      } else {
        OKindex <- !is.na(x)
      }
      x <- x[OKindex]
      } else {
        stop("Missing values cannot be present when na.rm = FALSE")
      }
    } else {
      OKindex <- rep(TRUE, length(x))
    }

  if (identical(distr, "normal") & isTRUE(robust)) {
    estimate <- covMcd(x)
    estimate <- c(
      mean = as.vector(estimate$center),
      sd = sqrt(as.vector(estimate$cov)))

    distribution <- list(
      d = dnorm,
      q = qnorm,
      Name = "Normal",
      fit =  structure(list(
        estimate = estimate,
        sd = c(mean = NA_real_, sd = NA_real_),
        vcov = structure(c(NA_real_, NA_real_, NA_real_, NA_real_),
                         .Dim = c(2L, 2L),
                         .Dimnames = list(c("mean", "sd"), c("mean", "sd"))),
        n = length(x),
        loglik = sum(dnorm(x, estimate["mean"], estimate["sd"],
                           log = TRUE))), class = "fitdistr"))
    rm(estimate)
  } else {
    distribution <- switch(
      distr,
      normal = list(
        d = dnorm,
        q = qnorm,
        Name = "Normal",
        fit = fitdistr(x, "normal")),
      beta = list(
        d = dbeta,
        q = qbeta,
        Name = "Beta",
        fit = fitdistr(x, "beta", start = starts)),
      chisq = list(
        d = dchisq,
        q = qchisq,
        Name = "Chi-squared",
        fit = fitdistr(x, "chi-squared", start = starts, lower = .01)),
      f = list(
        d = df,
        q = qf,
        Name = "F",
        fit = fitdistr(x, "f", start = starts)),
      gamma = list(
        d = dgamma,
        q = qgamma,
        Name = "Gamma",
        fit = fitdistr(x, "gamma", lower = c(.001, .001))),
      nbinom = list(
        d = dnbinom,
        q = qnbinom,
        Name = "Negative Binomial",
        fit = fitdistr(x, "negative binomial")),
      poisson = list(
        d = dpois,
        q = qpois,
        Name = "Poisson",
        fit = fitdistr(x, "poisson")),
      mvnormal = list(
        d = dchisq,
        q = qchisq,
        Name = "Chi-squared",
        fit = list(estimate = list(df = starts$df))))
  }

  if (!identical(distr, "mvnormal")) {
    distribution$LL <- logLik(distribution$fit)
  } else {
    distribution$LL <- structure(NA_real_, nobs = length(x),
                                 df = starts$df, class = "logLik")
  }

  d <- data.table(
    X = do.call(distribution$q,
                c(list(p = ppoints(length(x))),
                  as.list(distribution$fit$estimate))),
    Y = sort(x),
    OriginalOrder = which(OKindex)[order(x)])

  ev.limits <- switch(extremevalues,
    no = c(-Inf, Inf),
    empirical = if (!identical(distr, "mvnormal") && !identical(distr, "chisq")) {
                  quantile(x, probs = c(ev.perc, 1 - ev.perc), na.rm = TRUE)
                } else {
                  c(-Inf, quantile(x, probs = 1 - ev.perc, na.rm = TRUE))
                },
    theoretical = do.call(distribution$q,
      c(list(
        p = if (!identical(distr, "mvnormal") && !identical(distr, "chisq")) {
              c(ev.perc, 1 - ev.perc)
            } else {
              c(0, 1 - ev.perc)
            }),
        as.list(distribution$fit$estimate))))

  d[, isEV := factor(as.numeric(Y) %gele% ev.limits,
                     levels = c(TRUE, FALSE), labels = c("No", "Yes"))]

  nok <- sum(!is.na(d$Y))

  d$YDeviates <- resid(lm(Y ~ 0 + offset(X), data = d))

  as.testDistribution(list(
    Data = d,
    Distribution = distribution,
    EVLimits = ev.limits,
    NOK = nok,
    distr = distr,
    na.rm = na.rm,
    extremevalues = extremevalues,
    ev.perc = ev.perc,
    use = use,
    robust = robust))
}

#' @title Residual Diagnostics Functions
#' @description A set of functions to calculate
#'   residual diagnostics on models, including constructors,
#'   a generic function, a test of whether an object is of the
#'   \code{residualDiagnostics} class, and methods.
#' @param object A fitted model object, with methods for
#'   \code{model.frame}, \code{resid} and \code{fitted}.
#' @param ev.perc A real number between 0 and 1 indicating the
#'   proportion of the theoretical distribution beyond which
#'   values are considered extreme values (possible outliers).
#'   Defaults to .001.
#' @param robust Whether to use robust mean and standard deviation estimates
#'   for normal distribution
#' @param distr A character string given the assumed distribution.
#'   Passed on to \code{\link{testDistribution}}.
#'   Defaults to \dQuote{normal}.
#' @param standardized A logical whether to use standardized residuals.
#'   Defaults to \code{TRUE} generally where possible but may depend on
#'   method.
#' @param ... Additional arguments, not currently used.
#' @return A logical (\code{is.residualDiagnostics}) or
#'   a residualDiagnostics object (list) for
#'   \code{as.residualDiagnostics} and \code{residualDiagnostics}.
#' @export
#' @rdname residualDiagnostics
residualDiagnostics <- function(object, ...) {
  UseMethod("residualDiagnostics", object)
}

#' @param x A object (e.g., list or a modelDiagnostics object) to
#'   test or attempt coercing to a residualDiagnostics object.
#' @importFrom data.table is.data.table as.data.table
#' @rdname residualDiagnostics
#' @export
as.residualDiagnostics <- function(x) {
  if (!is.residualDiagnostics(x)) {
    if(!is.list(x)) {
      stop("Input must be a list or a residualDiagnostics object")
    }
    stopifnot(identical(length(x), 5L))
    augmentClass <- attr(x, "augmentClass")

    x <- list(
      Residuals = x[[1]],
      Frame = x[[2]],
      Hat = x[[3]],
      testDistribution = x[[4]],
      Outcome = x[[5]],
      N = nrow(x[[1]]),
      K = ncol(x[[2]]))

    if(is.null(augmentClass)) {
      class(x) <- "residualDiagnostics"
    } else {
      class(x) <- c(paste0("residualDiagnostics.", augmentClass), "residualDiagnostics")
    }
  }

  ## checks that the object is not malformed
  stopifnot(identical(length(x), 7L))
  stopifnot(identical(
    names(x),
    c("Residuals", "Frame", "Hat",
      "testDistribution", "Outcome", "N", "K")))
  stopifnot(is.character(x$Outcome))
  stopifnot(is.data.table(x$Residuals))
  stopifnot(is.data.table(x$Frame))
  stopifnot(is.testDistribution(x$testDistribution))
  stopifnot(identical(nrow(x$Residuals), nrow(x$Frame)))
  stopifnot(identical(ncol(x$Residuals), 4L))
  stopifnot(x$Outcome %in% names(x$Frame))

  return(x)
}

#' @rdname residualDiagnostics
#' @export
is.residualDiagnostics <- function(x) {
  inherits(x, "residualDiagnostics")
}

## clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("originalindex"))

#' @importFrom stats rstandard residuals fitted coef predict model.frame
#' @importFrom data.table data.table :=
#' @method residualDiagnostics lm
#' @export
#' @rdname residualDiagnostics
#' @examples
#' testm <- stats::lm(mpg ~ hp * factor(cyl), data = mtcars)
#'
#' resm <- residualDiagnostics(testm)
#' plot(resm$testDistribution)
#'
#' resm <- residualDiagnostics(testm, standardized = FALSE)
#' plot(resm$testDistribution)
#'
#' ## clean up
#' rm(testm, resm)
#' \dontrun{
#'
#' testdat <- data.frame(
#'   y = c(1, 2, 2, 3, 3, NA, 9000000, 2, 2, 1),
#'   x = c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2))
#'
#' residualDiagnostics(
#'   lm(y ~ x, data = testdat, na.action = "na.omit"),
#'   ev.perc = .1)$Residuals
#'
#' residualDiagnostics(
#'   lm(y ~ x, data = testdat, na.action = "na.exclude"),
#'   ev.perc = .1)$Residuals
#'
#' residualDiagnostics(
#'   lm(sqrt(mpg) ~ hp, data = mtcars, na.action = "na.omit"),
#'   ev.perc = .1)$Residuals
#' }
residualDiagnostics.lm <- function(object, ev.perc = .001,
                                   robust = FALSE, distr = "normal",
                                   standardized = TRUE, ...) {
  d.frame <- model.frame(object)
  naaction <- attr(d.frame, "na.action")
  if (isFALSE(is.null(naaction))) {
    if (isTRUE(inherits(naaction, "omit"))) {
      origindex <- index <- 1:(nrow(d.frame) + length(naaction))
      index[naaction] <- NA
      index[-naaction] <- 1:nrow(d.frame)
      key <- data.table(
        originalindex = origindex,
        index = index)[!is.na(index)]
    }
  } else {
    key <- data.table(
      originalindex = 1:nrow(d.frame),
      index = 1:nrow(d.frame))[!is.na(index)]
  }

  d.frame <- as.data.table(d.frame)
  dv <- names(d.frame)[1]

  d.res <- data.table(
    Residuals = if (standardized) rstandard(object) else residuals(object),
    Predicted = fitted(object))

  d.hat <- .quantilePercentiles(
    data = d.res,
    LL = .1, UL = .9)

  d.dist <- testDistribution(
    x = d.res$Residuals,
    distr = "normal",
    na.rm = TRUE,
    extremevalues = "theoretical",
    ev.perc = ev.perc,
    use = "complete.obs",
    robust = robust)

  d.res[!is.na(Residuals), isEV := d.dist$Data[order(OriginalOrder), isEV]]
  d.res[, Index := 1:.N]

  ## fix the index to match original data if missing data existed and were omitted
  if (isFALSE(is.null(naaction))) {
    if (isTRUE(inherits(naaction, "omit"))) {
      d.res[, Index := key[, originalindex]]
      d.dist$Data[, OriginalOrder := key[match(OriginalOrder, index), originalindex]]
    }
  }

  out <- list(na.omit(d.res), d.frame, d.hat, d.dist, dv)
  attr(out, "augmentClass") <- "lm"

  as.residualDiagnostics(out)
}

#' Internal Function to Calculate Quantiles
#'
#' Function calculates smoothing spline quantiles
#' or linear quantiles as a fall back. Not intended for general use.
#' Expected predicted and residual data.
#' Exported to support related packages.
#'
#' @param data A dataset of predicted and residual values.
#'   Assumed from some sort of (probably parametric) model.
#' @param LL The lower limit for prediction. Defaults to
#'   \code{.1} to give the 10th percentile.
#' @param UL The upper limit for prediction. Defaults to
#'   \code{.9} to give the 90th percentile.
#' @param na.rm A logical whether to remove missing values.
#'   Defaults to \code{TRUE}
#' @return A data.table with the scores and predicted LL and UL,
#'   possibly missing if quantile regression models do not
#'   converge.
#' @importFrom quantreg qss rq rqss
#' @importFrom data.table data.table :=
#' @export
.quantilePercentiles <- function(data, LL = .1, UL = .9, na.rm = TRUE) {
  d.hat <- data.table(
    Predicted = seq(
      min(data$Predicted, na.rm = na.rm),
      max(data$Predicted, na.rm = na.rm),
      length.out = 1000))

  tau.LL <- tryCatch(
    rqss(Residuals ~ qss(Predicted, lambda = 1),
         tau = LL, data = data),
    error = function(e) TRUE)
  if (!isTRUE(tau.LL)) {
    tau.UL <- tryCatch(
      rqss(Residuals ~ qss(Predicted, lambda = 1),
           tau = UL, data = data),
      error = function(e) TRUE)
  } else {
    tau.UL <- TRUE
  }
  if (!isTRUE(tau.LL) && !isTRUE(tau.UL)) {
    d.hat[, LL := predict(tau.LL, d.hat)]
    d.hat[, UL := predict(tau.UL, d.hat)]
  }
  if (isTRUE(tau.LL) || isTRUE(tau.UL) ||
      isTRUE(all.equal(d.hat$LL, d.hat$UL))) {
    tau.2LL <- tryCatch(
      rq(Residuals ~ Predicted, tau = LL, data = data),
      error = function(e) TRUE)
    if (!isTRUE(tau.2LL)) {
      tau.2UL <- tryCatch(
        rq(Residuals ~ Predicted, tau = UL, data = data),
        error = function(e) TRUE)
    } else {
      tau.2UL <- TRUE
    }
    if (!isTRUE(tau.2LL) && !isTRUE(tau.2UL)) {
      d.hat[, LL := predict(tau.2LL, d.hat)]
      d.hat[, UL := predict(tau.2UL, d.hat)]
    }
  }
  if (isTRUE(all.equal(d.hat$LL, d.hat$UL))) {
    d.hat[, LL := NA_real_]
    d.hat[, UL := NA_real_]
  }
  return(d.hat)
}


#' @title Model Diagnostics Functions
#' @description A set of functions to calculate
#'   model diagnostics on models, including constructors,
#'   a generic function, a test of whether an object is of the
#'   \code{modelDiagnostics} class, and methods.
#' @param object A fitted model object, with methods for
#'   \code{model.frame}, \code{resid} and \code{fitted}.
#' @param x An object to test or a list to coerce to a
#'   \code{modelDiagnostics} object.
#' @param ev.perc A real number between 0 and 1 indicating the
#'   proportion of the theoretical distribution beyond which
#'   values are considered extreme values (possible outliers).
#'   Defaults to .001.
#' @param robust Whether to use robust mean and standard deviation estimates
#'   for normal distribution
#' @param distr A character string given the assumed distribution.
#'   Passed on to \code{\link{testDistribution}}.
#'   Defaults to \dQuote{normal}.
#' @param standardized A logical whether to use standardized residuals.
#'   Defaults to \code{TRUE} generally where possible but may depend on
#'   method.
#' @param ... Additional arguments, not currently used.
#' @return A logical (\code{is.modelDiagnostics}) or
#'   a modelDiagnostics object (list) for
#'   \code{as.modelDiagnostics} and \code{modelDiagnostics}.
#' @export
#' @rdname modelDiagnostics
modelDiagnostics <- function(object, ...) {
  UseMethod("modelDiagnostics", object)
}


#' @importFrom data.table is.data.table as.data.table
#' @export
#' @rdname modelDiagnostics
as.modelDiagnostics <- function(x) {
  stopifnot(identical(length(x), 3L))
  if (!is.modelDiagnostics(x)) {
    if(!is.list(x)) {
      stop("Input must be a list or a modelDiagnostics object")
    }
    augmentClass <- attr(x, "augmentClass")
    x <- list(
      residualDiagnostics = x[[1]],
      modelDiagnostics = x[[2]],
      extremeValues = x[[3]])
    if(is.null(augmentClass)) {
      class(x) <- "modelDiagnostics"
    } else {
      class(x) <- c(paste0("modelDiagnostics.", augmentClass), "modelDiagnostics")
    }
  }

  stopifnot(identical(length(x), 3L))
  stopifnot(identical(names(x),
                      c("residualDiagnostics",
                        "modelDiagnostics",
                        "extremeValues")))

  stopifnot(is.residualDiagnostics(
    x$residualDiagnostics))

  return(x)
}

#' @export
#' @rdname modelDiagnostics
is.modelDiagnostics <- function(x) {
  inherits(x, "modelDiagnostics")
}

#' @export
#' @rdname modelDiagnostics
#' @examples
#' testm <- stats::lm(mpg ~ hp * factor(cyl), data = mtcars)
#'
#' md <- modelDiagnostics(testm)
#' plot(md$residualDiagnostics$testDistribution)
#' md$extremeValues
#'
#' plot(md)
#'
#' md <- modelDiagnostics(testm, ev.perc = .1)
#' md$extremeValues
#' plot(md, ncol = 2)
#'
#' testdat <- data.frame(
#'   y = c(1, 2, 2, 3, 3, NA, 9000000, 2, 2, 1),
#'   x = c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2))
#'
#' modelDiagnostics(
#'   lm(y ~ x, data = testdat, na.action = "na.omit"),
#'   ev.perc = .1)$extremeValues
#'
#' modelDiagnostics(
#'   lm(y ~ x, data = testdat, na.action = "na.exclude"),
#'   ev.perc = .1)$extremeValues
#'
#' ## clean up
#' rm(testm, md, testdat)
modelDiagnostics.lm <- function(object, ev.perc = .001,
                                   robust = FALSE, distr = "normal",
                                   standardized = TRUE, ...) {

  x <- residualDiagnostics(object,
                           ev.perc = ev.perc,
                           robust = robust,
                           distr = distr,
                           standardized = standardized)

  ## data for outliers
  d.extreme <- data.table(dv = NA_real_,
                          Index = NA_integer_,
                          EffectType = NA_character_)
  setnames(d.extreme, names(d.extreme), c(x$Outcome, "Index", "EffectType"))
  if ("EffectType" %in% x$Outcome) {
    stop("EffectType is used internally and cannot be a variable in the model")
  }

  x$Frame <- cbind(
    x$Frame, x$testDistribution$Data[order(OriginalOrder), .(isEV, OriginalOrder)])
  setnames(x$Frame, "OriginalOrder", "Index")
  x$Frame[, EffectType := "Residuals"]

  if (isTRUE(any(x$Frame[, isEV] == "Yes"))) {
    d.extreme <- rbind(
      d.extreme,
      x$Frame[isEV == "Yes",
              c(x$Outcome, "Index", "EffectType"),
              with = FALSE])

  }

  ## fix the frame to remove extra information
  x$Frame[, isEV := NULL]
  x$Frame[, Index := NULL]
  x$Frame[, EffectType := NULL]

  out <- list(x, NA_real_, na.omit(d.extreme))
  attr(out, "augmentClass") <- "lm"

  as.modelDiagnostics(out)
}

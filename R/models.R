##' Internal function to run a model using gam()
##'
##' This function is not intended to be called by users.
##'
##' @param formula A character string containing a formula style object.
##' @param type  A character string indicating the type of dependent variable.
##'   Currently \dQuote{normal}, \dQuote{binary}, or \dQuote{count}.
##' @param data A data frame to be used for analysis.
##' @param \dots Additional arguments passed to \code{gam}.
##' @return A summary of the gam model.
##' @importFrom stats as.formula gaussian binomial poisson
##' @importFrom mgcv gam summary.gam
##' @keywords internal
internalrunIt <- function(formula, type, data, ...) {
  summary(gam(formula = as.formula(formula), data = data,
              family = switch(type,
                              normal = gaussian(),
                              binary = binomial(),
                              count = poisson()),
              ...))
}

##' Internal function to create a formula
##'
##' This function is not intended to be called by users.
##' It creates a formula style character string from its argument.
##' But note that it does not actually create a formula class object.
##' If you do not want an argument, use the empty string.
##'
##' @param dv A character string of the dependent variable.
##' @param iv A character string or vector of the independent variables
##' @param covariates A character string or vector of the dependent variables
##' @return A character string
##' @keywords internal
##' @examples
##' JWileymisc:::internalformulaIt("mpg", "hp", "am")
##' JWileymisc:::internalformulaIt("mpg", "hp", "")
##' JWileymisc:::internalformulaIt("mpg", "", "am")
internalformulaIt <- function(dv, iv, covariates) {
  type <- paste0(
    c("", "iv")[(length(iv) & any(nzchar(iv))) + 1],
    c("", "cov")[(length(covariates) & any(nzchar(covariates))) + 1])

  switch(type,
         ivcov = sprintf("%s ~ %s + %s", dv,
                         paste(iv, collapse = " + "),
                         paste(covariates, collapse = " + ")),
         iv = sprintf("%s ~ %s", dv, paste(iv, collapse = " + ")),
         cov = sprintf("%s ~ %s", dv, paste(covariates, collapse = " + ")))
}

##' Compares the effects of various independent variables
##'
##' This is an internal function designed to run many models to compare the
##' unique predictive effect of different IVs with and without covariates on an
##' outcome.
##'
##' @param dv A character string of the depentent variable
##' @param type A character string indicating the type of dependent variable
##' @param iv A character string or vector giving the IV(s)
##' @param covariates A character string or vector giving the covariate(s)
##' @param data The data to be used for analysis
##' @param multivariate A logical value whether to have models with all IVs simultaneously.
##' @param \ldots Additional arguments passed on to the internal function, \code{.runIt}.
##' @return A list with all the model results.
##' @keywords internal
##' @import foreach
##' @importFrom stats na.omit get_all_vars
##' @examples
##' test1 <- JWileymisc:::internalcompareIV(
##'   dv = "mpg", type = "normal",
##'   iv = "hp",
##'   covariates = "am",
##'   data = mtcars, multivariate = FALSE)
##' test1$Summary
##' rm(test1)
internalcompareIV <- function(dv, type = c("normal", "binary", "count"),
                       iv, covariates = character(), data, multivariate = FALSE, ...) {

  if (length(iv) <= 1 & multivariate) {
    stop("Cannot use multivariate = TRUE when only a single IV")
  }
  nIV <- length(iv)

  if (length(covariates)) {
    f.cov <- internalformulaIt(dv, "", covariates)
  }
  data <- data
  if (multivariate) {
    message("Multivariate uses complete cases for all IVs and covariates")

    f.all <- internalformulaIt(dv, iv, covariates)

    data <- na.omit(get_all_vars(as.formula(f.all), data = data))

    m.all <- internalrunIt(f.all, type, data = data, ...)

    if (length(covariates)) {
      m.all.cov <- internalrunIt(f.cov, type, data = data, ...)
    }
  }

  i <- NULL; rm(i) ## make Rcmd check happy
  results <- foreach(i = 1:nIV, .combine = list) %dopar% {
    if (multivariate) {
      out <- list(
        data = data,
        Unadjusted = internalrunIt(internalformulaIt(dv, iv[i], ""),
                            type = type, data = data,
                            ...),
        Covariate = m.all.cov,
        Adjusted = if (!length(covariates)) {
                     NA
                   } else {
                     internalrunIt(internalformulaIt(dv, iv[i], covariates),
                            type = type, data = data,
                            ...)
                   },
        Reduced =  internalrunIt(internalformulaIt(dv, iv[-i], covariates),
                          type = type, data = data,
                          ...),
        Full = m.all)
      out$Summary <- data.frame(
        Type = c("Unadjusted", "Adjusted", "MultiUnique"),
        R2 = c(out$Unadjusted$r.sq,
               ifelse(is.na(out$Adjusted)[1], NA,
                      out$Adjusted$r.sq - out$Covariate$r.sq),
               ifelse(is.na(out$Reduced)[1], NA,
                      out$Full$r.sq - out$Reduced$r.sq)),
        D = c(out$Unadjusted$dev.expl,
              ifelse(is.na(out$Adjusted)[1], NA,
                     out$Adjusted$dev.expl - out$Covariate$dev.expl),
              ifelse(is.na(out$Reduced)[1], NA,
                     out$Full$dev.expl - out$Reduced$dev.expl)))
      out
    } else {
      tmpdata <- na.omit(get_all_vars(as.formula(
        internalformulaIt(dv, iv[i], covariates)), data = data))

      out <- list(
        data = tmpdata,
        Unadjusted = internalrunIt(internalformulaIt(dv, iv[i], ""),
                            type = type, data = tmpdata,
                            ...),
        Covariate = if (!length(covariates)) {
                      NA
                    } else {
                      internalrunIt(internalformulaIt(dv, "", covariates),
                             type = type, data = tmpdata,
                             ...)
                    },
        Adjusted = if (!length(covariates)) {
                     NA
                   } else {
                     internalrunIt(internalformulaIt(dv, iv[i], covariates),
                            type = type, data = tmpdata,
                            ...)
                   },
        Reduced = NA,
        Full = NA)
      out$Summary <- data.frame(
        Type = c("Unadjusted", "Adjusted", "MultiUnique"),
        R2 = c(out$Unadjusted$r.sq,
               ifelse(is.na(out$Adjusted)[1], NA,
                      out$Adjusted$r.sq - out$Covariate$r.sq),
               ifelse(is.na(out$Reduced)[1], NA,
                      out$Full$r.sq - out$Reduced$r.sq)),
        Deviance = c(out$Unadjusted$dev.expl,
              ifelse(is.na(out$Adjusted)[1], NA,
                     out$Adjusted$dev.expl - out$Covariate$dev.expl),
              ifelse(is.na(out$Reduced)[1], NA,
                     out$Full$dev.expl - out$Reduced$dev.expl)))
      out
    }
  }
  if (length(results) != nIV & nIV == 1) {
    results <- list(results)
  }
  return(results)
}


#' Compares the effects of various independent variables on dependent variables
#'
#' Utility to estimate the unadjusted, covariate adjusted, and multivariate adjusted
#' unique contributions of one or more IVs on one or more DVs
#'
#' @param dv A character string or vector of the depentent variable(s)
#' @param type A character string or vector indicating the type of dependent variable(s)
#' @param iv A character string or vector giving the IV(s)
#' @param covariates A character string or vector giving the covariate(s)
#' @param data The data to be used for analysis
#' @param multivariate A logical value whether to have models with all IVs simultaneously.
#' @param \ldots Additional arguments passed on to the internal function, \code{.runIt}.
#' @return A list with all the model results.
#' @export
#' @examples
#' test1 <- compareIVs(
#'   dv = c("mpg", "disp"),
#'   type = c("normal", "normal"),
#'   iv = c("hp", "qsec"),
#'   covariates = "am",
#'   data = mtcars, multivariate = TRUE)
#' test1$OverallSummary
#' rm(test1)
compareIVs <- function(dv, type, iv, covariates = character(), data, multivariate = FALSE, ...) {
  stopifnot(identical(length(dv), length(type)))

  res <- lapply(seq_along(dv), function(i) {
    internalcompareIV(
      dv = dv[i], type = type[i],
      iv = iv,
      covariates = covariates,
      data = data,
      multivariate = multivariate,
      ...)
  })

  res$OverallSummary <- do.call(rbind, lapply(seq_along(dv), function(x) {
    do.call(rbind, lapply(seq_along(iv), function(y) {
      cbind.data.frame(dv = dv[x], iv = iv[y], res[[x]][[y]]$Summary, stringsAsFactors = FALSE)
    }))
  }))

  return(res)
}

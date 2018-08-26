# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("V2", "Index"))


##' Function to find significant regions from an interaction
##'
##' This function uses the \code{contrast} function from \pkg{rms} to
##' find the threshold for significance from interactions.
##'
##'
##' @param object A fitted rms object
##' @param l1 the first set of values to fix for the contrast function
##' @param l2 the second set of values to fix for the contrast function
##' @param name.vary the name of the model parameter to vary values for
##'   to find the threshold.  Note that this should not be included in
##'   \code{l1} or \code{l2} arguments.
##' @param lower The lower bound to search for values for the varying value
##' @param upper The upper bound to search for values for the varying value
##' @param alpha The significance threshold, defaults to \code{.05}
##' @param starts Number of starting values to try between the
##'   lower and upper bounds.
##' @return A data table with notes if no convergence or significance
##'   thresholds (if any).
##' @export
##' @importFrom rms contrast
##' @importFrom stats optim
##' @examples
##' ## make me
findSigRegions <- function(object, l1, l2, name.vary, lower, upper, alpha = .05, starts = 50) {
  foo <- function(x) {
    tmp1 <- l1
    tmp2 <- l2
    tmp1[name.vary] <- x
    tmp2[name.vary] <- x

    out <- abs(alpha - as.data.table(as.data.frame(contrast(object, tmp1, tmp2)[c("Pvalue")]))[, Pvalue])

    if (is.na(out) || is.nan(out) || !is.finite(out)) {
      out <- 9
    }

    return(out)
  }

  fstarts <- function(startval) {
    res <- optim(par = startval, fn = foo,
                 lower = lower, upper = upper,
                 method = "L-BFGS-B",
                 control = list(factr = 1e11, maxit = 500))
    conv <- valid <- sig <- FALSE

    out <- data.table(A = as.numeric(startval), Contrast = NA_real_, Pvalue = NA_real_, Notes = NA_character_)
    setnames(out, names(out), c(name.vary, "Contrast", "Pvalue", "Notes"))

    if (isTRUE(all.equal(res$convergence, 0))) {
      conv <- TRUE
      out$Notes <- "Converged"
      if (res$value > (-.Machine$double.eps) & res$value < (.95 + .Machine$double.eps)) {
        valid <- TRUE
        out$Notes <- paste(out$Notes, "Valid", sep = "; ")

        if (res$value < .0001) {
          sig <- TRUE

          tmp1 <- l1
          tmp1[name.vary] <- res$par
          tmp2 <- l2
          tmp2[name.vary] <- res$par
          out <- cbind(
            as.data.table(as.data.frame(contrast(object, tmp1, tmp2)[c(name.vary, "Contrast", "Pvalue")])),
            Notes = out$Notes)
        }
      } else {
        out$Notes <- paste(out$Notes, "Invalid result", sep = "; ")
      }
    } else {
      out$Notes <- paste("Did not converge", res$convergence, res$message, sep = "; ")
    }
    return(out)
  }

  out <- do.call(rbind, lapply(seq(from = lower, to = upper, length.out = starts), fstarts))

  if (any(!is.na(out$Pvalue))) {
    out <- out[!is.na(Pvalue)]
  }

  out[, V2 := round(get(name.vary) / abs(lower - upper), 3)]
  out <- out[order(V2, -abs(alpha - Pvalue))]
  out[, Index := 1:.N, by = V2]

  out[Index == 1][, Index := NULL][, V2 := NULL]
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
           "Pvalue", "xz", "yz", "yhat", "yllz",
           "lower", "yulz", "upper", "reglab", "Contrast",
           "Lower", "Upper", "x", "ContrastAngle",
           "ContrastAngleZ", "Yhat"))
}

##' Function to find significant regions from an interaction
##'
##' This function uses the \code{contrast} function from \pkg{rms} to
##' find the threshold for significance from interactions.
##'
##' @param object A fitted rms object
##' @param predList TODO
##' @param contrastList TODO
##' @param xvar TODO
##' @param varyvar TODO
##' @param varyvar.levels TODO
##' @param xlab optional
##' @param ylab TODO
##' @param ratio TODO
##' @param xlim TODO
##' @param ylim TODO
##' @param xbreaks TODO
##' @param xlabels optional
##' @param scale.x optional
##' @param scale.y optional
##' @param starts Number of starting values to try between the
##'   lower and upper bounds.
##' @return A data table with notes if no convergence or significance
##'   thresholds (if any).
##' @export
##' @importFrom rms Predict
##' @examples
##' ## make me
intSigRegGraph <- function(object, predList, contrastList, xvar, varyvar,
                           varyvar.levels,
                           xlab = xvar, ylab = "Predicted Values", ratio = 1,
                           xlim, ylim,
                           xbreaks, xlabels = xbreaks,
                           scale.x = c(m = 0, s = 1), scale.y = c(m = 0, s = 1),
                           starts = 50) {

  preds <- as.data.table(do.call(Predict, list(x = object, factors = predList, conf.type = "mean")))
  preds[, xz := (get(xvar) - scale.x["m"])/scale.x["s"]]
  preds[, yz := (yhat - scale.y["m"])/scale.y["s"]]
  preds[, yllz := (lower - scale.y["m"])/scale.y["s"]]
  preds[, yulz := (upper - scale.y["m"])/scale.y["s"]]

  simpleSlopes <- do.call(rbind, lapply(contrastList, function(x) {
    out <- as.data.table(as.data.frame(
      contrast(object,
               c(x[[-1]], x[[1]][1]), c(x[[-1]], x[[1]][2]), type = "average")[
        c("Contrast", "SE", "Lower", "Upper", "Pvalue")]))
    out[, reglab := sprintf(
            "b = %0.2f [%0.2f, %0.2f], %s",
            Contrast, Lower, Upper,
            formatPval(Pvalue, 3, 3, includeP = TRUE))]
    return(out)
  }))

  yvals <- preds[, .(y = yhat[which.min(abs(get(xvar) - pmax(min(get(xvar), na.rm = TRUE), min(xlim))))],
                     yz = yz[which.min(abs(xz - pmax(min(xz, na.rm = TRUE), min(xlim))))]),
                     by = varyvar]
  yvals2 <- preds[,
                 .(y = yhat[which.min(abs(get(xvar) - pmin(max(get(xvar), na.rm = TRUE), max(xlim))))],
                   yz = yz[which.min(abs(xz - pmin(max(xz, na.rm = TRUE), max(xlim))))]),
                 by = varyvar]

  if ((abs(diff(yvals$yz))/abs(diff(yvals2$yz)) < .9)) {
    use.xmax <- TRUE
    simpleSlopes[, x := pmin(max(preds[[xvar]], na.rm = TRUE), max(xlim))]
    simpleSlopes[, xz := pmin(max(preds[["xz"]], na.rm = TRUE), max(xlim))]
    simpleSlopes <- cbind(simpleSlopes, yvals2)
  } else {
    use.xmax <- FALSE
    simpleSlopes[, x := pmax(min(preds[[xvar]], na.rm = TRUE), min(xlim))]
    simpleSlopes[, xz := pmax(min(preds[["xz"]], na.rm = TRUE), min(xlim))]
    simpleSlopes <- cbind(simpleSlopes, yvals)
  }

  simpleSlopes[, ContrastAngle := atan(Contrast * ratio)/(pi/180)]
  simpleSlopes[, ContrastAngleZ := atan(Contrast * ratio * scale.x["s"])/(pi/180)]

  sigThresh <- findSigRegions(object, contrastList[[1]][[2]], contrastList[[2]][[2]],
                               name.vary = xvar,
                               lower = min(preds[[xvar]], na.rm = TRUE),
                               upper = max(preds[[xvar]], na.rm = TRUE),
                              starts = starts)
  sigThresh[, xz := (get(xvar) - scale.x["m"])/scale.x["s"]]

  anysig <- any(!is.na(sigThresh$Pvalue))

  if (anysig) {
    sigThreshArrows <- lapply(1:nrow(sigThresh), function(i) {
      z <- sigThresh[i, xz]
      merge(cbind(preds[, .(Yhat = yhat[which.min(abs(xz - z))]), by = varyvar],
                  xz = z), sigThresh[i], by = "xz", all = TRUE)
    })
  } else {
    sigThreshArrows <- list(NA)
  }

  if (missing(varyvar.levels)) {
    varyvar.levels <- list(levels = unique(preds[[varyvar]]), labels = unique(preds[[varyvar]]))
  }

  preds[, (varyvar) := factor(get(varyvar), levels = varyvar.levels$levels, labels = varyvar.levels$labels)]

  finalOut <- list(
    Predictions = preds,
    simpleSlopes = simpleSlopes,
    significantThresholds = sigThresh,
    significantThresholdArrows = sigThreshArrows)

  if (anysig) {
    arrow.geoms <- lapply(sigThreshArrows, function(arr) {
      geom_segment(aes(x = xz[1], y = Yhat[1], xend = xz[2], yend = Yhat[2]),
                   data = arr,
                   size=.6, arrow = arrow(length = unit(.03, "npc"), ends = "both"))
    })
  } else {
    arrow.geoms <- list(NA)
  }

  base.code <- paste0('
  ggplot(Predictions, aes(xz, y = yhat)) +
    geom_ribbon(aes_string(ymin = "lower", ymax = "upper", group = \"', varyvar, '\"), alpha = .1) +
    geom_line(aes_string(linetype = \"', varyvar, '\"), size = 2) +
    geom_text(aes(x = xz, y = yz + ', (.05 * diff(ylim)), ', label = reglab, angle = ContrastAngleZ),
              data = simpleSlopes, hjust = ', use.xmax, ') +
    scale_x_continuous(breaks = ', deparse(xbreaks), ', labels = ', deparse(xlabels), ') +
    theme_cowplot() +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.title = element_blank(),
      legend.position = "bottom") +
    xlab(', deparse(xlab), ') +
    ylab(', deparse(ylab), ') +
    coord_fixed(ratio = ', ratio, ',
                xlim = ', deparse(xlim), ',
                ylim = ', deparse(ylim), ',
                expand = FALSE)')

  if (anysig) {
    p.code <- paste(c(base.code, sapply(1:length(sigThreshArrows), function(i) {
                  paste0('geom_segment(aes(x = xz[1], y = Yhat[1], xend = xz[2], yend = Yhat[2]),
                               data = significantThresholdArrows[[', i, ']],
                               size=.6, arrow = arrow(length = unit(.03, "npc"), ends = "both"))')})),
                  collapse = " + \n")
  } else {
    p.code <- base.code
  }

  p <- with(finalOut, eval(parse(text = p.code)))

  ## p <- ggplot(finalOut$Predictions, aes(xz, y = yhat)) +
  ##   geom_ribbon(aes_string(ymin = "lower", ymax = "upper", group = varyvar), alpha = .1) +
  ##   geom_line(aes_string(linetype = varyvar), size = 2) +
  ##   geom_text(aes(x = xz, y = yz + (.05 * diff(ylim)), label = reglab, angle = ContrastAngleZ),
  ##             data = simpleSlopes, hjust = use.xmax) +
  ##   scale_x_continuous(breaks = xbreaks, labels = xlabels) +
  ##   theme_cowplot() +
  ##   theme(
  ##     legend.key.width = unit(2, "cm"),
  ##     legend.title = element_blank(),
  ##     legend.position = "bottom") +
  ##   xlab(xlab) +
  ##   ylab(ylab) +
  ##   coord_fixed(ratio = ratio,
  ##               xlim = xlim,
  ##               ylim = ylim,
  ##               expand = FALSE)
  ## if (anysig) {
  ##   for (i in length(arrow.geoms)) {
  ##     p <- p + arrow.geoms[[i]]
  ##   }
  ## }

  finalOut$Graph <- p
  finalOut$GraphCode <- p.code

  return(finalOut)
}

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


#' Calculates all pairwise contrasts and omnibus tests for multinomial regression
#'
#' TODO: make me!
#'
#' @param formula A standard regression formula, passed to
#'   \code{vglm} from the \pkg{VGAM} package.
#' @param data A data frame (no other type currently supported)
#'   to be used for model fitting.
#' @param OR a logical value whether to report odds ratios and
#'   95 percent confidence intervals, if \code{TRUE}, or
#'   regression coefficients on the logit scale with standard
#'   errors, if \code{FALSE}.
#' @param digits An integer indicating the number of digits for coefficients,
#'   standard errors, and confidence intervals
#' @param pdigits An integer indicating the number of digits for
#'   p-values.
#' @return A list with two elements.
#'   \code{Results} contains a data table of the actual estimates.
#'   \code{Table} contains a nicely formatted character matrix.
#'
#' @export
#' @importFrom stats model.matrix vcov
#' @importFrom VGAM vglm multinomial summary
#' @importMethodsFrom VGAM vcov
#' @importFrom multcomp glht Chisqtest
#' @importFrom data.table data.table
#' @examples
#'
#' \dontrun{
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#'
#' ezMULTINOM(cyl ~ qsec, data = mtcars)
#' ezMULTINOM(cyl ~ qsec, data = mtcars, digits = 4)$Table
#' ezMULTINOM(cyl ~ qsec, data = mtcars, OR = FALSE)
#' ezMULTINOM(cyl ~ qsec, data = mtcars, digits = 4, OR = FALSE)$Table
#'
#' ezMULTINOM(cyl ~ scale(qsec), data = mtcars)
#'
#' ezMULTINOM(cyl ~ factor(vs) * scale(qsec), data = mtcars)
#'
#' ## does not work in binary case, no need for the computational overhead
#' ## ezMULTINOM(am ~ qsec, data = mtcars)
#'
#' ezMULTINOM(Species ~ Sepal.Length, data = iris)
#' }
ezMULTINOM <- function(formula, data, OR = TRUE, digits = 2L, pdigits = 3L) {

  dv <- all.vars(formula)[[1]]
  iv <- all.vars(formula)[-1]

  stopifnot(is.data.frame(data))
  usedat <- droplevels(na.omit(as.data.frame(data)[, c(dv, iv)]))

  stopifnot(is.factor(usedat[[dv]]))
  k <- levels(usedat[[dv]])

  if (length(k) < 3) stop("DV must have at least 3 levels, after omitting missing data")

  nk <- seq_along(k)[-length(k)]

  ivterms <- attr(terms(formula), "term.labels")

  ## models with different contrasts
  m <- lapply(nk, function(i) {
    VGAM::vglm(formula = formula,
         family = VGAM::multinomial(refLevel = i),
         data = usedat)
  })

  m.res <- lapply(nk, function(i) {
    tab <- VGAM::coef(VGAM::summary(m[[i]]))
    ci <- VGAM::confint(m[[i]])

    out <- data.table(
      Ref = k[i],
      Num = as.integer(gsub("^(.*)\\:([1-9]*)$", "\\2", names(VGAM::coef(m[[1]])))),
      Term = gsub("^(.*)\\:([1-9]*)$", "\\1", names(VGAM::coef(m[[1]]))),
      B = tab[, "Estimate"],
      SE = tab[, "Std. Error"],
      P = tab[, "Pr(>|z|)"],
      LL = ci[, 1],
      UL = ci[, 2])
    out[, Comp := k[-i][Num]]

    return(out)
  })

  ## omnibus p-values
  ivterms.pos <- attr(VGAM::model.matrix(m[[1]]), "assign")

  terms.res <- lapply(ivterms, function(v) {
    i <- ivterms.pos[[v]]

    testmat <- matrix(0, nrow = length(i), ncol = length(coef(m[[1]])))
    for (j in seq_along(i)) {
      testmat[j, i[j]] <- 1
    }

    out <- summary(multcomp::glht(m[[1]], linfct = testmat),
                   test = multcomp::Chisqtest())$test

    p <- with(out,
         sprintf(sprintf("Chi-square (df=%%d) = %%0.%df, %%s", digits),
                 df[[1]][1], SSH[1,1], formatPval(pvalue[1, 1],
                                                  pdigits, pdigits, includeP = TRUE)))

    est <- do.call(rbind, lapply(nk, function(j) m.res[[j]][i][Num >= j]))
    est[, Labels := sprintf("%s vs.\n%s", Comp, Ref)]

    ulabels <- unique(est$Labels)
    uterms <- unique(est$Term)

    if (length(uterms) > 1) {
      est[, Term := gsub(sprintf("^(%s)(.*)$", v), "\\2", Term)]
      uterms <- unique(est$Term)

      finaltab <- matrix("",
                         nrow = length(uterms) + 1,
                         ncol = choose(length(k), 2) + 2,
                         dimnames = list(NULL, c("Variable", ulabels, "PValue")))

      finaltab[1, "Variable"] <- v
      finaltab[1, "PValue"] <- p

      for (index in seq_along(uterms)) {
        finaltab[index + 1, "Variable"] <- uterms[index]
        finaltab[index + 1, "PValue"] <- ""
        if (OR) {
          for (i2 in ulabels) {
            finaltab[index + 1, i2] <- est[Term == uterms[index] & Labels == i2, .(
            resf = sprintf(sprintf("%%0.%df%%s\n[%%0.%df, %%0.%df]", digits, digits, digits),
                           exp(B), star(P), exp(LL), exp(UL)))]$resf
          }
        } else {
          for (i2 in ulabels) {
            finaltab[index + 1, i2] <- est[Term == uterms[index] & Labels == i2, .(
              resf = sprintf(sprintf("%%0.%df%%s (%%0.%df)", digits, digits), B, star(P), SE))]$resf
          }
        }
      }
    } else {
      finaltab <- matrix("",
                         nrow = 1,
                         ncol = choose(length(k), 2) + 2,
                         dimnames = list(NULL, c("Variable", ulabels, "PValue")))

      finaltab[1, "Variable"] <- v
      finaltab[1, "PValue"] <- p
      if (OR) {
        for (i2 in ulabels) {
          finaltab[1, i2] <- est[Labels == i2, .(
            resf = sprintf(sprintf("%%0.%df%%s\n[%%0.%df, %%0.%df]", digits, digits, digits),
                           exp(B), star(P), exp(LL), exp(UL)))]$resf
        }
      } else {
        for (i2 in ulabels) {
          finaltab[1, i2] <- est[Labels == i2, .(
              resf = sprintf(sprintf("%%0.%df%%s (%%0.%df)", digits, digits), B, star(P), SE))]$resf
        }
      }
    }

    return(list(Results = est, Table = finaltab))
  })

  list(
    Results = do.call(rbind, lapply(terms.res, `[[`, "Results")),
    Table = do.call(rbind, lapply(terms.res, `[[`, "Table")))
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
           "Comp", "Num", "Labels", "Ref", "Term",
           "B", "P", "LL", "UL", "SE"))
}

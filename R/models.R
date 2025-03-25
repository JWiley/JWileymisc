#' Return Indices of Model Performance
#'
#' Generic function. Generally returns things like
#' fit indices, absolute error metrics, tests of
#' overall model significance.
#'
#' For \code{lm} class objects, return number of observations,
#' AIC, BIC, log likelihood, R2, overall model F test, and p-value.
#'
#' @param object A fitted model object. The class of the model
#'   determines which specific method is called.
#' @param ... Additional arguments passed to specific methods.
#' @return A \code{data.table} with results.
#' @export
#' @rdname modelPerformance
modelPerformance <- function(object, ...) {
  UseMethod("modelPerformance", object)
}

#' @param x A object (e.g., list or a modelPerformance object) to
#'   test or attempt coercing to a modelPerformance object.
#' @importFrom data.table is.data.table as.data.table
#' @rdname modelPerformance
#' @export
as.modelPerformance <- function(x) {
  if (!is.modelPerformance(x)) {
    if (!is.list(x)) {
      stop("Input must be a list or a modelPerformance object")
    }
    augmentClass <- attr(x, "augmentClass")

    x <- list(
      Performance = x[[1]])

    if (is.null(augmentClass)) {
      class(x) <- "modelPerformance"
    } else {
      class(x) <- c(paste0("modelPerformance.", augmentClass), 
        "modelPerformance")
    }
  }

  ## checks that the object is not malformed
  stopifnot("Model" %in% names(x[[1]]))
  stopifnot(is.data.table(x[[1]]))

  return(x)
}

#' @rdname modelPerformance
#' @export
is.modelPerformance <- function(x) {
  inherits(x, "modelPerformance")
}

#' @rdname modelPerformance
#' @export
#' @method modelPerformance lm
#' @return A list with a \code{data.table} with the following elements:
#'   \describe{
#'   \item{Model}{A character string indicating the model type, here lm}
#'   \item{N_Obs}{The number of observations}
#'   \item{AIC}{Akaike Information Criterion}
#'   \item{BIC}{Bayesian Information Criterion}
#'   \item{LL}{log likelihood}
#'   \item{LLDF}{log likelihood degrees of freedom}
#'   \item{Sigma}{Residual variability}
#'   \item{R2}{in sample variance explained}
#'   \item{F2}{Cohen's F2 effect size R2 / (1 - R2)}
#'   \item{AdjR2}{adjusted variance explained}
#'   \item{F}{F value for overall model significance test}
#'   \item{FNumDF}{numerator degrees of freedom for F test}
#'   \item{FDenDF}{denominator degrees of freedom for F test}
#'   \item{P}{p-value for overall model F test}
#'   }
#' @examples
#' modelPerformance(lm(mpg ~ qsec * hp, data = mtcars))
#'
#' modelPerformance(lm(mpg ~ hp, data = mtcars))
#'
#' \dontrun{
#' modelPerformance(lm(mpg ~ 0 + hp, data = mtcars))
#' modelPerformance(lm(mpg ~ 1, data = mtcars))
#' modelPerformance(lm(mpg ~ 0, data = mtcars))
#' }
modelPerformance.lm <- function(object, ...) {
  LL <- logLik(object)
  LLdf <- attr(LL, "df")

  msum <- summary(object)

  ## empty model or intercept only model
  if (object$rank == 0 || (object$rank == 1 && 
      attr(terms(object), "intercept") == 1)) {
    msum$fstatistic <- c(value = NA_real_, numdf = NA_real_, dendf = NA_real_)
  }
  P <- pf(msum$fstatistic[["value"]],
          msum$fstatistic[["numdf"]],
          msum$fstatistic[["dendf"]],
          lower.tail = FALSE)

  out <- data.table(
    Model =  as.character("lm"),
    N_Obs =  as.numeric(nrow(model.matrix(object))),
    AIC =    as.numeric(AIC(object)),
    BIC =    as.numeric(BIC(object)),
    LL =     as.numeric(LL),
    LLDF =   as.numeric(LLdf),
    Sigma =  as.numeric(msum$sigma),
    R2 =     as.numeric(msum$r.squared),
    F2 =     as.numeric(msum$r.squared / (1 - msum$r.squared)),
    AdjR2 =  as.numeric(msum$adj.r.squared),
    F =      as.numeric(msum$fstatistic[["value"]]),
    FNumDF = as.numeric(msum$fstatistic[["numdf"]]),
    FDenDF = as.numeric(msum$fstatistic[["dendf"]]),
    P =      as.numeric(P))
  out <- list(out)
  attr(out, "augmentClass") <- "lm"

  as.modelPerformance(out)
}


#' Calculate R2 Values
#'
#' Generic function to return variance explained (R2)
#' estimates from various models. In some cases these will be true
#' R2 values, in other cases they may be pseudo-R2 values if
#' R2 is not strictly defined for a model.
#'
#' @param object A fitted model object.
#' @param ... Additional arguments passed to specific methods.
#' @return Depends on the method dispatch.
#' @export
#' @rdname R2
R2 <- function(object, ...) {
  UseMethod("R2", object)
}


#' @return The raw and adjusted r-squared value.
#' @method R2 lm
#' @export
#' @rdname R2
#' @examples
#' R2(lm(mpg ~ qsec * hp, data = mtcars))
R2.lm <- function(object, ...) {
  unlist(modelPerformance(object)$Performance[, c("R2", "AdjR2"), with = FALSE])
}


#' Compare Two Models
#'
#' Generic function.
#'
#' @param model1 A fitted model object.
#' @param model2 A fitted model object to compare to \code{model1}
#' @param ... Additional arguments passed to specific methods.
#' @return Depends on the method dispatch.
#' @rdname modelCompare
#' @export
modelCompare <- function(model1, model2, ...) {
  UseMethod("modelCompare", model1)
}

#' @param x An object (e.g., list or a modelCompare object) to
#'   test or attempt coercing to a modelCompare object.
#' @importFrom data.table is.data.table as.data.table
#' @rdname modelCompare
#' @export
as.modelCompare <- function(x) {
  if (!is.modelCompare(x)) {
    if(!is.list(x)) {
      stop("Input must be a list or a modelCompare object")
    }
    augmentClass <- attr(x, "augmentClass")

    x <- list(
      Comparison = x[[1]])

    if(is.null(augmentClass)) {
      class(x) <- "modelCompare"
    } else {
      class(x) <- c(paste0("modelCompare.", augmentClass), "modelCompare")
    }
  }

  ## checks that the object is not malformed
  stopifnot("Model" %in% names(x[[1]]))
  stopifnot(is.data.table(x[[1]]))
  stopifnot(nrow(x[[1]]) > 2)

  return(x)
}

#' @rdname modelCompare
#' @export
is.modelCompare <- function(x) {
  inherits(x, "modelCompare")
}

## clear R CMD CHECK notes
if (getRversion() >= "2.15.1")  utils::globalVariables(c("F2", "FNumDF", "FDenDF"))

#' @rdname modelCompare
#' @importFrom data.table data.table
#' @importFrom stats logLik anova AIC BIC
#' @method modelCompare lm
#' @export
#' @examples
#' m1 <- lm(mpg ~ qsec * hp, data = mtcars)
#'
#' m2 <- lm(mpg ~ am, data = mtcars)
#'
#' modelCompare(m1, m2)
#'
#' ## cleanup
#' rm(m1, m2)
#'
#' \dontrun{
#' m3 <- lm(mpg ~ 1, data = mtcars)
#' m4 <- lm(mpg ~ 0, data = mtcars)
#' modelCompare(m3, m4)
#'
#' ## cleanup
#' rm(m3, m4)
#' }
modelCompare.lm <- function(model1, model2, ...) {
  stopifnot(identical(class(model1), class(model2)))

  i1 <- modelPerformance(model1)$Performance
  i2 <- modelPerformance(model2)$Performance

  if (identical(i1$LLDF, i2$LLDF)) {
    stop("One model must be nested within the other")
  }
  if (i1$LLDF > i2$LLDF) {
    i3 <- i1
    model3 <- model1

    i1 <- i2
    model1 <- model2

    i2 <- i3
    model2 <- model3
    rm(i3, model3)
  }

  test <- anova(model1, model2, test = "F")
  i1$Model <- "Reduced"
  i2$Model <- "Full"
  out <- rbind(
    i1, i2,
    cbind(Model = "Difference", i2[,-1] - i1[,-1]))
  out[3, F2 := R2 / (1 - out[2, R2])]
  out[3, F := test$F[2]]
  out[3, FNumDF := test$Df[2]]
  out[3, FDenDF := test$Res.Df[2]]
  out[3, P := test[["Pr(>F)"]][2]]

  out <- list(out)
  attr(out, "augmentClass") <- "lm"

  as.modelCompare(out)
}


#' Detailed Tests on Models
#'
#' TODO: make me!
#'
#' @param object A fitted model object.
#' @param ... Additional arguments passed to specific methods.
#' @return Depends on the method dispatch.
#' @rdname modelTest
#' @export
modelTest <- function(object, ...) {
  UseMethod("modelTest", object)
}


#' @export
#' @rdname modelTest
is.modelTest <- function(x) {
  inherits(x, "modelTest")
}

#' @param x A object (e.g., list or a modelTest object) to
#'   test or attempt coercing to a modelTest object.
#' @importFrom data.table is.data.table as.data.table
#' @export
#' @rdname modelTest
as.modelTest <- function(x) {
  if (!is.modelTest(x)) {
    if(!is.list(x)) {
      stop("Input must be a list or a modelTest object")
    }
    augmentClass <- attr(x, "augmentClass")

    x <- list(
      FixedEffects = x[[1]],
      RandomEffects = x[[2]],
      EffectSizes = x[[3]],
      OverallModel = x[[4]])
    if(is.null(augmentClass)) {
      class(x) <- "modelTest"
    } else {
      class(x) <- c(paste0("modelTest.", augmentClass), "modelTest")
    }
  }

  stopifnot(is.modelTest(x))
  stopifnot(identical(length(x), 4L))
  stopifnot(identical(names(x),
                      c("FixedEffects",
                        "RandomEffects",
                        "EffectSizes",
                        "OverallModel")))
  return(x)
}

## clear R CMD CHECK notes
if (getRversion() >= "2.15.1")  utils::globalVariables(c("K"))

#' At the moment, \code{modelTest.vglm} method only handles the multinomial
#'   family, although this may get expanded in the future.
#'
#' @return A list with two elements.
#'   \code{Results} contains a data table of the actual estimates.
#'   \code{Table} contains a nicely formatted character matrix.
#' @method modelTest vglm
#' @rdname modelTest
#' @export
#' @importFrom stats model.matrix vcov formula terms update anova
#' @importFrom VGAM vglm multinomial summary anova.vglm vlm
#' @importMethodsFrom VGAM vcov lrtest
#' @importFrom data.table data.table
#' @examples
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' modelTest(m)
#'
#' ## clean up
#' rm(m, mtcars)
#'
#' \dontrun{
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' modelTest(m)
#'
#' m <- VGAM::vglm(cyl ~ scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' modelTest(m)
#'
#' m2 <- VGAM::vglm(cyl ~ factor(vs) * scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' modelTest(m2)
#'
#' m <- VGAM::vglm(Species ~ Sepal.Length,
#'   family = VGAM::multinomial(), data = iris)
#' modelTest(m)
#'
#' set.seed(1234)
#' sampdata <- data.frame(
#'   Outcome = factor(sample(letters[1:3], 20 * 9, TRUE)),
#'   C1 = rnorm(20 * 9),
#'   D3 = sample(paste0("L", 1:3), 20 * 9, TRUE))
#'
#' m <- VGAM::vglm(Outcome ~ factor(D3),
#'   family = VGAM::multinomial(), data = sampdata)
#' modelTest(m)
#'
#' m <- VGAM::vglm(Outcome ~ factor(D3) + C1,
#'   family = VGAM::multinomial(), data = sampdata)
#' modelTest(m)
#' }
modelTest.vglm <- function(object, ...) {

  if ("multinomial" %in% object@family@vfamily) {
    "do something"
  } else {
    stop("can only deal with multinomial family vglm models right now")
  }

  k <- seq_len(ncol(object@y))
  if (length(k) < 3) stop("DV must have at least 3 levels, after omitting missing data")

  ## k - 1 comparisons
  nk <- seq_along(k)[-length(k)]

  ## what are the predictors
  ivterms <- attr(terms(formula(object)), "term.labels")
  ## first get position of each term
  ivterms.pos <- attr(VGAM::model.matrix(object), "assign")
  coefNames <- names(VGAM::coef(object))
  coefNum <- as.integer(gsub("^(.*)\\:([1-9]*)$", "\\2", coefNames))
  coefTerm <- gsub("^(.*)\\:([1-9]*)$", "\\1", names(VGAM::coef(object)))

  coefInfo <- data.table(
    Num = coefNum,
    Names = coefTerm)
  for (v in seq_along(ivterms.pos)) {
    coefInfo[ivterms.pos[[v]], Term := names(ivterms.pos)[v]]
  }

  ## models with different contrasts
  m <- lapply(nk, function(i) {
    update(object, family = VGAM::multinomial(refLevel = i))
  })

  m.res <- lapply(nk, function(i) {
    tab <- VGAM::coef(VGAM::summary(m[[i]]))
    ci <- VGAM::confint(m[[i]])
    out <- copy(coefInfo)

    out[, Ref := k[i]]
    out[, Est := tab[, "Estimate"]]
    out[, SE := tab[, "Std. Error"]]
    out[, Pval := tab[, "Pr(>|z|)"]]
    out[, LL := ci[, 1]]
    out[, UL := ci[, 2]]
    out[, K := length(k)]
    out[, Comp := k[-i][Num]]

    return(out)
  })

  ## omnibus p-values
  terms.res <- lapply(ivterms, function(v) {
    i <- ivterms.pos[[v]]

    if (identical(length(ivterms), 1L)) {
      out <- VGAM::lrtest(object, update(object, as.formula(sprintf(". ~ . - %s", v))))
      out <- data.table(
        Term = v,
        Chisq = out@Body$Chisq[2],
        DF = out@Body$Df[2],
        Pval = out@Body[["Pr(>Chisq)"]][2],
        Type = "Fixed")
    } else {
      out <- anova(object, type = "III")
      out <- data.table(
        Term = v,
        Chisq = out[v, "Deviance"],
        DF = out[v, "Df"],
        Pval = out[v, "Pr(>Chi)"],
        Type = "Fixed")
    }

    est <- do.call(rbind, lapply(nk, function(j) m.res[[j]][i][Num >= j]))
    est[, Labels := sprintf("%s vs. %s", Comp, Ref)]

    return(list(Coefs = est,  ES = out))
  })

  out <- list(
    do.call(rbind, lapply(terms.res, `[[`, "Coefs")),
    NA,
    do.call(rbind, lapply(terms.res, `[[`, "ES")),
    NA)
  attr(out, "augmentClass") <- "vglm"

  as.modelTest(out)
}

## clear R CMD CHECK notes
if (getRversion() >= "2.15.1")  utils::globalVariables(c("Model", "Type"))

#' Modified lm() to use a specified design matrix
#'
#' This function is a minor modification of the lm() function
#' to allow the use of a pre-specified design matrix. It is not intended for
#' public use but only to support \code{modelTest.lm}.
#'
#' @param formula An object of class "formula" although it is only minimally used
#' @param data the dataset
#' @param subset subset
#' @param weights any weights
#' @param na.action Defaults to \code{na.omit}
#' @param model defaults to \code{TRUE}
#' @param x defaults to \code{FALSE}
#' @param y defaults to \code{FALSE}
#' @param qr defaults to \code{TRUE}
#' @param singular.ok defaults to \code{TRUE}
#' @param contrasts defaults to \code{NULL}
#' @param offset missing by default
#' @param designMatrix a model matrix / design matrix (all numeric, pre coded if applicable for discrete variables)
#' @param yObserved the observed y values
#' @param ... additional arguments
#' @return an lm class object
#' @importFrom stats .getXlevels is.empty.model lm.fit lm.wfit model.offset model.weights
#' @seealso \code{lm}
#' @examples
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- lm(mpg ~ hp * cyl, data = mtcars)
#'
#' x <- model.matrix(m)
#' y <- mtcars$mpg
#' m2 <- JWileymisc:::lm2(mpg ~ 1 + cyl + hp:cyl, data = mtcars,
#'   designMatrix = x[, -2, drop = FALSE],
#'   yObserved = y)
#'
#' anova(m, m2)
#'
#' rm(m, m2, x, y)
lm2 <- function (formula, data, subset, weights, na.action,## method = "qr",
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
    contrasts = NULL, offset, designMatrix, yObserved, ...) {
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    ## ## currently removed as not applicable for current use
    ## ## but may be needed if modelTest() ever expands / changes
    ## if (method == "model.frame")
    ##     return(mf)
    ## else if (method != "qr")
    ##     warning(gettextf("method = '%s' is not supported. Using 'qr'",
    ##         method), domain = NA)
    mt <- attr(mf, "terms")
    y <- yObserved
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w))
        stop("'weights' must be a numeric vector")
    offset <- model.offset(mf)
    mlm <- is.matrix(y)
    ny <- if (mlm)
        nrow(y)
    else length(y)
    if (!is.null(offset)) {
        if (!mlm)
            offset <- as.vector(offset)
        if (NROW(offset) != ny)
            stop(gettextf(
              "number of offsets is %d, should equal %d (number of observations)",
                NROW(offset), ny), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (mlm) matrix(NA_real_, 0,
            ncol(y)) else numeric(), residuals = y, fitted.values = 0 *
            y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
            0) else ny)
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    } else {
        x <- designMatrix
        z <- if (is.null(w))
            lm.fit(x, y, offset = offset, singular.ok = singular.ok,
                ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
            ...)
    }
    class(z) <- c(if (mlm) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model) {
        z$model <- mf
    }
    if (ret.x) {
        z$x <- x
    }
    if (ret.y) {
        z$y <- y
    }
    if (!qr) {
        z$qr <- NULL
    }
    return(z)
}

#' @return A list with two elements.
#'   \code{Results} contains a data table of the actual estimates.
#'   \code{Table} contains a nicely formatted character matrix.
#' @method modelTest lm
#' @rdname modelTest
#' @export
#' @importFrom stats model.matrix vcov formula terms update anova
#' @importFrom data.table data.table
#' @importFrom extraoperators %s!in% %?!in%
#' @examples
#' m1 <- lm(mpg ~ qsec * hp, data = mtcars)
#' modelTest(m1)
#'
#' mtcars$cyl <- factor(mtcars$cyl)
#' m2 <- lm(mpg ~ cyl, data = mtcars)
#' modelTest(m2)
#'
#' m3 <- lm(mpg ~ hp * cyl, data = mtcars)
#' modelTest(m3)
#'
#' m4 <- lm(sqrt(mpg) ~ hp * cyl, data = mtcars)
#' modelTest(m4)
#'
#' m5 <- lm(mpg ~ sqrt(hp) * cyl, data = mtcars)
#' modelTest(m5)
#'
#' ## cleanup
#' rm(m1, m2, m3, m4, m5, mtcars)
modelTest.lm <- function(object, ...) {
  ## get formula
  f <- formula(object)
  fe.terms <- terms(f)
  fe.labs <- labels(fe.terms)
  fe.intercept <- if (identical(attr(fe.terms, "intercept"), 1L)) "1" else "0"

  mf <- model.frame(object)
  vnames <- all.vars(formula(object))
  if (identical(ncol(mf), length(vnames))) {
    names(mf) <- vnames
  } else {
    stop(sprintf(paste0(
      "There are %d columns in the model frame [%s],\n  ",
      "but %d variable names in the formula [%s].\n  ",
      "If an on-the-fly transformation was applied,\n  ",
      "try creating the variable / transformation as a new ",
      "variable in the dataset and re-running."),
      ncol(mf),
      paste(names(mf), collapse = ", "),
      length(vnames),
      paste(vnames, collapse = ", " )))
  }
  x <- model.matrix(object)
  y <- object$residuals + object$fitted.values


  cis <- confint(object)
  msum <- summary(object)

  fes <- data.table(
    Term = rownames(cis),
    Est = as.numeric(coef(object)),
    LL = cis[, 1],
    UL = cis[, 2],
    Pval = coef(msum)[, "Pr(>|t|)"])


  out.tests <- do.call(rbind, lapply(seq_along(fe.labs), function(i) {
    out <- data.table(
      Model = NA_character_, N_Obs = NA_real_, AIC = NA_real_,
      BIC = NA_real_, LL = NA_real_, LLDF = NA_real_, Sigma = NA_real_,
      R2 = NA_real_, F2 = NA_real_, AdjR2 = NA_real_, F = NA_real_,
      FNumDF = NA_real_, FDenDF = NA_real_, P = NA_real_)
    if (!is.na(fe.labs[[i]])) {
      use.fe.labs <- fe.labs %s!in% fe.labs[[i]]
      out.f <- sprintf("%s ~ %s%s%s",
              as.character(f)[2],
              fe.intercept,
              if (length(use.fe.labs)) " + " else "",
              paste(use.fe.labs, collapse = " + "))

      use <- attr(x, "assign") %?!in% i
      reduced <- lm2(as.formula(out.f),
                      data = mf,
                      designMatrix = x[, use, drop = FALSE], yObserved = y)

      out <- modelCompare(object, reduced)$Comparison[Model == "Difference"]
    }
    setnames(out, old = "Model", new = "Term")
    out$Term <- fe.labs[[i]]
    return(out)
  }))
  out.tests[, Type := "Fixed"]

  out <- list(fes, NA, out.tests,
              modelPerformance(object))
  attr(out, "augmentClass") <- "lm"

  as.modelTest(out)
}

## clear R CMD CHECK notes
if (getRversion() >= "2.15.1") {
   utils::globalVariables(c(
           "Comp", "Num", "Labels", "Ref", "Term",
           "B", "P", "LL", "UL", "SE"))
}


## clear R CMD CHECK notes
if (getRversion() >= "2.15.1") utils::globalVariables(c("V2", "Index", ".N"))


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
##' @importFrom data.table as.data.table data.table
##' @importFrom stats optim
##' @examples
##' ## make me
findSigRegions <- function(object, l1, l2, name.vary, lower, upper, alpha = .05, starts = 50) {
  foo <- function(x) {
    tmp1 <- l1
    tmp2 <- l2
    tmp1[name.vary] <- x
    tmp2[name.vary] <- x

    out <- abs(alpha - as.data.table(
      as.data.frame(
        rms::contrast(object, tmp1, tmp2)[c("Pvalue")]))[, Pvalue])

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
            as.data.table(as.data.frame(rms::contrast(object, tmp1, tmp2)[c(name.vary, "Contrast", "Pvalue")])),
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
if (getRversion() >= "2.15.1") {
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
##' @importFrom data.table as.data.table
##' @importFrom ggplot2 ggplot geom_text aes scale_x_continuous theme xlab ylab coord_fixed
##' @importFrom ggplot2 element_blank unit geom_line geom_ribbon aes_string geom_segment
##' @importFrom grid arrow
##' @importFrom ggpubr theme_pubr
##' @examples
##' ## make me
intSigRegGraph <- function(object, predList, contrastList, xvar, varyvar,
                           varyvar.levels,
                           xlab = xvar, ylab = "Predicted Values", ratio = 1,
                           xlim, ylim,
                           xbreaks, xlabels = xbreaks,
                           scale.x = c(m = 0, s = 1), scale.y = c(m = 0, s = 1),
                           starts = 50) {

  preds <- as.data.table(do.call(rms::Predict, list(x = object, factors = predList, conf.type = "mean")))
  preds[, xz := (get(xvar) - scale.x["m"])/scale.x["s"]]
  preds[, yz := (yhat - scale.y["m"])/scale.y["s"]]
  preds[, yllz := (lower - scale.y["m"])/scale.y["s"]]
  preds[, yulz := (upper - scale.y["m"])/scale.y["s"]]

  simpleSlopes <- do.call(rbind, lapply(contrastList, function(x) {
    out <- as.data.table(as.data.frame(
      rms::contrast(object,
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
    theme_ggpubr() +
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
##' @param multivariate A logical value whether to have models with
##'   all IVs simultaneously.
##' @param \ldots Additional arguments passed on to the internal 
##'   function, \code{.runIt}.
##' @return A list with all the model results.
##' @keywords internal
##' @importFrom stats na.omit get_all_vars
##' @importFrom data.table data.table
##' @examples
##' test1 <- JWileymisc:::internalcompareIV(
##'   dv = "mpg", type = "normal",
##'   iv = "hp",
##'   covariates = "am",
##'   data = mtcars, multivariate = FALSE)
##' test1$Summary
##' rm(test1)
internalcompareIV <- function(dv, type = c("normal", "binary", "count"),
                       iv, covariates = character(), data,
                       multivariate = FALSE, ...) {

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

  if (requireNamespace("foreach", quietly = TRUE)) {
    `%dopar%` <- foreach::`%dopar%`
  results <- foreach::foreach(i = 1:nIV, .combine = list) %dopar% {
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
      out$Summary <- data.table(
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
      out$Summary <- data.table(
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
  } else {
    results <- list()
    results <- for (i in 1:nIV) {
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
      out$Summary <- data.table(
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
      results[[i]] <- out
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
      out$Summary <- data.table(
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
      results[[i]] <- out
    }
  }
  }

  if (length(results) != nIV & nIV == 1) {
    results <- list(results)
  }
  return(results)
}


#' Compares the effects of various independent variables on dependent variables
#'
#' Utility to estimate the unadjusted, covariate adjusted, and 
#' multivariate adjusted unique contributions of one or more IVs 
#' on one or more DVs
#'
#' @param dv A character string or vector of the depentent variable(s)
#' @param type A character string or vector indicating the type of 
#'   dependent variable(s)
#' @param iv A character string or vector giving the IV(s)
#' @param covariates A character string or vector giving the covariate(s)
#' @param data The data to be used for analysis
#' @param multivariate A logical value whether to have models with all 
#'   IVs simultaneously.
#' @param \ldots Additional arguments passed on to the internal function, 
#'   \code{.runIt}.
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
compareIVs <- function(dv, type, iv, covariates = character(), 
                       data, multivariate = FALSE, ...) {
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
      cbind.data.frame(
        dv = dv[x], 
        iv = iv[y], 
        res[[x]][[y]]$Summary,
        stringsAsFactors = FALSE)
    }))
  }))

  return(res)
}

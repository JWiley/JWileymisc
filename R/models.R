
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

#' estimate detailed results per variable and effect sizes for both fixed and random effects from lmer models
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
#' @importFrom lme4 ngrps
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
#' testm1 <- .detailedTests(m1, method = "profile")
#' testm2 <- .detailedTests(m2, method = "profile")
#' testm2b <- .detailedTests(m2, method = "boot", nsim = 100)
#' }
.detailedTestsLMER <- function(obj, method = c("Wald", "profile", "boot"), ...) {
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


#' Calculates all pairwise contrasts and omnibus tests for multinomial regression
#'
#' TODO: make me!
#'
#' @param obj A \code{vglm} class object, the fitted result of
#'   \code{vglm()}. At the moment only handles the multinomial
#'   family, although this may get expanded in the future.
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
#' @importMethodsFrom VGAM vcov lrtest
#' @importFrom data.table data.table
#' @examples
#'
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' .detailedTestsVGLM(m)
#'
#' rm(m, mtcars)
#'
#' \dontrun{
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' .detailedTestsVGLM(m)
#'
#' .detailedTestsVGLM(m, digits = 4)$Table
#' .detailedTestsVGLM(m, OR = FALSE)
#' .detailedTestsVGLM(m, digits = 4, OR = FALSE)$Table
#'
#' m <- VGAM::vglm(cyl ~ scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' .detailedTestsVGLM(m)
#'
#' m2 <- VGAM::vglm(cyl ~ factor(vs) * scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' .detailedTestsVGLM(m2)
#'
#' m <- VGAM::vglm(Species ~ Sepal.Length,
#'   family = VGAM::multinomial(), data = iris)
#' .detailedTestsVGLM(m)
#' }
.detailedTestsVGLM <- function(obj, OR = TRUE, digits = 2L, pdigits = 3L) {

  if (inherits(obj, "vglm")) {
    if ("multinomial" %in% obj@family@vfamily) {
      "do something"
    } else {
      stop ("can only deal with multinomial family vglm models right now")
    }
  } else {
    stop ("must be a vglm class object")
  }

  k <- 1L:ncol(m@y)
  if (length(k) < 3) stop("DV must have at least 3 levels, after omitting missing data")

  nk <- seq_along(k)[-length(k)]

  ivterms <- attr(terms(formula(obj)), "term.labels")

  ## models with different contrasts
  m <- lapply(nk, function(i) {
    update(obj, family = VGAM::multinomial(refLevel = i))
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

    if (identical(length(ivterms), 1L)) {
    out <- VGAM::lrtest(obj, update(obj, as.formula(sprintf(". ~ . - %s", v))))
    p <- sprintf(sprintf("Chi-square (df=%%d) = %%0.%df, %%s", digits),
                 out@Body$Df[2],
                 out@Body$Chisq[2],
                 formatPval(out@Body[["Pr(>Chisq)"]][2],
                            pdigits, pdigits, includeP = TRUE))
    } else {
    out <- anova(obj, type = "III")
    p <- sprintf(sprintf("Chi-square (df=%%d) = %%0.%df, %%s", digits),
                 out[v, "Df"],
                 out[v, "Deviance"],
                 formatPval(out[v, "Pr(>Chi)"],
                            pdigits, pdigits, includeP = TRUE))
    }

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

## clear R CMD CHECK notes
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
           "Comp", "Num", "Labels", "Ref", "Term",
           "B", "P", "LL", "UL", "SE"))
}


#' Detailed Comparisons and Tests on Models
#'
#' TODO: make me!
#'
#' @param obj A fitted model object, currently either a
#'   \code{merMod} or \code{vglm} class object.
#' @param ... Additional arguments passed to specific methods.
#' @return Depends on the method dispatch,
#'  see \code{.detailedTestsLMER} and \code{.detailedTestsVGLM}.
#' @export
#' @examples
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' detailedTests(m)
#' rm(m, mtcars)
#'
#' \dontrun{
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' detailedTests(m)
#'
#' detailedTests(m, digits = 4)$Table
#' detailedTests(m, OR = FALSE)
#' detailedTests(m, digits = 4, OR = FALSE)$Table
#'
#' m <- VGAM::vglm(cyl ~ scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' detailedTests(m)
#'
#' m2 <- VGAM::vglm(cyl ~ factor(vs) * scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' detailedTests(m2)
#'
#' m <- VGAM::vglm(Species ~ Sepal.Length,
#'   family = VGAM::multinomial(), data = iris)
#' detailedTests(m)
#'
#' data(aces_daily)
#' m1 <- lme4::lmer(NegAff ~ STRESS + (1 + STRESS | UserID),
#'   data = aces_daily)
#' m2 <- lme4::lmer(NegAff ~ STRESS + I(STRESS^2) + (1 + STRESS | UserID),
#'   data = aces_daily)
#' testm1 <- detailedTests(m1, method = "profile")
#' testm2 <- detailedTests(m2, method = "profile")
#' testm2b <- detailedTests(m2, method = "boot", nsim = 100)
#' }
detailedTests <- function(obj, ...) {
  if (inherits(obj, "merMod")) {
    if (isLMM(obj)) {
      .detailedTestsLMER(obj = obj, ...)
    } else {
      stop("can only deal with lmer() results from merMod objects")
    }
  } else if (inherits(obj, "vglm")) {
    if ("multinomial" %in% obj@family@vfamily) {
      .detailedTestsVGLM(obj = obj, ...)
    } else {
      stop ("can only deal with multinomial family vglm models right now")
    }
  } else {
    stop ("must be a merMod from lmer() or vglm() class object")
  }
}



## clear R CMD CHECK notes
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

    out <- abs(alpha - as.data.table(
      as.data.frame(
        contrast(object, tmp1, tmp2)[c("Pvalue")]))[, Pvalue])

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

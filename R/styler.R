#' A generic function for pretty printing in (semi) APA Style
#'
#' @param object An object with a class matching one of the methods
#' @param ... Additional argiuments passed on to methods.
#' @export
APAStyler <- function(object, ...) {
  UseMethod("APAStyler")
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1") utils::globalVariables(c("Rank", "._FinalRank", "._WhichModel"))

#' APAStyler method for lists
#'
#' This assumes that all the objects in a list have the same class
#' and that an \code{APAStyler} method exists for that class.
#'
#' @param object A list in this case, where each element
#'   is another known class.
#' @param ... Additional arguments.
#' @return Styled results.
#' @method APAStyler list
#' @importFrom data.table fifelse
#' @export
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ qsec * hp, data = mtcars)
#' m2 <- lm(mpg ~ qsec + hp, data = mtcars)
#' m3 <- lm(mpg ~ am + vs, data = mtcars)
#' mt1 <- modelTest(m1)
#' mt2 <- modelTest(m2)
#' mt3 <- modelTest(m3)
#'
#' ## styling regression models
#' APAStyler(list(m1, m2))
#'
#' ## modelTest objects get merged
#' APAStyler(list(mt1, mt2))
#'
#' ## the models can be named by passing a named list
#' ## including "special" characters using backticks, like spaces
#' APAStyler(list(Full = mt1, Reduced = mt2))
#' APAStyler(list(Full = mt1, Reduced = mt2, `Alternate Model` = mt3))
#'
#' ## you can customize the way output is presented
#' APAStyler(list(mt1, mt2), format = list(
#'   FixedEffects = "%s, %s\n(%s, %s)",
#'   EffectSizes = "Cohen's f2 = %s (%s)"))
#'
#' ## clean up
#' rm(m1, m2, m3, mt1, mt2, mt3)
#' }
APAStyler.list <- function(object, ...) {

  out <- lapply(object, APAStyler, ...)

  ## are all model test results that can be merged?
  OK <- sapply(object, inherits, what = "modelTest")
  if (isTRUE(all(OK, na.rm = TRUE))) {
    object <- object[OK]
    out <- out[OK]

    for (i in seq_along(out)) {
      out[[i]] <- out[[i]][, Rank := 1:.N]
      setnames(out[[i]],
               old = c("Est", "Rank"),
               new = paste0(c("Est", "Rank"), i))
    }

    if (length(out) > 1) {
      mout <- out[[1]]
      for (i in 2:length(out)) {
        mout <- merge(mout, out[[i]], by = c("Term", "Type"), all = TRUE)
      }
    }

    mout[, ._FinalRank := apply(
             mout[, grep("^Rank[0-9]+$", names(mout), value=TRUE), with = FALSE],
             1, function(x) na.omit(unlist(x))[[1]])]
    mout[, ._WhichModel := apply(
             mout[, grep("^Rank[0-9]+$", names(mout), value=TRUE), with = FALSE],
             1, function(x) min(which(!is.na(x))))]
    mout[, Type := factor(Type, levels = c(
                                  "Fixed Effects", "Random Effects",
                                  "Overall Model", "Effect Sizes"))]

    out <- mout[order(Type, ._WhichModel, ._FinalRank),
         c("Term", grep("^Est[0-9]+$", names(mout), value = TRUE), "Type"),
         with = FALSE]
    out[, Type := as.character(Type)]

    if (isFALSE(is.null(names(object[OK])))) {
      setnames(out,
               old = paste0("Est", 1:sum(OK)),
               new = names(object[OK]))
    }

    out <- out[, lapply(.SD, function(x) fifelse(is.na(x), "", x))]
  }

  return(out)
}


# clear R CMD CHECK notes
if(getRversion() >= "2.15.1") utils::globalVariables(c("Est", "Pval", "Type",
                                                       "F2"))

#' APAStyler method for model tests from a linear model
#'
#' @param object A \code{modelTest.lm} class object,
#'   results from running \code{modelTest()} function on a
#'   class \code{lm} object.
#' @param format A list giving the formatting style to be used for
#'   the fixed effecvts and effect sizes.
#' @param digits A numeric value indicating the number of digits to print.
#'   This is still in early implementation stages and currently does not
#'   change all parts of the output (which default to 2 decimals per
#'   APA style).
#' @param pcontrol A list controlling how p values are formatted.
#' @param ... Additional arguments.
#' @return Styled results.
#' @method APAStyler modelTest.lm
#' @export
#' @examples
#' m1 <- lm(mpg ~ qsec * hp, data = mtcars)
#' APAStyler(modelTest(m1))
#'
#' APAStyler(modelTest(m1),
#' format = list(
#'   FixedEffects = "%s, %s\n(%s, %s)",
#'   EffectSizes = "Cohen's f2 = %s (%s)"),
#' pcontrol = list(digits = 4,
#'   stars = FALSE, includeP = TRUE,
#'   includeSign = TRUE,
#'   dropLeadingZero = TRUE))
#'
#' ## clean up
#' rm(m1)
APAStyler.modelTest.lm <- function(object,
  format = list(
    FixedEffects = c("%s%s [%s, %s]"),
    EffectSizes = c("f2 = %s, %s")),
  digits = 2,
  pcontrol = list(
    digits = 3, stars = TRUE,
    includeP = FALSE, includeSign = FALSE,
    dropLeadingZero = TRUE),  ...) {

  FE <- copy(object$FixedEffects[, .(
    Term = Term,
    Est = .fround(Est, digits),
    LL = ifelse(is.na(LL), "", .fround(LL, digits)),
    UL = ifelse(is.na(UL), "", .fround(UL, digits)),
    P = if (pcontrol$stars) {
          star(Pval)
        } else {
          formatPval(Pval,
                     d = pcontrol$digits,
                     sd = pcontrol$digits,
                     includeP = pcontrol$includeP,
                     includeSign = pcontrol$includeSign,
                     dropLeadingZero = pcontrol$dropLeadingZero)
        })])
  FE <- FE[, .(
    Term = Term,
    Est = sprintf(format$FixedEffects, Est, P, LL, UL),
    Type = "Fixed Effects")]

  MISC <- data.table(
      Term = c(
        "N (Observations)",
        "logLik DF",
        "logLik",
        "AIC",
        "BIC",
        "F2",
        "R2",
        "Adj R2"),
      Est = c(
        as.character(object$OverallModel$Performance$N_Obs),
        as.character(object$OverallModel$Performance$LLDF),
        .fround(object$OverallModel$Performance$LL, digits),
        .fround(object$OverallModel$Performance$AIC, digits),
        .fround(object$OverallModel$Performance$BIC, digits),
        .fround(object$OverallModel$Performance$F2, digits),
        .fround(object$OverallModel$Performance$R2, digits),
        .fround(object$OverallModel$Performance$AdjR2, digits)))
  MISC[, Type := "Overall Model"]

  EFFECT <- copy(object$EffectSizes[, .(
      Term = Term,
      Est = sprintf(format$EffectSizes,
                    .fround(F2, digits = digits),
                    formatPval(P, d = pcontrol$digits, sd = pcontrol$digits,
                               includeP = TRUE, includeSign = TRUE)))])
  EFFECT[, Type := "Effect Sizes"]

  rbind(FE, MISC, EFFECT)
}


## clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("Names", ".SD", "Count", "DF", "Chisq", "Test"))

#' APAStyler method for model tests from a vglm multinomial model
#'
#' @param object A \code{modelTest.vglm} class object,
#'   results from running \code{modelTest()} function on a
#'   class \code{vglm} object with a multinomial family
#' @param format A list giving the formatting style to be used for
#'   the fixed effects and effect sizes.
#' @param digits A numeric value indicating the number of digits to print.
#'   This is still in early implementation stages and currently does not
#'   change all parts of the output (which default to 2 decimals per
#'   APA style).
#' @param pcontrol A list controlling how p values are formatted.
#' @param OR a logical value whether to report odds ratios and
#'   95 percent confidence intervals, if \code{TRUE}, or
#'   regression coefficients on the logit scale with standard
#'   errors, if \code{FALSE}.
#' @param ... Additional arguments.
#' @return Styled results.
#' @method APAStyler modelTest.vglm
#' @export
#' @importFrom stats reshape
#' @importFrom data.table data.table setnames copy
#' @importFrom extraoperators %ain%
#' @examples
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' mt <- modelTest(m)
#'
#' APAStyler(mt)
#'
#' APAStyler(mt, OR = FALSE)
#'
#' ## clean up
#' rm(m, mt, mtcars)
#'
#' \dontrun{
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#' m <- VGAM::vglm(cyl ~ qsec,
#'   family = VGAM::multinomial(), data = mtcars)
#' APAStyler(modelTest(m))
#'
#' m <- VGAM::vglm(cyl ~ scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' APAStyler(modelTest(m))
#'
#' m2 <- VGAM::vglm(cyl ~ factor(vs) * scale(qsec),
#'   family = VGAM::multinomial(), data = mtcars)
#' APAStyler(modelTest(m2))
#'
#' m <- VGAM::vglm(Species ~ Sepal.Length,
#'   family = VGAM::multinomial(), data = iris)
#' APAStyler(modelTest(m))
#'
#' set.seed(1234)
#' sampdata <- data.frame(
#'   Outcome = factor(sample(letters[1:3], 20 * 9, TRUE)),
#'   C1 = rnorm(20 * 9),
#'   D3 = sample(paste0("L", 1:3), 20 * 9, TRUE))
#'
#' m <- VGAM::vglm(Outcome ~ factor(D3),
#'   family = VGAM::multinomial(), data = sampdata)
#' APAStyler(modelTest(m))
#'
#' m <- VGAM::vglm(Outcome ~ factor(D3) + C1,
#'   family = VGAM::multinomial(), data = sampdata)
#' APAStyler(modelTest(m))
#' }
APAStyler.modelTest.vglm <- function(object,
  format = list(
    FixedEffects = c("%s%s [%s, %s]"),
    EffectSizes = c("Chi-square (df=%s) = %s, %s")),
  digits = 2,
  pcontrol = list(
    digits = 3, stars = TRUE,
    includeP = FALSE, includeSign = FALSE,
    dropLeadingZero = TRUE),
  OR = TRUE, ...) {

  FE <- copy(object$FixedEffects)
  if (OR) {
    FE[, Est := exp(Est)]
    FE[, LL := exp(LL)]
    FE[, UL := exp(UL)]
  }

  FE <- FE[, .(
    Names = Names,
    Term = Term,
    Labels = Labels,
    Est = .fround(Est, digits),
    LL = ifelse(is.na(LL), "", .fround(LL, digits)),
    UL = ifelse(is.na(UL), "", .fround(UL, digits)),
    P = if (pcontrol$stars) {
          star(Pval)
        } else {
          formatPval(Pval,
                     d = pcontrol$digits,
                     sd = pcontrol$digits,
                     includeP = pcontrol$includeP,
                     includeSign = pcontrol$includeSign,
                     dropLeadingZero = pcontrol$dropLeadingZero)
        })]
  FE <- FE[, .(
    Term = Term,
    Names = Names,
    Labels = Labels,
    Est = sprintf(format$FixedEffects, Est, P, LL, UL),
    Type = "Fixed Effects")]

  FE <- reshape(FE, v.names = "Est",
          idvar = c("Names", "Term"),
          timevar = "Labels",
          direction = "wide",
          sep = "_")
  setnames(FE, names(FE), gsub("^Est_", "", names(FE)))

  ## deal with cases where due to dummy coding
  ## there are multiple rows for one term
  FE <- do.call(rbind, lapply(unique(FE$Term), function(v) {
    temp <- FE[Term == v]
    if (nrow(temp) > 1) {
      tempO <- copy(temp[1])
      tempO[, (names(tempO)) := lapply(.SD, as.na)]
      tempO[, Term := temp[1, Term]]
      tempO[, Names := "Overall"]
      tempO[, Type := "Fixed Effects"]
      rbind(tempO, temp)
    } else {
      temp
    }}))
  FE[, Count := .N, by = Term]

  EFFECT <- copy(object$EffectSizes[, .(
      Term = Term,
      Test = sprintf(format$EffectSizes,
                     as.character(DF),
                     .fround(Chisq, digits = digits),
                     formatPval(Pval, d = pcontrol$digits, sd = pcontrol$digits,
                                includeP = TRUE, includeSign = TRUE)))])

  stopifnot(FE$Term %ain% EFFECT$Term && EFFECT$Term %ain% FE$Term)

  out <- merge(
    FE, EFFECT,
    by = "Term")
  out[Count > 1 & Names != "Overall", Test := as.na(Test)]
  ## "delete" columns
  out[, Count := NULL]
  out[, Type := NULL]

  return(out)
}




#' APAStyler method for linear models
#'
#' @param object A \code{lm} object
#' @param digits The number of digits to round results to. Defaults to 2.
#' @param pdigits The number of digits to use for p values. Defaults to digits + 1 if missing.
#' @param file An optional argument indicating whether the output should be written to a file.
#' @param ... Additional argiuments passed on to \code{write.table}.
#' @importFrom stats coef confint
#' @importFrom utils write.table
#' @method APAStyler lm
#' @export
APAStyler.lm <- function(object, digits = 2, pdigits, file, ...) {
  if (missing(pdigits)) pdigits <- digits + 1

  s <- summary(object)
  ctable <- coef(s)
  cstars <- star(ctable[, "Pr(>|t|)"])
  ctable <- .fround(ctable, digits = digits)
  est <- paste(ctable[, "Estimate"], cstars, " (",
               ctable[, "Std. Error"], ")", sep = "")

  ci <- .fround(confint(object, level = 0.95), digits = digits)
  ci <- paste("[", ci[, 1], ", ", ci[, 2], "]", sep = "")


  F <- s$fstatistic
  if (is.null(F)) {
    F <- c("value" = NA_real_,
           "numdf" = NA_real_,
           "dendf" = NA_real_)
  }
  p <- pf(F[1], F[2], F[3], lower.tail = FALSE)

  r2 <- paste(.fround(s$r.squared, digits = digits), star(p), sep = "")
  F <- .fround(F, digits = digits)

  p <- formatPval(p, d = pdigits, includeP = TRUE, includeSign = TRUE,
                  dropLeadingZero = TRUE)

  F <- c(paste("F(", F[2], ", ", F[3], ") = ", F[1], sep = ""), p)

  out <- matrix(character(1), nrow = length(est) + 2, ncol = 2,
                dimnames = list(c("Constant", rownames(ctable)[-1], "R^2", "F"),
                                c("B (SE)", "95% CI")))

  out[1:length(est), 1] <- est
  out[1:length(ci), 2] <- ci
  out["R^2", 1] <- r2
  out["F", ] <- F

  if (!missing(file)) write.table(out, file = file, ...)
  print(noquote(out))
  return(invisible(out))
}

#' A generic function for pretty printing in (semi) APA Style
#'
#' @param object \code{mira} object
#' @param lmobject an lm object the degrees of freedom of which can be used for conservative F tests
#' @param digits The number of digits to round results to. Defaults to 2.
#' @param pdigits The number of digits to use for p values. Defaults to digits + 1 if missing.
#' @param file An optional argument indicating whether the output should be written to a file.
#' @param ... Additional argiuments passed on to \code{write.table}.
#' @importFrom mice pool.r.squared
#' @importFrom mice pool
#' @method APAStyler mira
#' @export
APAStyler.mira <- function(object, lmobject, digits = 2, pdigits, file, ...) {
  if (!inherits(object[["analyses"]][[1]], "lm"))
    stop("mira object must use a model that inherits from class 'lm' such as 'lm' or 'glm'")

  if (missing(pdigits)) pdigits <- digits + 1
  F <- r2 <- NULL

  ctable <- summary(pool(object))
  cstars <- star(ctable[, "Pr(>|t|)"])
  ctable <- .fround(ctable, digits = digits)

  est <- paste(ctable[, "est"], cstars, " (",
               ctable[, "se"], ")", sep = "")

  ci <- paste("[", ctable[, "lo 95"], ", ", ctable[, "hi 95"], "]", sep = "")

  out <- matrix(character(1), nrow = length(est), ncol = 2,
                dimnames = list(c("Constant", rownames(ctable)[-1]),
                                c("B (SE)", "95% CI")))
  out[1:length(est), 1] <- est
  out[1:length(ci), 2] <- ci

  if (identical(class(object[["analyses"]][[1]]), "lm")) {
    r2 <- pool.r.squared(object)[, "est"]

    if (!missing(lmobject)) {
      df <- summary(lmobject)$fstatistic[2:3]
      F <- f.r2(r2, df[1], df[2])[1:3]
      p <- f.r2(r2, df[1], df[2])[4]

      r2 <- c(paste(.fround(r2, digits = digits), star(p), sep = ""), "")

      F <- .fround(F, digits = digits)

      p <- formatPval(p, d = pdigits, includeP = TRUE, includeSign = TRUE,
                      dropLeadingZero = TRUE)

      F <- c(paste("F(", F[2], ", ", F[3], ") = ", F[1], sep = ""), p)
      warning("Using the degrees of freedom for the listwise deleted model for the F test and R^2 significance.",
              call. = FALSE)
    } else {
      r2 <- c(paste(.fround(r2, digits = digits), sep = ""), "")
    }
    out <- rbind(out, "R^2" = r2, "F" = F)
  } else if (!missing(lmobject)) warning("lmobject argument ignored")

  if (!missing(file)) write.table(out, file = file, ...)

  print(noquote(out))

  return(invisible(out))
}

#' A generic function for pretty printing in (semi) APA Style
#'
#' @param object \code{SEMSummary} object
#' @param digits The number of digits to round results to. Defaults to 2.
#' @param type A character vector giving what to print. Defaults to \sQuote{cov},
#'   the covariances. Other options are \sQuote{cor} and \sQuote{both}.
#' @param stars A logical value whether to include significance values as
#'   stars (*** p < .001, ** p < .01, * p < .05).
#' @param file An optional argument indicating whether the output should be written to a file.
#' @param sep Character what the separator for the table should be. Defaults to tabs.
#' @param ... Additional argiuments passed on to \code{write.table}.
#' @export
#' @method APAStyler SEMSummary
#' @examples
#' m <- SEMSummary(~., data = mtcars)
#' APAStyler(m, type = "cor", stars = FALSE, file = FALSE)
#' APAStyler(m, type = "cov", stars = FALSE, file = FALSE)
#' APAStyler(m, type = "both", stars = FALSE, file = FALSE)
#' APAStyler(m, type = "cor", stars = TRUE, file = FALSE)
#' APAStyler(m, type = "cov", stars = TRUE, file = FALSE)
#' APAStyler(m, type = "both", stars = TRUE, file = FALSE)
APAStyler.SEMSummary <- function(object, digits = 2, type = c("cov", "cor", "both"), stars = FALSE,
  file = ifelse(.Platform$OS.type == "windows", "clipboard", FALSE), sep = "\t", ...) {

  type <- match.arg(type)
  mat <- switch(type,
                cov = {
                  m <- .fround(object$Sigma, digits = digits)
                  if (stars) m[] <- paste0(m, star(object$pvalue))
                  m[lower.tri(m)] <- ""
                  diag(m) <- " - "
                  m
                },
                cor = {
                  m <- .fround(object$sSigma, digits = digits)
                  if (stars) m[] <- paste0(m, star(object$pvalue))
                  m[lower.tri(m)] <- ""
                  diag(m) <- " - "
                  m
                },
                both = {
                  mv <- .fround(object$Sigma, digits = digits)
                  mc <- .fround(object$sSigma, digits = digits)

                  if (stars) {
                    mv[] <- paste0(mv, star(object$pvalue))
                    mc[] <- paste0(mc, star(object$pvalue))
                  }

                                        #mv[] <- as.character(mv)
                  mv[lower.tri(mv)] <- mc[lower.tri(mc)]
                  diag(mv) <- " - "
                  mv
                })


  X <- lapply(object[c("n", "nmissing", "mu", "stdev", "coverage")],
              round, digits = digits)

  out.table <- matrix("", nrow = length(object$names), ncol = 3 + length(object$names))
  rownames(out.table) <- paste(seq_along(object$names), ". ", object$names, sep = '')
  colnames(out.table) <- c("N", "M", "SD", paste(seq_along(object$names), ".", sep = ''))

  out.table[, "N"] <- round(X$n - X$nmissing, digits = digits)
  out.table[, "M"] <- format(X$mu, nsmall = digits)
  out.table[, "SD"] <- format(X$stdev, nsmall = digits)
  out.table[, -c(1:3)] <- mat

  mtable <- X$coverage
  mtable[] <- as.character(mtable)
  mtable[lower.tri(mtable)] <- ""

  if (!identical(FALSE, file)) {
    copyout <- cbind(rownames(out.table), out.table)
    copyout <- rbind(colnames(copyout), copyout)
    write.table(copyout, file = file, sep = sep, row.names = FALSE, col.names = FALSE, ...)
  }
  output <- list(table = out.table, coverage = mtable)

  print(noquote(out.table))
  cat("\nPercentage of coverage for each pairwise covariance or correlation\n\n")
  print(noquote(mtable))

  return(invisible(output))
}


#' Function to simplify converting p-values to asterisks
#'
#' @param x p values to convert to stars
#' @param includeMarginal logical value whether to include a symbol for
#'   marginally significant >.05 but < .10 p-values. Defaults to \code{FALSE}.
#' @return A character string with stars
#' @keywords misc
#' @importFrom stats symnum
#' @export
#' @examples
#' star(c(.0005, .001, .005, .01, .02, .05, .08, .1, .5, 1))
star <- function(x, includeMarginal = FALSE) {
  if (includeMarginal) {
    symnum(x, legend = FALSE, na = "",
           cutpoints = c(0, 0.001, 0.01, 0.05, 0.10, 1),
           symbols = c("***", "**", "*", "^", ""))

  } else {
    symnum(x, legend = FALSE, na = "",
           cutpoints = c(0, 0.001, 0.01, 0.05, 1),
           symbols = c("***", "**", "*", ""))
  }
}


#' Function to simplify formatting p-values for easy viewing / publication
#'
#' @param x p values to convert
#' @param d number of digits
#' @param sd number of scientific digits. Defaults to \code{d} if missing.
#' @param includeP logical value whether to include the character \dQuote{p} itself.
#'   Defaults to \code{FALSE}.
#' @param includeSign logical value whether to include the character \dQuote{=} or \dQuote{<}.
#'   Defaults to \code{FALSE} and if \code{includeP = TRUE} it must be \code{TRUE}.
#' @param dropLeadingZero logical value whether to drop leading zeros for p-values.
#'   Defaults to \code{TRUE}.
#' @return A character string with stars
#' @keywords misc
#' @export
#' @examples
#' formatPval(c(.00052456, .000000124, .01035, .030489, .534946))
#' formatPval(c(.00052456, .000000124, .01035, .030489, .534946), 3, 3, FALSE, TRUE)
#' formatPval(c(.00052456, .000000124, .01035, .030489, .534946), 3, 3, TRUE, TRUE)
#' formatPval(c(.00052456, .000000124, .01035, .030489, .534946), 5)
#' formatPval(c(1, .15346, .085463, .05673, .04837, .015353462,
#'   .0089, .00164, .0006589, .0000000053326), 3, 5)
#' formatPval(c(1, .15346, .085463, .05673, .04837, .015353462,
#'   .0089, .00164, .0006589, .0000000053326), 3, 5, dropLeadingZero = FALSE)
formatPval <- function(x, d = 3, sd, includeP = FALSE, includeSign = FALSE, dropLeadingZero = TRUE) {
  if (includeP) {
    includeSign <- TRUE
  }
  if (missing(sd)) {
    sd <- d
  }

  if (!includeP & !includeSign) {
    out <- ifelse(x < 1/(10^d),
           paste0("< .", paste(rep(0, d - 1), collapse = ""), "1"),
           format(round(x, digits = d), digits = d, nsmall = d, scientific = sd))
  } else if (includeSign & !includeP) {
    out <- ifelse(x < 1/(10^d),
           paste0("< .", paste(rep(0, d - 1), collapse = ""), "1"),
           paste0("= ", format(round(x, digits = d), digits = d, nsmall = d, scientific = sd)))
  } else if (includeSign & includeP) {
    out <- ifelse(x < 1/(10^d),
           paste0("p < .", paste(rep(0, d - 1), collapse = ""), "1"),
           paste0("p = ", format(round(x, digits = d), digits = d, nsmall = d, scientific = sd)))
  }

  if (dropLeadingZero) {
    out <- gsub("0\\.", ".", out)
  }
  return(out)
}

#' Calculates summaries for a parameter
#'
#' This function takes a vector of statistics and calculates
#' several summaries: mean, median, 95% CI, and
#' the empirical p-value, that is, how many fall on the other
#' side of zero.
#'
#' @param x a data vector to operate on
#' @param trans A function to transform the data. Used for summaries,
#'   but not p-values. Defaults to the identity function.
#' @param \dots Additional arguments passed to \code{formatPval}
#'   to control p-value printing.
#' @param na.rm Logical whether to remove NA values. Defaults to \code{TRUE}
#' @return A data frame of summary statistics
#' @export
#' @keywords utilities
#' @importFrom stats median sd
#' @importFrom data.table data.table
#' @examples
#'
#' param_summary(rnorm(100))
param_summary <- function(x, trans = function(x) x, ..., na.rm = TRUE) {
  data.table(
    Mean = trans(mean(x, na.rm = na.rm)),
    Median = trans(median(x, na.rm = na.rm)),
    SE = sd(trans(x), na.rm = na.rm),
    LL2.5 = trans(as.vector(quantile(x, probs = .025, na.rm = na.rm))),
    UL97.5 = trans(as.vector(quantile(x, probs = .975, na.rm = na.rm))),
    pvalue = formatPval(empirical_pvalue(x)[["p-value"]], ...))
}

#' Format a data frame of summary statistics
#'
#' This functions nicely formats a data frame of parameter summary
#' statistics and is designed to be used with the param_summary()
#' function.
#'
#' @param d A data frame of the parameter summary statistics
#' @param digits Number of digits to round to for printing
#' @param pretty Logical value whether prettified values should be returned.
#'   Defaults to \code{FALSE}.
#' @return A formatted data.table of summary statistics or a formated
#' vector (if \code{pretty = TRUE}).
#' @export
#' @importFrom data.table as.data.table
#' @keywords utilities
#' @examples
#' set.seed(1234)
#' xsum <- do.call(rbind, apply(matrix(rnorm(100*10), ncol = 10),
#'   2, param_summary))
#' rownames(xsum) <- letters[1:10]
#' param_summary_format(xsum)
#' param_summary_format(xsum, pretty = TRUE)
#'
#' rm(xsum)
param_summary_format <- function(d, digits = getOption("digits"), pretty = FALSE) {
  if (pretty) {
    string <- sprintf("%%01.%df [%%01.%df, %%01.%df], %%s", digits, digits, digits)

    out <- sprintf(string,
                   d$Mean,
                   d$LL2.5,
                   d$UL97.5,
                   ifelse(grepl("<", d$pvalue),
                          paste0("p ", d$pvalue),
                          paste0("p = ", d$pvalue)))
    names(out) <- rownames(d)
  } else {
    out <- as.data.table(as.data.frame(lapply(d, function(x) {
      if (is.numeric(x)) {
        sprintf(sprintf("%%01.%df", digits), x)
      } else {
        x
      }
    }), stringsAsFactors = FALSE))
    rownames(out) <- rownames(d)
    }

  return(out)
}


#' Function to format the reuslts of a hypothesis test as text
#'
#' @param x A \code{htest} class object
#' @param type The type of htest. Currently one of: \dQuote{t}, \dQuote{F}, \dQuote{chisq},
#'   \dQuote{kw}, \dQuote{mh}, \dQuote{r_pearson}, \dQuote{r_kendall}, or \dQuote{r_spearman}
#'   for t-tests, F-tests, chi-square tests, kruskal-wallis tests,
#'   Mantel-Haenszel tests, pearson correlations, kendall tau correlation,
#'   and spearman rho correlation, respectively.
#' @param \dots Arguments passed on to p-value formatting
#' @return A character string with results
#' @keywords misc
#' @export
#' @examples
#' formatHtest(t.test(extra ~ group, data = sleep), type = "t")
#' formatHtest(anova(aov(mpg ~ factor(cyl), data = mtcars)), type = "F")
#' formatHtest(chisq.test(c(A = 20, B = 15, C = 25)), type = "chisq")
#' formatHtest(kruskal.test(Ozone ~ Month, data = airquality), type = "kw")
#' formatHtest(mantelhaen.test(UCBAdmissions), type = "mh")
#' formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "pearson"), type = "r_pearson")
#' formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "kendall"), type = "r_kendall")
#' formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "spearman"), type = "r_spearman")
formatHtest <- function(x, type = c("t", "F", "chisq", "kw", "mh", "r_pearson", "r_kendall", "r_spearman"), ...) {
  type <- match.arg(type)

  switch(type,
         r_pearson = sprintf("r = %0.2f, CI = (%0.2f, %0.2f), t(df = %s) = %0.2f, %s",
                     x$estimate, x$conf.int[1], x$conf.int[2],
                     as.character(round(x$parameter, 2)),
                     x$statistic,
                     formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         r_kendall = sprintf("tau = %0.2f, z = %0.2f, %s",
                     x$estimate,
                     x$statistic,
                     formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         r_spearman = sprintf("rho = %0.2f, S = %0.1f, %s",
                     x$estimate,
                     x$statistic,
                     formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         t = sprintf("t(df = %s) = %0.2f, %s",
                     as.character(round(x$parameter, 2)),
                     x$statistic,
                     formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         F = sprintf("F(%s, %s) = %0.2f, %s",
                     as.character(round(x[1, "Df"], 2)),
                     as.character(round(x[2, "Df"], 2)),
                     x[1, "F value"],
                     formatPval(x[1, "Pr(>F)"], includeP = TRUE, includeSign = TRUE, ...)),
         chisq = sprintf("Chi-square(df = %d) = %s, %s",
                         x$parameter,
                         as.character(round(x$statistic, 2)),
                         formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         kw = sprintf("Kruskal-Wallis chi-square(df = %d) = %s, %s",
                      x$parameter,
                      as.character(round(x$statistic, 2)),
                      formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         mh = sprintf("Mantel-Haenszel chi-square(df = %d) = %0.2f, %s, common odds ratio = %0.2f, CI = (%0.2f, %0.2f).",
                      x$parameter, x$statistic,
                      formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...),
                      x$estimate, x$conf.int[1], x$conf.int[2]))
}


#' Function to format the median and IQR of a variable
#'
#' @param x the data to have the median and IQR calculated
#' @param d How many digits to display. Defaults to 2.
#' @param na.rm Logical whether to remove missing values. Defaults to \code{TRUE}.
#' @return A character string with results
#' @keywords misc
#' @export
#' @examples
#' formatMedIQR(mtcars$mpg)
formatMedIQR <- function(x, d = 2, na.rm = TRUE) {
 stats <- quantile(x, probs = c(.5, .25, .75), na.rm = na.rm)
 sprintf(fmt = sprintf("%%0.%df, (%%0.%df, %%0.%df)", d, d, d),
         stats[1], stats[2], stats[3])
}

#' Function to round and format a number
#'
#' @param x the data to round and format
#' @param digits the number of digits to used
#' @return a character vector
#' @export
.fround <- function(x, digits) {
  format(round(x, digits = digits), digits = digits, nsmall = digits)
}

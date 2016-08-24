#' A generic function for pretty printing in (semi) APA Style
#' @param object An object with a class matching one of the methods
#' @param ... Additional argiuments passed on to methods.
#' @export
APAStyler <- function(object, ...) {
  UseMethod("APAStyler")
}

#' A generic function for pretty printing in (semi) APA Style
#'
#' @param object \code{lm} object
#' @param digits The number of digits to round results to. Defaults to 2.
#' @param pdigits The number of digits to use for p values. Defaults to digits + 1 if missing.
#' @param file An optional argument indicating whether the output should be written to a file.
#' @param ... Additional argiuments passed on to \code{write.table}.
#' @importFrom stats coef confint
#' @importFrom utils write.table
#' @export
APAStyler.lm <- function(object, digits = 2, pdigits, file, ...) {
  if (missing(pdigits)) pdigits <- digits + 1

  s <- summary(object)
  ctable <- coef(s)
  cstars <- star(ctable[, "Pr(>|t|)"])
  ctable <- format(round(ctable, digits = digits),
                   trim = TRUE, nsmall = digits)
  est <- paste(ctable[, "Estimate"], cstars, " (",
               ctable[, "Std. Error"], ")", sep = "")

  ci <- format(round(confint(object, level = 0.95), digits = digits),
               nsmall = digits, trim = TRUE)
  ci <- paste("[", ci[, 1], ", ", ci[, 2], "]", sep = "")


  F <- s$fstatistic
  p <- pf(F[1], F[2], F[3], lower.tail = FALSE)

  r2 <- paste(format(round(s$r.squared, digits = digits),
                     nsmall = digits), star(p), sep = "")
  F <- format(round(F, digits = digits), nsmall = digits, trim = TRUE)

  if (identical(round(p[[1]], digits = pdigits), 0)) {
    p <- paste("p < .", paste(rep(0, pdigits - 1), collapse = ""),
               "1", sep = "")
  } else {p <- paste("p = ", format(round(p, digits = pdigits),
                                    nsmall = pdigits), sep = "")
  }

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
#' @export
APAStyler.mira <- function(object, lmobject, digits = 2, pdigits, file, ...) {
  if (!inherits(object[["analyses"]][[1]], "lm"))
    stop("mira object must use a model that inherits from class 'lm' such as 'lm' or 'glm'")

  if (missing(pdigits)) pdigits <- digits + 1
  F <- r2 <- NULL

  ctable <- summary(pool(object))
  cstars <- star(ctable[, "Pr(>|t|)"])
  ctable <- format(round(ctable, digits = digits),
                   trim = TRUE, nsmall = digits)

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

      r2 <- c(paste(format(round(r2, digits = digits),
                           nsmall = digits), star(p), sep = ""), "")

      F <- format(round(F, digits = digits), nsmall = digits, trim = TRUE)

      if (identical(round(p[[1]], digits = pdigits), 0)) {
        p <- paste("p < .", paste(rep(0, pdigits - 1), collapse = ""),
                   "1", sep = "")
      } else {p <- paste("p = ", format(round(p, digits = pdigits),
                                        nsmall = pdigits), sep = "")
      }

      F <- c(paste("F(", F[2], ", ", F[3], ") = ", F[1], sep = ""), p)
      warning("Using the degrees of freedom for the listwise deleted model for the F test and R^2 significance.",
              call. = FALSE)
    } else {
      r2 <- c(paste(format(round(r2, digits = digits),
                           nsmall = digits), sep = ""), "")
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
                  m <- format(round(object$Sigma, digits = digits), nsmall = digits)
                  if (stars) m[] <- paste0(m, star(object$pvalue))
                  m[lower.tri(m)] <- ""
                  diag(m) <- " - "
                  m
                },
                cor = {
                  m <- format(round(object$sSigma, digits = digits), nsmall = digits)
                  if (stars) m[] <- paste0(m, star(object$pvalue))
                  m[lower.tri(m)] <- ""
                  diag(m) <- " - "
                  m
                },
                both = {
                  mv <- format(round(object$Sigma, digits = digits), nsmall = digits)
                  mc <- format(round(object$sSigma, digits = digits), nsmall = digits)

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
  } else {
    stop("error, invalid combination of includeP and includeSign")
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
#' @author Joshua F. Wiley <josh@@elkhartgroup.com>
#' @export
#' @keywords utilities
#' @importFrom stats median sd
#' @examples
#'
#' param_summary(rnorm(100))
param_summary <- function(x, trans = function(x) x, ..., na.rm = TRUE) {
  data.frame(
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
#' @return A formatted data frame of summary statistics or a formated
#' vector (if \code{pretty = TRUE}).
#' @author Joshua F. Wiley <josh@@elkhartgroup.com>
#' @export
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
    out <- as.data.frame(lapply(d, function(x) {
      if (is.numeric(x)) {
        sprintf(sprintf("%%01.%df", digits), x)
      } else {
        x
      }
    }), stringsAsFactors = FALSE)
    rownames(out) <- rownames(d)
    }

  return(out)
}


#' Function to format the reuslts of a hypothesis test as text
#'
#' @param x A \code{htest} class object
#' @param type The type of htest. Currently one of: \dQuote{t}, \dQuote{F}, \dQuote{chisq},
#'   \dQuote{kw}, or \dQuote{mh} for t-tests, F-tests, chi-square tests, kruskal-wallis tests,
#'   and Mantel-Haenszel tests, respectively.
#' @param \dots Arguments passed on to p-value formatting
#' @return A character string with results
#' @keywords misc
#' @export
#' @examples
#' formatHtest(t.test(extra ~ group, data = sleep), type = "t")
#' formatHtest(anova(aov(mpg ~ factor(cyl), data = mtcars)), type = "F")
#' formatHtest(chisq.test(c(A = 20, B = 15, C = 25)), type = "chisq")
#' formatHtest(kruskal.test(Ozone ~ Month, data = airquality))
#' formatHtest(mantelhaen.test(UCBAdmissions), type = "mh")
formatHtest <- function(x, type = c("t", "F", "chisq", "kw", "mh"), ...) {
  type <- match.arg(type)

  switch(type,
         t = sprintf("t = %0.2f, df = %s, %s",
                     x$statistic,
                     as.character(round(x$parameter, 2)),
                     formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         F = sprintf("F(%s, %s) = %0.2f, p-value = %s",
                     as.character(round(x[1, "Df"], 2)),
                     as.character(round(x[2, "Df"], 2)),
                     x[1, "F value"],
                     formatPval(x[1, "Pr(>F)"], includeP = TRUE, includeSign = TRUE, ...)),
         chisq = sprintf("Chi-square = %s, df = %d, %s",
                         as.character(round(x$statistic, 2)),
                         x$parameter,
                         formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         kw = sprintf("Kruskal-Wallis chi-square = %s, df = %d, %s",
                      as.character(round(x$statistic, 2)),
                      x$parameter,
                      formatPval(x$p.value, includeP = TRUE, includeSign = TRUE, ...)),
         mh = sprintf("Mantel-Haenszel chi-square = %0.2f, df = %d, %s, common odds ratio = %0.2f, CI = (%0.2f, %0.2f).",
                      x$statistic, x$parameter,
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
  c(F = F[[1]], numdf, dendf, p = p[[1]])
}



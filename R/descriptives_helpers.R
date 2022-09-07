#' @name styletests
#' @rdname styletests
#'
#' @title Several internal functions to style inference tests
#'
#' @param dv An outcome variable
#' @param g A grouping/predictor variable
#' @param digits An integer indicating the number of significant digits to use.
#'   Defaults to \code{2}.
#' @param pdigits An integer indicating the number of digits for p values.
#'   Defaults to \code{3}.
#' @param simChisq A logical value, whether or not to simulate chi-square values.
#'   Only applies to some functions. Defaults to \code{FALSE}.
#' @param sims An integer indicating the number of simulations to conduct.
#'   Only applies to some functions. Defaults to \code{10000}, but this is
#'   arbitrary and should be chosen.
#' @keywords internal
#' @importFrom data.table data.table
#' @importFrom stats aov chisq.test kruskal.test quantile xtabs t.test
#' @importFrom stats wilcox.test mcnemar.test reshape
#' @return A character string of the formatted results.
NULL

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.styleaov(mtcars$mpg, mtcars$cyl)
.styleaov <- function(dv, g, digits = 2L, pdigits = 3L) {
  usedat <- data.table(dv = dv, g = g)
  tests <- summary(aov(dv ~ g, data = usedat))[[1]]
  es <- tests[1, "Sum Sq"] / sum(tests[, "Sum Sq"])
  outputstring <- sprintf("F(%%d, %%d) = %%0.%df, %%s, Eta-squared = %%0.%df",
                          digits, digits)
  sprintf(outputstring,
          tests[1, "Df"], tests[2, "Df"], tests[1, "F value"],
          formatPval(tests[1, "Pr(>F)"], d = pdigits, sd = pdigits, includeP = TRUE),
          es)
}

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.style2sttest(mtcars$mpg, mtcars$am)
.style2sttest <- function(dv, g, digits = 2, pdigits = 3) {
  usedat <- data.table(dv = dv, g = g)
  tests <- t.test(dv ~ g, data = usedat, var.equal = TRUE)
  es <- usedat[, smd(dv, g, "all")]

  outputstring <- sprintf("t(df=%%0.0f) = %%0.%df, %%s, d = %%0.%df",
                          digits, digits)

  sprintf(outputstring,
          tests$parameter[["df"]],
          tests$statistic[["t"]],
          formatPval(tests$p.value, d = pdigits, sd = pdigits, includeP = TRUE),
          es)
}

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.stylepairedttest(sleep$extra, sleep$group, sleep$ID)
.stylepairedttest <- function(dv, g, ID, digits = 2, pdigits = 3) {
  widedat <- copy(reshape(data.table(
    dv = dv,
    g = as.integer(factor(g)),
    ID = ID),
    v.names = "dv",
    timevar = "g",
    idvar = "ID",
    direction = "wide", sep = ""))
  widedat[, diff := dv2 - dv1]

  tests <- t.test(widedat$dv2, widedat$dv1, paired = TRUE)
  es <- mean(widedat$diff, na.rm = TRUE) / sd(widedat$diff, na.rm = TRUE)

  outputstring <- sprintf("t(df=%%0.0f) = %%0.%df, %%s, d = %%0.%df",
                          digits, digits)

  sprintf(outputstring,
          tests$parameter[["df"]],
          tests$statistic[["t"]],
          formatPval(tests$p.value, d = pdigits, sd = pdigits, includeP = TRUE),
          es)
}

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.stylepairedwilcox(sleep$extra, sleep$group, sleep$ID)
.stylepairedwilcox <- function(dv, g, ID, digits = 2, pdigits = 3) {
  widedat <- copy(reshape(data.table(
    dv = dv,
    g = as.integer(factor(g)),
    ID = ID),
    v.names = "dv",
    timevar = "g",
    idvar = "ID",
    direction = "wide", sep = ""))
  ## widedat[, diff := dv2 - dv1]

  tests <- wilcox.test(widedat$dv2, widedat$dv1, paired = TRUE)

  outputstring <- sprintf("Wilcoxon Paired V = %%0.%df, %%s",
                          digits)
  sprintf(outputstring,
          tests$statistic,
          formatPval(tests$p.value, d = pdigits, sd = pdigits, includeP = TRUE))
}

#' @rdname styletests
#' @examples
#'
#' ## example data
#' set.seed(1234)
#' exdata <- data.frame(
#'   ID = rep(1:10, 2),
#'   Time = rep(c("base", "post"), each = 10),
#'   Rating = sample(c("good", "bad"), size = 20, replace = TRUE))
#' JWileymisc:::.stylepairedmcnemar(exdata$Rating, exdata$Time, exdata$ID)
#' rm(exdata) ## cleanup
.stylepairedmcnemar <- function(dv, g, ID, digits = 2, pdigits = 3) {
  widedat <- copy(reshape(data.table(
    dv = dv,
    g = as.integer(factor(g)),
    ID = ID),
    v.names = "dv",
    timevar = "g",
    idvar = "ID",
    direction = "wide", sep = ""))

  tab <- xtabs(~ dv1 + dv2, data = widedat)

  tests <- mcnemar.test(tab)

  outputstring <- sprintf("McNemar's Chi-square = %%0.%df, df = %%d, %%s",
                          digits)
  sprintf(outputstring,
          tests$statistic, tests$parameter,
          formatPval(tests$p.value, d = pdigits, sd = pdigits, includeP = TRUE))
}

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.stylekruskal(mtcars$mpg, mtcars$am)
#' JWileymisc:::.stylekruskal(mtcars$mpg, mtcars$cyl)
.stylekruskal <- function(dv, g, digits = 2, pdigits = 3) {
  usedat <- data.table(dv = dv, g = g)
  tests <- kruskal.test(dv ~ g, data = usedat)

  outputstring <- sprintf("KW chi-square = %%0.%df, df = %%d, %%s",
                          digits)

  sprintf(outputstring,
          tests$statistic, tests$parameter,
          formatPval(tests$p.value, d = pdigits, sd = pdigits, includeP = TRUE))
}

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.stylechisq(mtcars$cyl, mtcars$am)
.stylechisq <- function(dv, g, digits = 2, pdigits = 3, simChisq = FALSE, sims = 1e4) {
  usedat <- data.table(dv = dv, g = g)
  tabs <- xtabs(~ dv + g, data = usedat)
  es <- cramerV(tabs)
  es <- sprintf(
    sprintf("%%s = %%0.%df", digits),
    names(es), es)

  tests <- chisq.test(tabs,
                      correct = FALSE,
                      simulate.p.value = simChisq,
                      B = sims)

  outputstring <- sprintf("Chi-square = %%0.%df, %%s, %%s, %%s",
                          digits)

  sprintf(outputstring,
          tests$statistic,
          ifelse(simChisq, "simulated", sprintf("df = %d", tests$parameter)),
          formatPval(tests$p.value, d = pdigits, sd = pdigits, includeP = TRUE),
          es)
}


#' @name styledescriptives
#' @rdname styledescriptives
#'
#' @title Several internal functions to style descriptive statistics
#'
#' @param n A character string giving the variable name (just the name)
#' @param x A variable (actual data, not just the name)
#' @param digits An integer indicating the number of significant digits to use.
#'   Defaults to \code{2}.
#' @param includeLabel A logical value, whether or not to include the type of
#'   descriptive statistics in the label/name.
#'   Only applies to some functions. Defaults to \code{FALSE}.
#' @keywords internal
#' @importFrom stats sd median IQR
#' @importFrom data.table data.table
#' @return A data table with results.
NULL

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.stylemsd("Miles per Gallon", mtcars$mpg)
#' JWileymisc:::.stylemsd("Miles per Gallon", mtcars$mpg, includeLabel = TRUE)
.stylemsd <- function(n, x, digits = 2, includeLabel = FALSE) {
  outputstring <- sprintf("%%0.%df (%%0.%df)", digits, digits)
  centralx <- mean(x, na.rm = TRUE)
  scalex   <- sd(x, na.rm = TRUE)

  data.table(
    Variable = sprintf(
      "%s%s", n,
      c("", ", M (SD)")[includeLabel + 1]),
    Res = sprintf(outputstring, centralx, scalex))
}


#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.stylemdniqr("Miles per Gallon", mtcars$mpg)
#' JWileymisc:::.stylemdniqr("Miles per Gallon", mtcars$mpg, includeLabel = TRUE)
.stylemdniqr <- function(n, x, digits = 2, includeLabel = FALSE) {
  outputstring <- sprintf("%%0.%df (%%0.%df)", digits, digits)
  centralx <- median(x, na.rm = TRUE)
  scalex   <- IQR(x, na.rm = TRUE)

  data.table(
    Variable = sprintf(
      "%s%s", n,
      c("", ", Mdn (IQR)")[includeLabel + 1]),
    Res = sprintf(outputstring, centralx, scalex))
}

#' @rdname styletests
#' @examples
#'
#' JWileymisc:::.stylefreq("Transmission", mtcars$am)
#' JWileymisc:::.stylefreq("Transmission", mtcars$am)
.stylefreq <- function(n, x) {
  freq <- table(x)
  perc <- prop.table(freq) * 100
  data.table(
    Variable = c(n, paste0("  ", names(freq))),
    Res = c("", sprintf("%d (%2.1f%%)", freq, perc)))
}

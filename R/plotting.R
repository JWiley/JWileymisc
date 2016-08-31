#' Plots SEMSummary object
#'
#' @param x An object of class SEMSummary.
#' @param y Ignored
#' @param \dots Additional arguments passed on to the real workhorse, \code{corplot}.
#' @method plot SEMSummary
#' @seealso \code{\link{corplot}}, \code{\link{SEMSummary}}
#' @importFrom graphics plot
#' @export
#' @examples
#' # default plot
#' plot(SEMSummary(~ ., data = mtcars))
#'
#' # same as default
#' plot(SEMSummary(~ ., data = mtcars), plot = "coverage")
#'
#' # shows p values
#' plot(SEMSummary(~ ., data = mtcars), plot = "p")
#'
#' # shows correlations
#' plot(SEMSummary(~ ., data = mtcars), plot = "cor")
plot.SEMSummary <- function(x, y, ...) {
  corplot(x = x$sSigma, coverage = x$coverage, pvalues = x$pvalue, ...)
}

#' Plots SEMSummary.list object
#'
#' @param x An object of class SEMSummary.list.
#' @param y Ignored
#' @param which either a numeric vector based on the positions,
#'   or a character vector giving the names of the levels of the
#'   list to plot.
#' @param \dots Additional arguments passed on to the real workhorse, \code{corplot}.
#' @method plot SEMSummary.list
#' @seealso \code{\link{corplot}}, \code{\link{SEMSummary}}
#' @importFrom cowplot plot_grid
#' @export
plot.SEMSummary.list <- function(x, y, which, ...) {

  n <- names(x)
  names(n) <- n
  n <- n[n != "Levels"]

  if (!missing(which)) {
    n <- n[which]
    if (length(n) == 1) {
      p <- corplot(x = x[[n]]$sSigma, coverage = x[[n]]$coverage, pvalues = x[[n]]$pvalue, ...) + ggtitle(n)
      print(p)
      return(invisible(p))
    }
  }

  p <- lapply(n, function(i) {
    corplot(x = x[[i]]$sSigma, coverage = x[[i]]$coverage, pvalues = x[[i]]$pvalue, ...) +
      ggtitle(i)
  })
  names(p) <- n

  nr <- NULL
  if (length(p) <= 3) {
    nr <- 1
  }

  do.call(plot_grid, c(p, list(nrow = nr)))

  return(invisible(p))
}

#' Heatmap of a Correlation Matrix
#'
#' This function creates a heatmap of a correlation matrix using \pkg{ggplot2}.
#'
#' The actual plot is created using \code{ggplot2} and \code{geom_tile}.
#' In addition to creating the plot, the variables are ordered based on a
#' hierarchical clustering of the correlation matrix.  Specifically, \code{1 - x}
#' is used as the distance matrix. If coverage is passed, will also add a bubble
#' plot with the area proportional to the proportion of data present for any
#' given cell.  Defaults for \code{ggplot2} are set, but it is possible to use a
#' named list of quote()d ggplot calls to override all defaults. This is not
#' expected for typical use.  Particularly main, points, and text as these rely
#' on internal variable names; however, labels, the gradient color, and area
#' scaling can be adjusted more safely.
#'
#' @param x A correlation matrix or some other square symmetric matrix.
#' @param coverage An (optional) matrix with the same dimensions as
#'   \code{x} giving the proportion of data present.  Particularly
#'   useful when the correlation matrix is a pairwise present.
#' @param pvalues An (optional) matrix with the same dimensions as
#'   \code{x} giving the p values for each correlation. To show, use
#'   \code{plot = "p"}.
#' @param plot A character string indicating what to show on top of the heatmap. Can be
#'   \sQuote{coverage}, in which case bubble points show coverage;
#'   \sQuote{p}, in which case p values are shown, or
#'   \sQuote{cor}, in which case correlations are shown.
#'   Only has an effect if a coverage (or pvalue) matrix is passed
#'   also. Defaults to \code{cor}.
#' @param digits The number of digits to round to when printing the
#'   correlations on the heatmap. Text is suppressed when a coverage
#'   matrix is passed and \code{points = TRUE}.
#' @param order A character string indicating how to order the resulting
#'   plot. Defaults to \sQuote{cluster} which uses hierarchical clustering
#'   to sensibly order the variables. The other option is \sQuote{asis}
#'   in which case the matrix is plotted in the order it is passed.
#' @param \dots Additional arguments currently only passed to
#'   \code{hclust} and \code{corOK}.
#' @param control.grobs A list of additional \code{quote()}d
#'   options to customize the \code{ggplot2} output.
#' @return Primarily called for the side effect of creating a plot.
#'   However, the \code{ggplot2} plot object is returned,
#'   so it can be saved, replotted, edited, etc.
#' @keywords hplot
#' @import ggplot2
#' @importFrom stats setNames as.dist hclust
#' @importFrom utils type.convert
#' @importFrom plyr amv_dimnames
#' @export
#' @examples
#' # example plotting the correlation matrix from the
#' # mtcars dataset
#' corplot(cor(mtcars))
#'
#' dat <- as.matrix(iris[, 1:4])
#'
#' # randomly set 25% of the data to missing
#' set.seed(10)
#' dat[sample(length(dat), length(dat) * .25)] <- NA
#'
#' # create a summary of the data (including coverage matrix)
#' sdat <- SEMSummary(~ ., data = dat)
#' # using the plot method for SEMSummary (which basically just calls corplot)
#' plot(sdat)
#'
#' # getting p values instaed of coverage
#' plot(sdat, plot = "p")
#'
#' # showing correlations instead of coverage
#' plot(sdat, plot = "cor")
#'
#' # use the control.grobs argument to adjust the coverage scaling
#' # to go from 0 to 1 rather than the range of coverage
#' corplot(x = sdat$sSigma, coverage = sdat$coverage,
#'   control.grobs = list(area = quote(scale_size_area(limits = c(0, 1))))
#' )
#'
#' # also works with plot() on a SEMSummary
#' plot(x = sdat, control.grobs = list(area = quote(scale_size_area(limits = c(0, 1)))))
#'
#' rm(dat, sdat)
corplot <- function(x, coverage, pvalues,
  plot = c("cor", "p", "coverage"),
  digits = 2, order = c("cluster", "asis"), ..., control.grobs = list()) {

  ## copied from reshape2 as otherwise creates clashes with depending on data.table package
  reshape2.melt.matrix <- function (data, varnames = names(dimnames(data)), ..., na.rm = FALSE,
                           as.is = FALSE, value.name = "value")
  {
    var.convert <- function(x) {
      if (!is.character(x))
        return(x)
      x <- type.convert(x, as.is = TRUE)
      if (!is.character(x))
        return(x)
      factor(x, levels = unique(x))
    }
    dn <- amv_dimnames(data)
    names(dn) <- varnames
    if (!as.is) {
      dn <- lapply(dn, var.convert)
    }
    labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    if (na.rm) {
      missing <- is.na(data)
      data <- data[!missing]
      labels <- labels[!missing, ]
    }
    value_df <- setNames(data.frame(as.vector(data)), value.name)
    cbind(labels, value_df)
  }

  order <- match.arg(order)
  plot <- match.arg(plot)

  stopifnot(is.matrix(x))
  if (!missing(coverage)) stopifnot(is.matrix(coverage))

  n <- switch(order,
    cluster = {
      ok <- corOK(x, ...)$keep.indices
      if (length(ok) > 0) {
        hc <- hclust(as.dist(1 - x[ok, ok]), ...)
        n <- colnames(x)[ok][hc$order]
      }
      if (!identical(length(ok), dim(x)[2L])) {
        n <- c(n, colnames(x)[-ok])
      }
      n
    },
    asis = colnames(x)
  )

  mx <- reshape2.melt.matrix(x, value.name = "r")
  mx$Var1 <- factor(mx[, "Var1"], levels = n)
  mx$Var2 <- factor(mx[, "Var2"], levels = n)
  mx$correlation <- gsub(".+\\.", ".", format(round(mx[, "r"],
    digits = digits), digits = digits, nsmall = digits))
  mx$correlation[mx[, "Var1"] == mx[, "Var2"]] <- ""
  if (!missing(coverage)) {
    mx$coverage <- reshape2.melt.matrix(coverage, value.name = "coverage")[, "coverage"]
  }

  if (!missing(pvalues)) {
    mx$pvalues <- reshape2.melt.matrix(pvalues, value.name = "p")[, "p"]
    ## mx$p <- gsub(".+\\.", ".", format.pval(round(mx[, "pvalues"],
    ##   digits = digits), digits = digits, nsmall = digits))
    mx$p <- gsub(".+\\.", ".", format.pval(mx[, "pvalues"],
      digits = digits, nsmall = digits))

    mx$p[mx[, "Var1"] == mx[, "Var2"]] <- ""
  }

  defaults <- list(
    main = quote(ggplot(mx, aes_string(x = "Var1", y = "Var2", fill = "r"))),
    tiles = quote(geom_tile()),
    labels = quote(labs(list(x = NULL, y = NULL))),
    gradient = quote(scale_fill_gradientn(name = "Correlation",
      guide = guide_colorbar(),
      colours = c("blue", "white", "red"), limits = c(-1, 1),
      breaks = c(-.99, -.5, 0, .5, .99), labels = c("-1", "-.5", "0", "+.5", "+1"))),
    area = quote(scale_size_area()),
    text = quote(geom_text(aes(label = correlation), size = 3, vjust = 0)))

  i <- names(defaults)[!names(defaults) %in% names(control.grobs)]
  control.grobs[i] <- defaults[i]

  p <- substitute(main + tiles + labels + gradient, control.grobs)

  if (identical(plot, "coverage") & !missing(coverage)) {
    control.grobs$points = quote(geom_point(aes(size = coverage)))
    p <- substitute(main + tiles + labels + gradient + points + area, control.grobs)
  } else if (identical(plot, "p") & !missing(pvalues)) {
    control.grobs$text = quote(geom_text(aes(label = p), size = 3, vjust = 0))
    p <- substitute(main + tiles + labels + gradient + text, control.grobs)
  } else {
    p <- substitute(main + tiles + labels + gradient + text, control.grobs)
  }

  eval(p)
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("X", "Y"))

#' Graphically compare the distribution of a variable against a specific distribution
#'
#' @param x The data as a single variable or vector to check the distribution.
#' @param distr A character string indicating the distribution to be tested.
#'   Currently one of: \dQuote{normal}, \dQuote{beta}, \dQuote{chisq} (chi-squared),
#'   \dQuote{f}, \dQuote{gamma}, \dQuote{nbinom} (negative binomial), or
#'   \dQuote{poisson}.
#' @param na.rm A logical value whether to omit missing values. Defaults to \code{TRUE}.
#' @param starts A named list of the starting values. Not required for all distributions.
#'   Passed on to \code{fitdistr} which fits the maximum likelihood estimates of the
#'   distribution parameters.
#' @param xlim An optional vector to control the x limits for the theoretical distribution
#'   density line, useful when densities become extreme at boundary values to help keep the
#'   scale of the graph reasonable.  Passed on to \code{stat_function}.
#' @param varlab A character vector the label to use for the variable
#' @param plot A logical vector whether to plot the graphs. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed on to \code{geom_density}
#' @return An invisible list with the ggplot2 objects for graphs,
#'   as well as information about the distribution (parameter estimates,
#'   name, log likelihood (useful for comparing the fit of different distributions
#'   to the data), and a dataset with the sorted data and theoretical quantiles.#'
#' @importFrom MASS fitdistr
#' @importFrom ggplot2 ggplot stat_function geom_density geom_point
#' @importFrom ggplot2 geom_abline ggtitle xlab ylab
#' @importFrom stats dnorm qnorm dbeta qbeta dchisq qchisq
#' @importFrom stats df qf dgamma qgamma dnbinom qnbinom dpois qpois
#' @importFrom stats logLik ppoints
#' @export
#' @keywords hplot
#' @examples
#'
#' ## example data
#' set.seed(1234)
#' d <- data.frame(
#'   Ynorm = rnorm(200),
#'   Ybeta = rbeta(200, 1, 4),
#'   Ychisq = rchisq(200, 8),
#'   Yf = rf(200, 5, 10),
#'   Ygamma = rgamma(200, 2, 2),
#'   Ynbinom = rnbinom(200, mu = 4, size = 9),
#'   Ypois = rpois(200, 4))
#'
#' ## testing and graphing
#' testdistr(d$Ynorm, "normal")
#' testdistr(d$Ybeta, "beta", starts = list(shape1 = 1, shape2 = 4))
#' testdistr(d$Ychisq, "chisq", starts = list(df = 8))
#' testdistr(d$Yf, "f", starts = list(df1 = 5, df2 = 10))
#' testdistr(d$Ygamma, "gamma")
#' testdistr(d$Ynbinom, "nbinom")
#' testdistr(d$Ypois, "poisson")
#'
#' ## compare log likelihood of two different distributions
#' testdistr(d$Ygamma, "normal")$Distribution$LL
#' testdistr(d$Ygamma, "gamma")$Distribution$LL
#'
#' rm(d) ## cleanup
testdistr <- function(x,
  distr = c("normal", "beta", "chisq", "f", "gamma", "nbinom", "poisson"),
  na.rm = TRUE, starts, xlim = NULL, varlab = "X", plot = TRUE, ...) {

  distr <- match.arg(distr)

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
      x <- na.omit(x)
    } else {
      stop("Missing values cannot be present when na.rm = FALSE")
    }
  }

  distribution <- switch(distr,
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
                           fit = fitdistr(x, "chi-squared", start = starts)),
                         f = list(
                           d = df,
                           q = qf,
                           Name = "F",
                           fit = fitdistr(x, "f", start = starts)),
                         gamma = list(
                           d = dgamma,
                           q = qgamma,
                           Name = "Gamma",
                           fit = fitdistr(x, "gamma")),
                         nbinom = list(
                           d = dnbinom,
                           q = qnbinom,
                           Name = "Negative Binomial",
                           fit = fitdistr(x, "negative binomial")),
                         poisson = list(
                           d = dpois,
                           q = qpois,
                           Name = "Poisson",
                           fit = fitdistr(x, "poisson")))

  distribution$LL <- logLik(distribution$fit)

  d <- data.table(
    X = do.call(distribution$q,
                c(list(p = ppoints(length(x))),
                  as.list(distribution$fit$estimate))),
    Y = sort(x))

  p.density <- ggplot(d, aes(Y)) +
    geom_density(...) +
    stat_function(fun = distribution$d,
                  args = as.list(distribution$fit$estimate),
                  colour = "blue", xlim = xlim) +
    xlab(varlab) + ylab("Density") +
    theme_cowplot() + ggtitle("Density Plot")

  p.qq <- ggplot(d, aes(X, Y)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    xlab(label = sprintf("Theoretical %s Quantiles", distribution$Name)) +
    ylab(label = varlab) +
    theme_cowplot() + ggtitle("Q-Q Plot")

  if (plot) {
    print(plot_grid(p.density, p.qq, ncol = 1, labels = c("A", "B")))
  }

  return(invisible(list(
    DensityPlot = p.density,
    QQPlot = p.qq,
    Data = d,
    Distribution = distribution)))
}


# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("D2", "ChiQuant"))

#' This is a simple plotting function designed to help examine
#' multivariate normality using the (squared) Mahalanobis distance.
#'
#' @param dat A data frame or matrix of multivariate data to be plotted
#' @param use A character vector indicating how the moments
#'   (means and covariance matrix) should be estimated in the presence of
#'   missing data.  The default is to use full information maximum likelihood
#'   based on functions in \pkg{lavaan}.
#' @param plot A logical argument whether to plot the results. Defaults to \code{TRUE}.
#' @return An invisible list of the density plot, QQ plot, and the data containing
#'   quantiles from the chi-squared distribution. Can be useful to find and remove
#'   multivariate outliers.
#' @seealso \code{\link{SEMSummary}}
#' @keywords multivariate
#' @importFrom stats mahalanobis qchisq ppoints
#' @importFrom cowplot theme_cowplot
#' @export
#' @examples
#' mvqq(mtcars)
#'
mvqq <- function(dat, use = c("fiml", "pairwise.complete.obs", "complete.obs"), plot = TRUE) {
  use <- match.arg(use)


  if (anyNA(dat)) {
    OK <- rowSums(is.na(dat)) == 0
  } else if (!anyNA(dat)) {
    use <- "complete.obs"
    OK <- rep(TRUE, nrow(dat))
  }

  desc <- switch(match.arg(use),
    fiml = {moments(dat)},
    pairwise.complete.obs = {
      list(mu = colMeans(dat, na.rm = TRUE),
        sigma = cov(dat, use = "pairwise.complete.obs"))
    },
    complete.obs = {
      list(mu = colMeans(dat[OK,]),
        sigma = cov(dat[OK,]))
    })

  d <- data.table(OK = OK)
  d[OK == TRUE,
    D2 := mahalanobis(dat[OK,], desc$mu, desc$sigma)]
  d[OK == TRUE,
    ChiQuant := qchisq(ppoints(.N), df = ncol(dat))[order(order(D2))]]

  p.density <- ggplot(d[OK==TRUE], aes_string(x = "D2")) +
    geom_density() +
    geom_rug() +
    xlab("Mahalanobis Distances") +
    ylab("Density") +
    ggtitle(sprintf("Mahalanobis Distances, n=%d, p=%d", sum(OK), ncol(dat))) +
    theme_cowplot()

  p.qq <- ggplot(d[OK==TRUE], aes_string(x = "D2", y = "ChiQuant")) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    xlab("Mahalanobis Distances") +
    ylab("Chi-square Quantiles") +
    ggtitle(eval(substitute(
      expression("Q-Q plot of Mahalanobis" * ~D^2 * " vs. quantiles of" * ~chi[df]^2),
      list(df = ncol(dat))))) +
    ## coord_equal() +
    theme_cowplot()

  if (plot) {
    print(plot_grid(p.density, p.qq, ncol = 2, align = "h"))
  }

  return(invisible(list(
    DensityPlot = p.density,
    QQPlot = p.qq,
    Data = d)))
}


# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("V1"))

#' Tukey HSD Plot
#'
#' This calculates and displays means, confidence intervals
#' as well as which groups are different based on Tukey's HSD.
#' Inspired by http://stackoverflow.com/questions/18771516/is-there-a-function-to-add-aov-post-hoc-testing-results-to-ggplot2-boxplot
#'
#' @param x X
#' @param y Y
#' @param d D
#' @param ci Confidence interval, defaults to .95
#' @param ordered Logical, defaults to \code{FALSE}.
#' @param \ldots Additional arguments passed on.
#' @return A ggplot graph object.
#' @importFrom stats TukeyHSD
#' @importFrom Hmisc smean.cl.normal
#' @importFrom plyr ddply
#' @importFrom multcompView multcompLetters
#' @keywords plot
#' @export
#' @examples
#' TukeyHSDgg("cyl", "mpg", mtcars)
#' TukeyHSDgg("Species", "Sepal.Length", iris, ci = .9)
#'
TukeyHSDgg <- function(x, y, d, ci = .95, ordered = FALSE, ...) {
  d <- droplevels(na.omit(as.data.frame(d)[, c(x, y)]))
  if (!is.factor(d[[x]])) {
    warning("x was not a factor, attempting to coerce")
    d[[x]] <- factor(d[[x]])
  }
  if (!(is.numeric(d[[y]]) || is.integer(d[[y]]))) {
    warning("y was not numeric or integer, attempting to coerce")
    d[[y]] <- as.numeric(as.character(d[[y]]))
  }

  fit <- aov(as.formula(sprintf("%s ~ %s", y, x)), data = d)
  tHSD <- TukeyHSD(fit, ordered = ordered, conf.level = ci)

  ## Extract labels and factor levels from Tukey post-hoc
  Tukey.levels.names <- rownames(tHSD[[x]][, 4, drop = FALSE])
  Tukey.levels <- tHSD[[x]][, 4]
  names(Tukey.levels) <- Tukey.levels.names

  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])

  ## Get the top of the CIs
  ## upper quantile and label placement
  y.df <- ddply(d, x, function(tmpd) mean_cl_normal(tmpd[[y]], conf.int = ci)$ymax)

  ## Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)

  ## Merge it with the labels
  labels.df <- merge(plot.levels, y.df, by.x = 'plot.labels', by.y = x, sort = FALSE)

  p <- ggplot(d, aes_string(x=x, y=y)) +
    stat_summary(fun.data = function(d) mean_cl_normal(d, conf.int = ci), ...) +
    geom_text(data = labels.df,
              aes(x = plot.labels,
                  y = V1 + (max(V1)*.05),
                  label = labels))

  return(p)
}

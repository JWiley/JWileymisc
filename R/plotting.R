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
#' plot(SEMSummary(~ ., data = mtcars), type = "coverage")
#'
#' # shows p values
#' plot(SEMSummary(~ ., data = mtcars), type = "p")
#'
#' # shows correlations
#' plot(SEMSummary(~ ., data = mtcars), type = "cor")
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
#' @param plot A logical, whether to actually plot the results or not.
#'   Defaults to \code{TRUE}.
#' @param \dots Additional arguments passed on to the real workhorse, \code{corplot}.
#' @method plot SEMSummary.list
#' @seealso \code{\link{corplot}}, \code{\link{SEMSummary}}
#' @importFrom cowplot plot_grid
#' @export
plot.SEMSummary.list <- function(x, y, which, plot = TRUE, ...) {

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


  if (plot) {
    if (length(p) > 1) {
      glegend <- get_legend(p[[1]])
      pnolegend <- lapply(p, function(x) x + theme(legend.position = "none"))
      nc <- ceiling(sqrt(length(p)))
      widths <- c(rep(1, length(p)), .3)
      print(do.call(plot_grid, c(pnolegend, list(glegend), list(ncol = nc, rel_widths = widths))))
    } else {
      print(p[[1]])
    }
  }

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
#' @param type A character string indicating what to show on top of the heatmap. Can be
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
#' @importFrom ggplot2 ggtitle
#' @importFrom stats setNames as.dist hclust
#' @importFrom utils type.convert
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
#' ## getting p values instaed of coverage
#' # plot(sdat, type = "p")
#'
#' ## showing correlations instead of coverage
#' # plot(sdat, type = "cor")
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
  type = c("cor", "p", "coverage"),
  digits = 2, order = c("cluster", "asis"), ..., control.grobs = list()) {

  ## copied and revised from reshape2 as otherwise creates clashes with depending on data.table package
  reshape2.melt.matrix <- function (data, varnames = names(dimnames(data)), ..., value.name = "value")  {
    var.convert <- function(x) {
      if (!is.character(x)) return(x)
      x <- type.convert(x, as.is = TRUE)
      if (!is.character(x)) return(x)
      factor(x, levels = unique(x))
    }

    dn <- list(
      if (is.null(dimnames(data)[[1]])) 1:nrow(data) else dimnames(data)[[1]],
      if (is.null(dimnames(data)[[2]])) paste0("V", 1:ncol(data)) else dimnames(data)[[2]])

    names(dn) <- varnames
    dn <- lapply(dn, var.convert)

    labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

    value_df <- setNames(data.frame(as.vector(data)), value.name)
    cbind(labels, value_df)
  }

  order <- match.arg(order)
  type <- match.arg(type)

  stopifnot(is.matrix(x))
  if (!missing(coverage)) stopifnot(is.matrix(coverage))

  n <- switch(order,
    cluster = {
      ok <- corOK(x, ...)$keep.indices
      if (length(ok) > 2) {
        hc <- hclust(as.dist(1 - x[ok, ok]), ...)
        n <- colnames(x)[ok][hc$order]
      } else {
        n <- colnames(x)[ok]
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
  ## deal with cases where correlations are 1
  mx$correlation[mx$r == 1] <- "1"
  ## set diagonals to blank
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
    gradient = quote(scale_fill_gradientn(name = "Correlation",
      guide = guide_colorbar(),
      colours = c("blue", "white", "red"), limits = c(-1, 1),
      breaks = c(-.99, -.5, 0, .5, .99), labels = c("-1", "-.5", "0", "+.5", "+1"))),
    area = quote(scale_size_area()),
    text = quote(geom_text(aes(label = correlation), size = 3, vjust = 0)),
    theme = quote(theme(axis.title = element_blank())))

  i <- names(defaults)[!names(defaults) %in% names(control.grobs)]
  control.grobs[i] <- defaults[i]

  p <- substitute(main + tiles + gradient + theme, control.grobs)

  if (identical(type, "coverage") & !missing(coverage)) {
    control.grobs$points = quote(geom_point(aes(size = coverage)))
    p <- substitute(main + tiles + gradient + points + area + theme, control.grobs)
  } else if (identical(type, "p") & !missing(pvalues)) {
    control.grobs$text = quote(geom_text(aes(label = p), size = 3, vjust = 0))
    p <- substitute(main + tiles + gradient + text + theme, control.grobs)
  } else {
    p <- substitute(main + tiles + gradient + text + theme, control.grobs)
  }

  eval(p)
}


#' Creates a plot for likert scale
#'
#' @param x Variable to plot on the x axis (the likert scale responses or averages)
#' @param y The variable containing an index of the different items, should be integers
#' @param leftLab The variable with anchors for the low end of the Likert scale
#' @param rightLab The variable with anchors for the high end of the Likert scale
#' @param colour  A character string giving the name of a variable for colouring the data, like a grouping variable. Alternately the colour of points passed to \code{\link{geom_point}}
#' @param data The data to use for plotting
#' @param xlim A vector giving the lower an upper limit for the x axis.  This should be the
#'   possible range of the Likert scale, not the actual range.
#' @param title A character vector giving the title for the plot
#' @param shape A number indicating the point shape, passed to \code{\link{geom_point}}
#' @param size  A number indicating the size of points, passed to \code{\link{geom_point}}
#' @importFrom ggplot2 ggplot aes_string geom_point scale_y_reverse dup_axis
#' @importFrom ggplot2 theme element_line element_blank element_text coord_cartesian ggtitle
#' @importFrom cowplot theme_cowplot plot_grid get_legend
#' @export
#' @examples
#'
#' testdat <- data.table::data.table(
#'   Var = 1:4,
#'   Mean = c(1.5, 3, 2.2, 4.6),
#'   Low = c("Happy", "Peaceful", "Excited", "Content"),
#'   High = c("Sad", "Angry", "Hopeless", "Anxious"))
#'
#' gglikert("Mean", "Var", "Low", "High", data = testdat, xlim = c(1, 5),
#'   title = "Example Plot of Average Affect Ratings")
#'
#' testdat <- rbind(
#'   cbind(testdat, Group = "Young"),
#'   cbind(testdat, Group = "Old"))
#' testdat$Mean[5:8] <- c(1.7, 2.6, 2.0, 4.4)
#'
#' gglikert("Mean", "Var", "Low", "High", colour = "Group",
#'   data = testdat, xlim = c(1, 5),
#'   title = "Example Plot of Average Affect Ratings")
#'
#' gglikert("Mean", "Var", "Low", "High", colour = "Group",
#'   data = testdat, xlim = c(1, 5),
#'   title = "Example Plot of Average Affect Ratings") +
#' ggplot2::scale_colour_manual(values = c("Young" = "grey50", "Old" = "black"))
#'
#' ## clean up
#' rm(testdat)
gglikert <- function(x, y, leftLab, rightLab, colour, data, xlim, title,
                     shape = 18, size = 7) {
  stopifnot(is.character(data[[leftLab]]))
  stopifnot(is.character(data[[rightLab]]))

  if (!(is.numeric(data[[y]]) || is.integer(data[[y]]))) {
    data[[y]] <- as.numeric(data[[y]])
    message(sprintf("attempting to coerce %s to numeric", y))
  }

  index <- !duplicated(data[[y]])
  databreaks <- data[[y]][index]

  if (missing(colour)) {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point(shape = shape, size = size, colour = "grey50")
  } else if (colour %in% names(data)) {
    p <- ggplot(data, aes_string(x = x, y = y, colour = colour)) +
      geom_point(shape = shape, size = size)
  } else {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point(shape = shape, size = size, colour = colour)
  }

  p + scale_y_reverse("",
      breaks = databreaks, labels = data[[leftLab]][index],
      sec.axis = dup_axis(
        breaks = databreaks,
        labels = data[[rightLab]][index])) +
    theme_cowplot() +
    theme(
      panel.grid.major.y = element_line(size = 1),
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(hjust = 0),
      axis.text.y.right = element_text(hjust = 1)) +
    coord_cartesian(
      xlim = xlim,
      ylim = c(min(data[[y]]) - .5, max(data[[y]]) + .5),
      expand = FALSE) +
    ggtitle(title)
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
           c("X", "Y", "isEV", "YDeviates", "..count..", "variable",
             "V1", "value"))
}

#' Plot method for testDistribution objects
#'
#' Make plots of testDistribution objects, including
#' density and QQ plots.
#'
#' @param x A list of class \dQuote{testDistribution}.
#' @param y Included to match the generic. Not used.
#' @param xlim An optional vector to control the x limits for the theoretical distribution
#'   density line, useful when densities become extreme at boundary values to help keep the
#'   scale of the graph reasonable.  Passed on to \code{stat_function}.
#' @param varlab A character vector the label to use for the variable
#' @param plot A logical vector whether to plot the graphs. Defaults to \code{TRUE}.
#' @param rugthreshold Integer determining the number of observations beyond
#'   which no rug plot is added. Note that even if this threshold is exceeded,
#'   a rug plot will still be added for any extreme values (if extreme values are
#'   used and present).
#' @param seed a random seed used to make the jitter added for Poisson and
#'   Negative Binomial distributions reproducible
#' @param factor A scale factor fo the amount of jitter added to the QQ and Deviates
#'   plots for Poisson and Negative Binomial distributions. Defaults to 1.
#'   This results in 1 * smallest distance between points / 5 being used.
#' @param ... Additional arguments.
#' @return An invisible list with the ggplot2 objects for graphs,
#'   as well as information about the distribution (parameter estimates,
#'   name, log likelihood (useful for comparing the fit of different distributions
#'   to the data), and a dataset with the sorted data and theoretical quantiles.
#' @importFrom ggplot2 ggplot stat_function geom_density geom_point
#' @importFrom ggplot2 geom_abline ggtitle xlab ylab
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme ggtitle
#' @importFrom ggplot2 element_text element_line
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom data.table melt as.data.table setnames
#' @importFrom robustbase covMcd
#' @seealso \code{\link{testDistribution}}
#' @method plot testDistribution
#' @export
#' @keywords hplot multivariate
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
#' plot(testDistribution(d$Ybeta, "beta", starts = list(shape1 = 1, shape2 = 4)))
#' plot(testDistribution(d$Ychisq, "chisq", starts = list(df = 8)))
#'
#' ## for chi-square distribution, extreme values only on
#' ## the right tail
#' plot(testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
#'   extremevalues = "empirical", ev.perc = .1))
#' plot(testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
#'   extremevalues = "theoretical", ev.perc = .1))
#'
#' plot(testDistribution(d$Yf, "f", starts = list(df1 = 5, df2 = 10)))
#' plot(testDistribution(d$Ygamma, "gamma"))
#' plot(testDistribution(d$Ynbinom, "poisson"))
#' plot(testDistribution(d$Ynbinom, "nbinom"))
#' plot(testDistribution(d$Ypois, "poisson"))
#'
#' ## compare log likelihood of two different distributions
#' testDistribution(d$Ygamma, "normal")$Distribution$LL
#' testDistribution(d$Ygamma, "gamma")$Distribution$LL
#'
#' plot(testDistribution(d$Ynorm, "normal"))
#' plot(testDistribution(c(d$Ynorm, 10, 1000), "normal",
#'   extremevalues = "theoretical"))
#' plot(testDistribution(c(d$Ynorm, 10, 1000), "normal",
#'   extremevalues = "theoretical", robust = TRUE))
#'
#' plot(testDistribution(mtcars, "mvnormal"))
#'
#' ## for multivariate normal mahalanobis distance
#' ## which follows a chi-square distribution, extreme values only on
#' ## the right tail
#' plot(testDistribution(mtcars, "mvnormal", extremevalues = "empirical",
#'   ev.perc = .1))
#' plot(testDistribution(mtcars, "mvnormal", extremevalues = "theoretical",
#'   ev.perc = .1))
#'
#' rm(d) ## cleanup
#' }
plot.testDistribution <- function(x, y, xlim = NULL, varlab = "X", plot = TRUE,
                                  rugthreshold = 500, seed = 1234, factor = 1, ...) {

  if (x$distr %in% c("poisson", "nbinom")) {
    tmpd <- as.data.table(prop.table(table(x$Data$Y)))
    tmpd[, V1 := as.numeric(V1)]
    tmpd$Density <- do.call(x$Distribution$d,
                            c(
                              list(x = tmpd$V1),
                              as.list(x$Distribution$fit$estimate)))
    setnames(tmpd, old = c("N", "Density"), new = c("Observed", "Expected"))
    tmpd <- melt(tmpd, id.vars = "V1")

    p.density <- ggplot(tmpd) +
      geom_col(aes(V1, value, fill = variable),
               position = "dodge", width = .3) +
      scale_fill_manual("", values = c(
                              "Observed" = "grey30",
                              "Expected" = "blue"))
  } else {
    p.density <- ggplot(x$Data, aes(Y)) +
      geom_density() +
      stat_function(fun = x$Distribution$d,
                    args = as.list(x$Distribution$fit$estimate),
                    colour = "blue", linetype = 2, size = 1, xlim = xlim)
  }
  p.density <- p.density +
    geom_rug(aes(x = Y, colour = isEV),
             data = x$Data[isEV == "Yes" | (x$NOK < rugthreshold)],
             alpha = pmax(pmin(1 / sqrt(log10(x$NOK)), 1), as.integer(x$NOK >= rugthreshold))) +
    scale_colour_manual(values = c("No" = "grey70", "Yes" = "black")) +
    ylab("Density") +
    scale_x_continuous(breaks = roundedfivenum(x$Data$Y)) +
    geom_rangeframe() +
    theme_tufte(base_family = "sans") +
    theme(
      legend.position = "none",
      axis.text = element_text(colour = "black"),
      axis.ticks = element_line(colour = "white", size = 2))

  if (identical(x$distr, "mvnormal")) {
    p.density <- p.density +
      xlab(sprintf("Mahalanobis Distances, p=%d", attr(x$Distribution$LL, "df"))) +
      ggtitle("Density Plot (Chi-squared)")
  } else {
    p.density <- p.density +
      xlab(varlab) +
      ggtitle(sprintf("Density Plot (%s)\nLL(df = %d) = %0.2f",
                      x$Distribution$Name,
                      attr(x$Distribution$LL, "df"),
                      x$Distribution$LL))
  }

  ## if categorical, jitter values slightly to reduce overplotting
  ## by factor * smallest distance / 5
  if (x$distr %in% c("poisson", "nbinom")) {
    origx <- x
    set.seed(seed)
    x$Data$X <- jitter(x$Data$X, factor = factor)
    x$Data$Y <- jitter(x$Data$Y, factor = factor)
    x$Data$YDeviates <- jitter(x$Data$YDeviates, factor = factor)
  }

  p.qq <- ggplot(x$Data, aes(X, Y)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(colour = isEV)) +
    scale_colour_manual(values = c("No" = "grey70", "Yes" = "black")) +
    xlab(label = "Theoretical Quantiles") +
    scale_x_continuous(breaks = roundedfivenum(x$Data$X)) +
    scale_y_continuous(breaks = roundedfivenum(x$Data$Y)) +
    geom_rangeframe() +
    theme_tufte(base_family = "sans") +
    theme(
      legend.position = "none",
      axis.text = element_text(colour = "black"))

  if (identical(x$distr, "mvnormal")) {
    p.qq <- p.qq +
      ylab(sprintf("Mahalanobis Distances, p=%d", attr(x$Distribution$LL, "df"))) +
      ggtitle("Q-Q Plot (Chi-squared)")
  } else {
    p.qq <- p.qq +
      ylab(label = varlab) +
      ggtitle(sprintf("Q-Q Plot (%s)\nLL(df = %d) = %0.2f",
                      x$Distribution$Name,
                      attr(x$Distribution$LL, "df"),
                      x$Distribution$LL))
  }

  p.qqdeviates <- ggplot(x$Data, aes(X, YDeviates)) +
    geom_point(aes(colour = isEV)) +
    scale_colour_manual(values = c("No" = "grey70", "Yes" = "black")) +
    geom_hline(yintercept = 0) +
    ylab("Deviates") +
    theme_tufte(base_family = "sans") +
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")

  if (plot) {
    print(plot_grid(p.density, p.qqdeviates,
                    ncol = 1, align = "v",
                    rel_heights = c(3, 1)))
  }

  return(invisible(list(
    testDistribution = if (x$distr %in% c("poisson", "nbinom")) origx else x,
    DensityPlot = p.density,
    QQPlot = p.qq,
    QQDeviatesPlot = p.qqdeviates)))
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("ymax", ".", "upper.CL", "Letters"))

#' Tukey HSD Plot
#'
#' This calculates and displays means, confidence intervals
#' as well as which groups are different based on Tukey's HSD.
#' Inspired by http://stackoverflow.com/questions/18771516/is-there-a-function-to-add-aov-post-hoc-testing-results-to-ggplot2-boxplot
#'
#' @param x A categorical grouping variable name.
#' @param y A continuous outcome variable name.
#' @param d A dataset
#' @param ci A numeric value indicating the coverage of the
#'   confidence interval to use.  Defaults to 0.95.
#' @param idvar An optional ID variable for multilevel data
#' @param \ldots Additional arguments passed on.
#' @return A ggplot graph object.
#' @importFrom stats as.formula
#' @importFrom lme4 lmer
#' @importFrom emmeans emmeans
#' @importFrom multcompView multcompLetters
#' @keywords plot
#' @export
#' @examples
#'
#' ## examples using it with single level data
#' ## differences based on an ANOVA and follow up contrasts
#' mtcars$cyl <- factor(mtcars$cyl)
#' TukeyHSDgg("cyl", "mpg", mtcars)
#' rm(mtcars)
#'
#' \dontrun{
#' TukeyHSDgg("Species", "Sepal.Length", iris)
#'
#' ## example based on multilevel data
#' ## differences based on model fit with lmer and follow up contrasts
#' TukeyHSDgg("treatment", "decrease", OrchardSprays, idvar = "colpos")
#' }
TukeyHSDgg <- function(x, y, d, ci = .95, idvar, ...) {
  if (missing(idvar)) {
    d <- droplevels(na.omit(as.data.frame(d)[, c(x, y)]))
  } else {
    d <- droplevels(na.omit(as.data.frame(d)[, c(x, y, idvar)]))
  }
  if (!is.factor(d[[x]])) {
    warning("x was not a factor, attempting to coerce")
    d[[x]] <- factor(d[[x]])
  }
  if (!(is.numeric(d[[y]]) || is.integer(d[[y]]))) {
    warning("y was not numeric or integer, attempting to coerce")
    d[[y]] <- as.numeric(as.character(d[[y]]))
  }

  if (missing(idvar)) {
    fit <- aov(as.formula(sprintf("%s ~ %s", y, x)), data = d)
  } else {
    fit <- lmer(as.formula(sprintf("%s ~ %s + (1 | %s)", y, x, idvar)), data = d)
  }
  tHSD <- emmeans(fit, specs = as.formula(sprintf("pairwise ~ %s", x)))
  tHSDs <- summary(tHSD)

  ## Extract labels and factor levels from Tukey post-hoc
  Tukey.levels.names <- tHSDs[["contrasts"]][, "contrast"]
  Tukey.levels <- tHSDs[["contrasts"]][, "p.value"]
  names(Tukey.levels) <- gsub("\\s", "", Tukey.levels.names)

  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  plot.labels <- gsub("[ \t]$", "", gsub("^(\\s)*(.*)$", "\\2", plot.labels))

  thsdsem <- tHSDs$emmeans
  thsdsem[[x]] <- gsub("\\s", "", thsdsem[[x]])

  plotdf <- merge(
    thsdsem,
    data.frame(Labels = plot.labels, Letters = Tukey.labels),
    by.x = x, by.y = "Labels")

  p <- ggplot(plotdf, aes_string(x=x, y="emmean", ymin = "lower.CL", ymax = "upper.CL")) +
    geom_pointrange() + geom_point() +
    geom_text(aes(y = upper.CL + (max(upper.CL) * .05),
                  label = Letters))

  return(p)
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("EffectType", "OriginalOrder"))

## clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("Predicted", "Residuals",
                                                        "LL", "UL"))


#' Plot Diagnostics for an lm model
#'
#' This function creates a number of diagnostic plots
#' from lm models. It relies heavily on the \code{testdistr}
#' function.
#'
#' @param x A fitted model object from lm.
#' @param y Included to match the generic. Not used.
#' @param plot A logical value whether or not to plot the results or
#'   simply return the graaphical  objects.
#' @param ask A logical whether to ask before changing plots.
#'   Only applies to interactive environments.
#' @param ncol The number of columns to use for plots.
#'   Missing by default which means individual plots are created.
#'   If specified, plots are put together in a grid.
#' @param ... Included to match the generic. Not used.
#' @return a list including plots of the residuals,
#'   residuals versus fitted values, and one list for
#'   plots of all random effects and finally a data table with
#'   any extreme values identified
#'
#' @importFrom grDevices dev.interactive devAskNewPage
#' @importFrom ggplot2 ggtitle theme geom_quantile stat_smooth
#' @importFrom ggplot2 geom_point geom_bin2d scale_fill_gradient scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 element_text element_line
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom cowplot plot_grid
#' @keywords plot
#' @method plot modelDiagnostics.lm
#' @export
#' @examples
#' testm <- stats::lm(mpg ~ hp * factor(cyl), data = mtcars)
#'
#' md <- modelDiagnostics(testm, ev.perc = .1)
#'
#' plot(md)
#' plot(md, ncol = 2)
#'
#' ## clean up
#' rm(testm, md)
plot.modelDiagnostics.lm <- function(x, y, plot = TRUE, ask = TRUE, ncol, ...) {
  ## residuals versus fitted
  p.resfit <- ggplot(x$residualDiagnostics$Residuals, aes(Predicted, Residuals))
  if (x$residualDiagnostics$N < 500) {
    p.resfit <- p.resfit +
      geom_point(aes(colour = isEV),
        alpha = pmin(1 / sqrt(log10(x$residualDiagnostics$N)), 1)) +
      scale_colour_manual(values = c("No" = "grey50", "Yes" = "black")) +
      guides(colour = "none")
  } else {
    p.resfit <- p.resfit +
      geom_bin2d(aes(fill = ..count..), bins = 80) +
      scale_fill_gradient(low = "grey70", high = "black")
  }

  p.resfit <- p.resfit +
    stat_smooth(method = "loess", se = FALSE, size = 1, colour = "blue") +
    geom_rangeframe() +
    scale_x_continuous(breaks = roundedfivenum(x$residualDiagnostics$Residuals$Predicted)) +
    scale_y_continuous(breaks = roundedfivenum(x$residualDiagnostics$Residuals$Residuals)) +
    theme_tufte(base_family = "sans") +
    theme(
      legend.position = "bottom",
      axis.text = element_text(colour = "black"),
      axis.ticks.x = element_line(colour = "white", size = 2)) +
    ggtitle(x$residualDiagnostics$Outcome)

  if (any(!is.na(x$residualDiagnostics$Hat$LL) |
          !is.na(x$residualDiagnostics$Hat$UL))) {
    p.resfit <- p.resfit +
      geom_line(mapping = aes(x = Predicted, y = LL),
                data = x$residualDiagnostics$Hat,
                colour = "blue", size = 1, linetype = 2) +
      geom_line(mapping = aes(x = Predicted, y = UL),
                data = x$residualDiagnostics$Hat,
                colour = "blue", size = 1, linetype = 2)
  }

  ## distributions of residuals
  p.tmpres <- plot(x$residualDiagnostics$testDistribution,
                   varlab = "Residuals",
                   plot = FALSE)

  p.res <- plot_grid(
    p.tmpres$DensityPlot + ggtitle(x$residualDiagnostics$Outcome),
    p.tmpres$QQDeviatesPlot, ncol = 1,
    rel_heights = c(3, 1), align = "v")


  if (plot) {
    if (ask && dev.interactive()) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    if (!missing(ncol)) {
      print(plot_grid(
        p.res,
        p.resfit,
        ncol = ncol))
    } else {
      print(p.res)
      print(p.resfit)
    }
  }

  return(invisible(list(
    ResPlot = p.res,
    ResFittedPlot = p.resfit)))
}


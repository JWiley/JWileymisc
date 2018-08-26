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
#' # plot(sdat, plot = "p")
#'
#' ## showing correlations instead of coverage
#' # plot(sdat, plot = "cor")
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

  ## copied and revised from reshape2 as otherwise creates clashes with depending on data.table package
  reshape2.melt.matrix <- function (data, varnames = names(dimnames(data)), ..., value.name = "value")  {
    var.convert <- function(x) {
      if (!is.character(x))
        return(x)
      x <- type.convert(x, as.is = TRUE)
      if (!is.character(x))
        return(x)
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
#' @importFrom cowplot theme_cowplot
#' @export
#' @examples
#'
#' testdat <- data.frame(
#'   Var = 1:4,
#'   Mean = c(1.5, 3, 2.2, 4.6),
#'   Low = c("Happy", "Peaceful", "Excited", "Content"),
#'   High = c("Sad", "Angry", "Hopeless", "Anxious"),
#'   stringsAsFactors = FALSE)
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
#' scale_colour_manual(values = c("Young" = "grey50", "Old" = "black"))
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
if(getRversion() >= "2.15.1")  utils::globalVariables(c("X", "Y", "isEV", "YDeviates", "..count.."))

#' Graphically compare the distribution of a variable against a specific distribution
#'
#' This is a simple plotting function designed to help examine distributions.
#' It also includes an option for assessing multivariate normality using the
#' (squared) Mahalanobis distance.
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
#' @param xlim An optional vector to control the x limits for the theoretical distribution
#'   density line, useful when densities become extreme at boundary values to help keep the
#'   scale of the graph reasonable.  Passed on to \code{stat_function}.
#' @param varlab A character vector the label to use for the variable
#' @param plot A logical vector whether to plot the graphs. Defaults to \code{TRUE}.
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
#' @param rugthreshold Integer determining the number of observations beyond
#'   which no rug plot is added. Note that even if this threshold is exceeded,
#'   a rug plot will still be added for any extreme values (if extreme values are
#'   used and present).
#' @param ... Additional arguments. If these include mu and sigma and the distribution
#'   is multivariate normal, then it will use the passed values instead of calculating
#'   the mean and covariances of the data.
#' @return An invisible list with the ggplot2 objects for graphs,
#'   as well as information about the distribution (parameter estimates,
#'   name, log likelihood (useful for comparing the fit of different distributions
#'   to the data), and a dataset with the sorted data and theoretical quantiles.#'
#' @importFrom MASS fitdistr
#' @importFrom ggplot2 ggplot stat_function geom_density geom_point
#' @importFrom ggplot2 geom_abline ggtitle xlab ylab
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme ggtitle
#' @importFrom ggplot2 element_text element_line
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom stats dnorm qnorm dbeta qbeta dchisq qchisq
#' @importFrom stats df qf dgamma qgamma dnbinom qnbinom dpois qpois
#' @importFrom stats logLik ppoints
#' @importFrom stats mahalanobis qchisq ppoints
#' @importFrom stats lm resid offset
#' @importFrom data.table %inrange% %between%
#' @importFrom robustbase covMcd
#' @seealso \code{\link{SEMSummary}}
#' @export
#' @keywords hplot multivariate
#' @examples
#'
#' \dontrun{
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
#' testdistr(d$Ynorm, "normal")
#' testdistr(c(d$Ynorm, 10, 1000), "normal",
#'   extremevalues = "theoretical")
#' testdistr(c(d$Ynorm, 10, 1000), "normal",
#'   extremevalues = "theoretical", robust = TRUE)
#'
#' testdistr(mtcars, "mvnormal")
#'
#' rm(d) ## cleanup
#' }
testdistr <- function(x,
  distr = c("normal", "beta", "chisq", "f", "gamma", "nbinom", "poisson", "mvnormal"),
  na.rm = TRUE, starts, xlim = NULL, varlab = "X", plot = TRUE,
  extremevalues = c("no", "theoretical", "empirical"), ev.perc = .005,
  use = c("complete.obs", "pairwise.complete.obs", "fiml"),
  robust = FALSE, rugthreshold = 500, ...) {

  distr <- match.arg(distr)
  use <- match.arg(use)
  if (use != "complete.obs" & isTRUE(robust)) {
    use <- "complete.obs"
    message("use set to 'complete.obs' as robust = TRUE")
  }

  extremevalues <- match.arg(extremevalues)
  stopifnot(ev.perc %inrange% c(0L, 1L))

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
        desc <- switch(match.arg(use),
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
    ## five percent increase per iteration, up to 10 iterations
    i <- 1L
    while(isTRUE(tryCatch(solve(desc$sigma), error = function(e) TRUE)) && i <= 10) {
      diag(desc$sigma) <- diag(desc$sigma) * 1.05
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
      x <- na.omit(x)
    } else {
      stop("Missing values cannot be present when na.rm = FALSE")
    }
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
        loglik = sum(dnorm(x, estimate["mean"], estimate["sd"],
                           log = TRUE))), class = "fitdistr"))
    rm(estimate)
  } else {

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
                             fit = fitdistr(x, "poisson")),
                           mvnormal = list(
                             d = dchisq,
                             q = qchisq,
                             Name = "Chi-squared",
                             fit = list(estimate = list(df = starts$df))))
  }

  if (!identical(distr, "mvnormal")) {
    distribution$LL <- logLik(distribution$fit)
  }

  d <- data.table(
    X = do.call(distribution$q,
                c(list(p = ppoints(length(x))),
                  as.list(distribution$fit$estimate))),
    Y = sort(x),
    OriginalOrder = order(x))

  ev.limits <- switch(extremevalues,
                      no = c(-Inf, Inf),
                      empirical = quantile(x, probs = c(ev.perc, 1 - ev.perc), na.rm = TRUE),
                      theoretical = do.call(distribution$q,
                                            c(list(p = c(ev.perc, 1 - ev.perc)),
                                              as.list(distribution$fit$estimate))))

  d[, isEV := factor(Y %inrange% ev.limits, levels = c(TRUE, FALSE), labels = c("No", "Yes"))]

  nok <- sum(!is.na(d$Y))

  p.density <- ggplot(d, aes(Y)) +
    geom_density() +
    stat_function(fun = distribution$d,
                  args = as.list(distribution$fit$estimate),
                  colour = "blue", linetype = 2, size = 1, xlim = xlim) +
    geom_rug(aes(colour = isEV),
             data = d[isEV == "Yes" | (nok < rugthreshold)],
             alpha = pmax(pmin(1 / sqrt(log10(nok)), 1), as.integer(nok >= rugthreshold))) +
    scale_colour_manual(values = c("No" = "grey70", "Yes" = "black")) +
    ylab("Density") +
    scale_x_continuous(breaks = roundedfivenum(d$Y)) +
    geom_rangeframe() +
    theme_tufte(base_family = "sans") +
    theme(
      legend.position = "none",
      axis.text = element_text(colour = "black"),
      axis.ticks = element_line(colour = "white", size = 2))

  if (identical(distr, "mvnormal")) {
    p.density <- p.density +
      xlab(sprintf("Mahalanobis Distances, p=%d", starts$df)) +
      ggtitle("Density Plot (Chi-squared)")
  } else {
    p.density <- p.density +
      xlab(varlab) +
      ggtitle(sprintf("Density Plot (%s)\nLL(df = %d) = %0.2f",
                      distribution$Name,
                      attr(distribution$LL, "df"),
                      distribution$LL))
  }

  p.qq <- ggplot(d, aes(X, Y)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(colour = isEV)) +
    scale_colour_manual(values = c("No" = "grey70", "Yes" = "black")) +
    xlab(label = "Theoretical Quantiles") +
    scale_x_continuous(breaks = roundedfivenum(d$X)) +
    scale_y_continuous(breaks = roundedfivenum(d$Y)) +
    geom_rangeframe() +
    theme_tufte(base_family = "sans") +
    theme(
      legend.position = "none",
      axis.text = element_text(colour = "black"))

  if (identical(distr, "mvnormal")) {
    p.qq <- p.qq +
      ylab(sprintf("Mahalanobis Distances, p=%d", starts$df)) +
      ggtitle("Q-Q Plot (Chi-squared)")
  } else {
    p.qq <- p.qq +
      ylab(label = varlab) +
      ggtitle(sprintf("Q-Q Plot (%s)\nLL(df = %d) = %0.2f",
                      distribution$Name,
                      attr(distribution$LL, "df"),
                      distribution$LL))
  }

  d$YDeviates <- resid(lm(Y ~ 0 + offset(X), data = d))

  p.qqdeviates <- ggplot(d, aes(X, YDeviates)) +
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
    DensityPlot = p.density,
    QQPlot = p.qq,
    QQDeviatesPlot = p.qqdeviates,
    Data = d,
    Distribution = distribution)))
}


# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("D2", "ChiQuant"))

#' NOTE: this function is replaced and combined into the \code{testdistr} function.
#'
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
#' testdistr(mtcars, "mvnormal")
#'
mvqq <- function(dat, use = c("fiml", "pairwise.complete.obs", "complete.obs"), plot = TRUE) {
  use <- match.arg(use)

  .Deprecated("testdistr")

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
    stat_function(fun = dchisq,
                  args = list(df = ncol(dat)),
                  colour = "blue") +
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
#' @importFrom lme4 lmer
#' @importFrom lsmeans lsmeans
#' @importFrom multcompView multcompLetters
#' @keywords plot
#' @export
#' @examples
#'
#' ## examples using it with single level data
#' ## differences based on an ANOVA and follow up contrasts
#' TukeyHSDgg("cyl", "mpg", mtcars)
#' TukeyHSDgg("Species", "Sepal.Length", iris)
#'
#' ## example based on multilevel data
#' ## differences based on model fit with lmer and follow up contrasts
#' TukeyHSDgg("treatment", "decrease", OrchardSprays, idvar = "colpos")
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
  tHSD <- lsmeans(fit, specs = as.formula(sprintf("pairwise ~ %s", x)))
  tHSDs <- summary(tHSD)

  ## Extract labels and factor levels from Tukey post-hoc
  Tukey.levels.names <- tHSDs[["contrasts"]][, "contrast"]
  Tukey.levels <- tHSDs[["contrasts"]][, "p.value"]
  names(Tukey.levels) <- gsub("\\s", "", Tukey.levels.names)

  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])

  plotdf <- merge(
    tHSDs$lsmeans,
    data.frame(Labels = plot.labels, Letters = Tukey.labels),
    by.x = x, by.y = "Labels")

  p <- ggplot(plotdf, aes_string(x=x, y="lsmean", ymin = "lower.CL", ymax = "upper.CL")) +
    geom_pointrange() + geom_point() +
    geom_text(aes(y = upper.CL + (max(upper.CL) * .05),
                  label = Letters))

  return(p)
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("Predicted", "StandardizedResiduals",
                                                        "Res.1", "Res.9"))


#' Plot Residual Diagnostics
#'
#' This is an internal worker function and is not meant to be
#' called directly.
#'
#' @param object A fitted model object from lm.
#' @param ev.perc A real number between 0 and 1 indicating the
#'   proportion of the theoretical distribution beyond which
#'   values are considered extreme values (possible outliers).
#'   Defaults to .001.
#' @return a list including plots of the residuals,
#'   residuals versus fitted values, and one list for
#'   plots of all random effects and finally a data table with
#'   any extreme values identified
#' @importFrom quantreg qss rq rqss
#' @importFrom ggplot2 ggtitle theme geom_quantile stat_smooth
#' @importFrom ggplot2 geom_point geom_bin2d scale_fill_gradient scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 element_text element_line
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom cowplot plot_grid
#' @importFrom stats resid fitted coef predict
#' @keywords plot internal
#' @rdname plotDiagnosticsResiduals
#' @examples
#' # make me!
.plotDiagnosticsResiduals <- function(object, ev.perc = .001) {
  d.frame <- model.frame(object)
  dv <- names(d.frame)[1]

  if (isTRUE(inherits(object, c("ols", "rms", "lm")))) {
    tmp.res <- resid(object, type = "ordinary")
  } else {
    tmp.res <- resid(object, type = "pearson", scaled = TRUE)
  }

  d.res <- data.table(
    StandardizedResiduals = tmp.res,
    Predicted = fitted(object))

  ## residuals versus fitted
  p.resfit <- ggplot(d.res, aes(Predicted, StandardizedResiduals))
  if (nrow(d.res) < 500) {
    p.resfit <- p.resfit +
      geom_point(alpha = pmin(1 / sqrt(log10(nrow(d.res))), 1))
  } else {
    p.resfit <- p.resfit +
      geom_bin2d(aes(fill = ..count..), bins = 80) +
      scale_fill_gradient(low = "grey70", high = "black")
  }

  p.resfit <- p.resfit +
    stat_smooth(method = "loess", se = FALSE, size = 1, colour = "blue") +
    geom_rangeframe() +
    scale_x_continuous(breaks = roundedfivenum(d.res$Predicted)) +
    scale_y_continuous(breaks = roundedfivenum(d.res$StandardizedResiduals)) +
    theme_tufte(base_family = "sans") +
    theme(
      legend.position = "bottom",
      axis.text = element_text(colour = "black"),
      axis.ticks.x = element_line(colour = "white", size = 2)) +
    ggtitle(dv)

  d.hat <- data.table(
    Predicted = seq(
      min(d.res$Predicted),
      max(d.res$Predicted),
      length.out = 1000))
  tau.1 <- tryCatch(
    rqss(StandardizedResiduals ~ qss(Predicted, lambda = 1),
         tau = .1, data = d.res),
    error = function(e) TRUE)
  if (!isTRUE(tau.1)) {
    tau.9 <- tryCatch(
      rqss(StandardizedResiduals ~ qss(Predicted, lambda = 1),
           tau = .9, data = d.res),
      error = function(e) TRUE)
  } else {
    tau.9 <- TRUE
  }
  if (!isTRUE(tau.1) && !isTRUE(tau.9)) {
    d.hat[, Res.1 := predict(tau.1, d.hat)]
    d.hat[, Res.9 := predict(tau.9, d.hat)]
  }
  if (isTRUE(tau.1) || isTRUE(tau.9) || isTRUE(all.equal(d.hat$Res.1, d.hat$Res.9))) {
    tau.21 <- tryCatch(
      rq(StandardizedResiduals ~ Predicted, tau = .1, data = d.res),
      error = function(e) TRUE)
    if (!isTRUE(tau.21)) {
      tau.29 <- tryCatch(
        rq(StandardizedResiduals ~ Predicted, tau = .9, data = d.res),
        error = function(e) TRUE)
    } else {
      tau.29 <- TRUE
    }
    if (!isTRUE(tau.21) && !isTRUE(tau.29)) {
      d.hat[, Res.1 := predict(tau.21, d.hat)]
      d.hat[, Res.9 := predict(tau.29, d.hat)]
    }
  }
  if (isTRUE(all.equal(d.hat$Res.1, d.hat$Res.9))) {
    d.hat[, Res.1 := NA_real_]
    d.hat[, Res.9 := NA_real_]
  }

  p.resfit <- p.resfit +
    geom_line(mapping = aes(x = Predicted, y = Res.1),
              data = d.hat, colour = "blue", size = 1, linetype = 2) +
    geom_line(mapping = aes(x = Predicted, y = Res.9),
              data = d.hat, colour = "blue", size = 1, linetype = 2)

  ## distributions of residuals
  p.tmpres <- testdistr(d.res[, StandardizedResiduals],
                     varlab = "Standardized Residuals",
                     plot = FALSE, extremevalues = "theoretical",
                     ev.perc = ev.perc)
  p.res <- plot_grid(
    p.tmpres$DensityPlot + ggtitle(dv),
    p.tmpres$QQDeviatesPlot, ncol = 1,
    rel_heights = c(3, 1), align = "v")

  return(list(
    p.res = p.res,
    p.tmpres = p.tmpres,
    p.resfit = p.resfit,
    d.res = d.res,
    d.frame = d.frame,
    dv = dv))
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("EffectType", "OriginalOrder"))


#' Plot Diagnostics for an lmer model
#'
#' This function creates a number of diagnostic plots
#' from lmer models. It relies heavily on the \code{testdistr}
#' function.
#'
#' @param object A fitted model object, either of class merMod from
#'   the lme4 package or merModLmerTest from the lmerTest package.
#' @param plot A logical value whether or not to plot the results or
#'   simply return the graaphical  objects.
#' @param ev.perc A real number between 0 and 1 indicating the
#'   proportion of the theoretical distribution beyond which
#'   values are considered extreme values (possible outliers).
#'   Defaults to .001.
#' @param ask A logical whether to ask before changing plots.
#'   Only applies to interactive environments.
#' @param ncol The number of columns to use for interactive plots
#'   Must be either 1 or 2.  Defaults to 1.
#' @return a list including plots of the residuals,
#'   residuals versus fitted values, and one list for
#'   plots of all random effects and finally a data table with
#'   any extreme values identified
#'
#' @importFrom grDevices dev.interactive devAskNewPage
#' @importFrom quantreg qss
#' @importFrom ggplot2 ggtitle theme geom_quantile stat_smooth
#' @importFrom ggplot2 geom_point geom_bin2d scale_fill_gradient scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 element_text element_line
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom nlme VarCorr ranef
#' @importFrom cowplot plot_grid
#' @keywords plot
#' @export
#' @examples
#' # make me!
plotDiagnosticsLMER <- function(object, plot = TRUE, ev.perc = .001, ask = TRUE, ncol = 1) {
  stopifnot(ncol %in% 1:2)

  x <- .plotDiagnosticsResiduals(object, ev.perc = ev.perc)
  idvars <- names(VarCorr(object))

  ## data for outliers
  d.extreme <- x$d.frame[1, c(x$dv, idvars)]
  d.extreme[] <- lapply(d.extreme, as.na)
  d.extreme <- as.data.table(d.extreme)
  if ("EffectType" %in% names(d.extreme)) {
    stop("EffectType is used internally and cannot be a variable or ID in the model")
  }
  d.extreme[, EffectType := NA_character_]

  if (any(x$p.tmpres$Data[, isEV] == "Yes")) {
    d.extreme <- rbind(d.extreme,
                       cbind(as.data.table(
                         x$d.frame[x$p.tmpres$Data[isEV == "Yes", OriginalOrder],
                                 c(x$dv, idvars)]),
                         EffectType = "Residuals"))
  }

  p.ranef <- list()
  for (n in idvars) {
    tmp <- subset(coef(object)[[n]],
                  select = names(ranef(object)[[n]]))
    for (n2 in names(tmp)) {
      p.tmpranef <- testdistr(tmp[[n2]],
                              varlab = "Random Effects",
                              plot = FALSE, extremevalues = "theoretical",
                              ev.perc = ev.perc)
      p.ranef <- c(p.ranef, list(plot_grid(
                              p.tmpranef$DensityPlot + ggtitle(paste(n, ":", n2)),
                              p.tmpranef$QQDeviatesPlot, ncol = 1,
                              rel_heights = c(3, 1), align = "v")))
      if (any(p.tmpranef$Data[, isEV] == "Yes")) {
        d.extreme <- rbind(d.extreme,
                           cbind(as.data.table(
                             x$d.frame[x$d.frame[[n]] %in% rownames(tmp)[p.tmpranef$Data[isEV == "Yes", OriginalOrder]],
                                     c(x$dv, idvars)]),
                             EffectType = paste("Random Effect", n, ":", n2)))
      }
    }
    if (ncol(tmp) > 1) {
      p.tmpranef <- testdistr(tmp, distr = "mvnormal",
                              varlab = "Random Effects",
                              plot = FALSE, extremevalues = "theoretical",
                              ev.perc = ev.perc)
      p.ranef <- c(p.ranef, list(plot_grid(
                              p.tmpranef$DensityPlot + ggtitle(paste(n, ":", "MV Normal")),
                              p.tmpranef$QQDeviatesPlot, ncol = 1,
                              rel_heights = c(3, 1), align = "v")))
      if (any(p.tmpranef$Data[, isEV] == "Yes")) {
        d.extreme <- rbind(d.extreme,
                           cbind(as.data.table(
                             x$d.frame[x$d.frame[[n]] %in% rownames(tmp)[p.tmpranef$Data[isEV == "Yes", OriginalOrder]],
                                     c(x$dv, idvars)]),
                             EffectType = paste("Multivariate Random Effect", n)))
      }
    }
  }

  if (plot) {
    if (ask && dev.interactive()) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    if (ncol == 1) {
      print(x$p.res)
      print(x$p.resfit)
      for (i in seq_along(p.ranef)) {
        print(p.ranef[[i]])
      }
    } else if (ncol == 2) {
      print(plot_grid(
        x$p.res,
        x$p.resfit,
        ncol = 2))
      for (i in 1:ceiling(length(p.ranef) / 2)) {
        print(do.call(plot_grid, c(p.ranef[(i * 2 - 1):(i * 2)], ncol = 2)))
      }
    }
  }

  return(invisible(list(
    ResPlot = x$p.res,
    ResFittedPlot = x$p.resfit,
    RanefPlot = p.ranef,
    ExtremeValues = na.omit(d.extreme))))
}



#' Plot Diagnostics for an lm model
#'
#' This function creates a number of diagnostic plots
#' from lm models. It relies heavily on the \code{testdistr}
#' function.
#'
#' @param object A fitted model object from lm.
#' @param plot A logical value whether or not to plot the results or
#'   simply return the graaphical  objects.
#' @param ev.perc A real number between 0 and 1 indicating the
#'   proportion of the theoretical distribution beyond which
#'   values are considered extreme values (possible outliers).
#'   Defaults to .001.
#' @param ask A logical whether to ask before changing plots.
#'   Only applies to interactive environments.
#' @param ncol The number of columns to use for interactive plots
#'   Must be either 1 or 2.  Defaults to 1.
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
#' @export
#' @examples
#' # make me!
plotDiagnosticsLM <- function(object, plot = TRUE, ev.perc = .001, ask = TRUE, ncol = 1) {
  stopifnot(ncol %in% 1:2)

  x <- .plotDiagnosticsResiduals(object, ev.perc = ev.perc)

  ## data for outliers
  d.extreme <- data.table(dv = NA_real_,
                          EffectType = NA_character_)
  setnames(d.extreme, names(d.extreme), c(x$dv, "EffectType"))
  if ("EffectType" %in% x$dv) {
    stop("EffectType is used internally and cannot be a variable in the model")
  }

  if (any(x$p.tmpres$Data[, isEV] == "Yes")) {
    d.extreme <- rbind(d.extreme,
                       cbind(as.data.table(
                         x$d.frame[x$p.tmpres$Data[isEV == "Yes", OriginalOrder],
                                 c(x$dv), drop = FALSE]),
                         EffectType = "Residuals"))
  }

  if (plot) {
    if (ask && dev.interactive()) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    if (ncol == 1) {
      print(x$p.res)
      print(x$p.resfit)
    } else if (ncol == 2) {
      print(plot_grid(
        x$p.res,
        x$p.resfit,
        ncol = 2))
    }
  }

  return(invisible(list(
    ResPlot = x$p.res,
    ResFittedPlot = x$p.resfit,
    ExtremeValues = na.omit(d.extreme))))
}

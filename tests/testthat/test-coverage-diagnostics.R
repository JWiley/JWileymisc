test_that("testDistribution covers matrix missingness and chi-square EV limits", {
  # Matrix input uses row-wise missing checks before fitting the distribution.
  normal_matrix <- testDistribution(as.matrix(c(1, NA, 2, 3)), "normal")
  expect_s3_class(normal_matrix, "testDistribution")
  expect_equal(normal_matrix$NOK, 3L)

  empirical <- testDistribution(
    seq(1, 10), "chisq",
    starts = list(df = 3),
    extremevalues = "empirical")
  expect_equal(empirical$EVLimits[1], -Inf, ignore_attr = TRUE)

  theoretical <- testDistribution(
    seq(1, 10), "chisq",
    starts = list(df = 3),
    extremevalues = "theoretical")
  expect_equal(theoretical$EVLimits[1], 0)
})

test_that("testDistribution covers FIML multivariate normal and mvnormal plots", {
  x <- as.data.frame(scale(mtcars[, 1:3]))
  x[1, 1] <- NA

  fiml <- testDistribution(x, distr = "mvnormal", use = "fiml")
  expect_s3_class(fiml, "testDistribution")
  expect_equal(fiml$Distribution$Name, "Chi-squared")

  # mvnormal plots use Mahalanobis-specific axis labels.
  plotted <- plot(testDistribution(mtcars[, 1:3], distr = "mvnormal"), plot = FALSE)
  expect_s3_class(plotted$DensityPlot, "ggplot")
  expect_match(plotted$DensityPlot$labels$x, "Mahalanobis")
  expect_match(plotted$QQPlot$labels$y, "Mahalanobis")
})

test_that("residualDiagnostics handles omitted rows and no quantile bands", {
  d <- data.frame(y = c(1:5, NA, 7:12), x = 1:12)
  m <- stats::lm(y ~ x, data = d, na.action = stats::na.omit)

  rd <- residualDiagnostics(m, quantiles = FALSE)
  expect_s3_class(rd, "residualDiagnostics.lm")
  expect_equal(max(rd$Residuals$Index), 12L)
  expect_true(all(is.na(rd$Hat$Mid)))
})

test_that("diagnostic plots cover printed grid paths and binned residual plots", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit(grDevices::dev.off())

  d <- data.frame(y = c(1:5, NA, 7:12), x = 1:12)
  rd <- residualDiagnostics(
    stats::lm(y ~ x, data = d, na.action = stats::na.omit),
    quantiles = FALSE)
  expect_invisible(plot(rd, plot = TRUE, ask = FALSE, ncol = 1))
  expect_invisible(plot(rd, plot = TRUE, ask = FALSE))

  # N >= 500 with continuous fitted values uses geom_bin2d.
  set.seed(1)
  n <- 505
  big <- residualDiagnostics(
    stats::lm(y ~ x, data = data.frame(y = (1:n) + stats::rnorm(n), x = 1:n)),
    quantiles = FALSE)
  expect_invisible(big_plot <- plot(big, plot = FALSE))
  expect_s3_class(big_plot$ResFittedPlot, "ggplot")
})

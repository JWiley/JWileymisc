test_that("plot.SEMSummary.list prints a single available level", {
  # A grouped SEMSummary with one observed level reaches the single-plot branch.
  s <- SEMSummary(~ mpg + hp | am, data = subset(mtcars, am == 0))
  expect_invisible(p <- plot(s, plot = TRUE))
  expect_length(p, 1L)
})

test_that("corplot handles numeric dimnames and dropped invalid columns", {
  no_names <- matrix(c(1, .2, .2, 1), 2, 2)
  # Current corplot() requires dimnames; this still covers NULL row labels.
  expect_error(corplot(no_names, order = "asis"))

  numeric_names <- matrix(
    c(1, NA, .3, NA, 1, .4, .3, .4, 1),
    3, 3,
    dimnames = list(c("1", "2", "3"), c("1", "2", "3")))
  # The NA column is appended after the clusterable columns.
  expect_s3_class(corplot(numeric_names), "ggplot")
})

test_that("TukeyHSDgg coerces numeric character outcomes", {
  d <- data.frame(
    g = factor(rep(letters[1:3], each = 3)),
    y = as.character(1:9))

  expect_warning(
    p <- TukeyHSDgg("g", "y", d),
    "y was not numeric")
  expect_s3_class(p, "ggplot")
})

test_that("diagnostic plot methods handle interactive ask branches", {
  testthat::local_mocked_bindings(
    dev.interactive = function() TRUE,
    devAskNewPage = function(...) FALSE,
    .package = "JWileymisc")

  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit(grDevices::dev.off())

  rd <- residualDiagnostics(stats::lm(mpg ~ hp, data = mtcars))
  expect_invisible(plot(rd, plot = TRUE, ask = TRUE, ncol = 1))

  md <- modelDiagnostics(stats::lm(mpg ~ hp, data = mtcars))
  expect_invisible(plot(md, plot = TRUE, ask = TRUE, ncol = 1))
})

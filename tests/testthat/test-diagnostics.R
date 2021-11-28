test_that("is.residualDiagnostics and as.residualDiagnostics work", {
  expect_false(is.residualDiagnostics(mtcars))
  expect_error(as.residualDiagnostics(1))
  expect_error(as.residualDiagnostics(list(1, 2, 3, 4, 5)))
})

test_that("residualDiagnostics works with linear models", {
  m <- stats::lm(mpg ~ hp, data = mtcars)
  rd <- suppressWarnings(residualDiagnostics(m))
  expect_s3_class(rd, "residualDiagnostics.lm")

  rd <- suppressWarnings(residualDiagnostics(m, standardized = FALSE))
  expect_s3_class(rd, "residualDiagnostics.lm")
  expect_true(is.residualDiagnostics(rd))
})

test_that("internal function, .quantilePercentiles works", {
  x <- JWileymisc:::.quantilePercentiles(
    data.frame(
      Predicted = mtcars$mpg,
      Residuals = mtcars$hp))

  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 1000)
  expect_false(anyNA(x))

  x <- JWileymisc:::.quantilePercentiles(
    data.frame(
      Predicted = 1:10,
      Residuals = 1))

  expect_s3_class(x, "data.table")
  expect_equal(nrow(x), 1000)
  expect_true(anyNA(x))
})


test_that("is.modelDiagnostics and as.modelDiagnostics work", {
  expect_false(is.modelDiagnostics(mtcars))
  expect_error(as.modelDiagnostics(1))
  expect_error(as.modelDiagnostics(1:3))
  expect_error(as.modelDiagnostics(list(1, 2, 3)))
})

test_that("modelDiagnostics works with linear models", {
  m <- stats::lm(mpg ~ hp * factor(cyl), data = mtcars)
  md <- suppressWarnings(modelDiagnostics(m))
  expect_s3_class(md, "modelDiagnostics.lm")

  expect_invisible(td <- plot(md, plot = FALSE, ask = FALSE))
  expect_length(td, 2)
  expect_s3_class(td$ResPlot, "ggplot")
  expect_s3_class(td$ResFittedPlot, "ggplot")

  expect_invisible(td <- plot(md, plot = TRUE, ask = FALSE))
  expect_invisible(td <- plot(md, plot = TRUE, ask = FALSE, ncol = 1))

  md2 <- suppressWarnings(modelDiagnostics(m, ev.perc = .2))
  expect_s3_class(md2, "modelDiagnostics.lm")

  m <- stats::lm(EffectType ~ 1, data = data.frame(EffectType = 1:10))
  expect_error(modelDiagnostics(m))

  md <- modelDiagnostics(stats::lm(Y ~ 1, data = data.frame(Y = 1:10)))
  expect_invisible(td <- plot(md, plot = FALSE, ask = FALSE, ncol = 1))

  md <- modelDiagnostics(stats::lm(Y ~ 1, data = data.frame(Y = 1:505)))
  expect_warning(plot(md, plot = TRUE, ask = FALSE, ncol = 1))
})

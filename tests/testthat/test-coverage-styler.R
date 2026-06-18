fakepool_summary <- function() {
  data.frame(
    est = c(1, 2),
    se = c(.1, .2),
    `lo 95` = c(.8, 1.6),
    `hi 95` = c(1.2, 2.4),
    `Pr(>|t|)` = c(.01, .2),
    row.names = c("(Intercept)", "hp"),
    check.names = FALSE)
}

test_that("APAStyler.list merges named modelTest outputs", {
  # Named modelTest lists rename estimate columns to the supplied model names.
  mt1 <- modelTest(stats::lm(mpg ~ hp, data = mtcars))
  mt2 <- modelTest(stats::lm(mpg ~ qsec, data = mtcars))

  out <- APAStyler(list(Horsepower = mt1, QuarterMile = mt2), print = FALSE)
  expect_s3_class(out, "data.table")
  expect_true(c("Horsepower", "QuarterMile") %ain% names(out))
})

test_that("APAStyler.lm prints styled matrices when requested", {
  expect_output(
    APAStyler(stats::lm(mpg ~ hp, data = mtcars), print = TRUE),
    "R\\^2")
})

test_that("APAStyler.mira formats pooled linear model results", {
  testthat::local_mocked_bindings(
    pool = function(object) structure(list(), class = "fakepool"),
    summary = function(object, ...) {
      if (inherits(object, "fakepool")) fakepool_summary() else base::summary(object, ...)
    },
    pool.r.squared = function(object) {
      matrix(.5, nrow = 1, dimnames = list(NULL, "est"))
    },
    .package = "JWileymisc")

  mira <- structure(
    list(analyses = list(stats::lm(mpg ~ hp, data = mtcars))),
    class = "mira")

  expect_warning(
    out <- APAStyler(
      mira,
      lmobject = stats::lm(mpg ~ hp, data = mtcars),
      print = FALSE),
    "listwise deleted")
  expect_true(c("R^2", "F") %ain% rownames(out))

  f <- tempfile()
  expect_invisible(APAStyler(mira, file = f, print = FALSE))
  expect_true(file.exists(f))

  expect_output(APAStyler(mira, print = TRUE), "Constant")
})

test_that("APAStyler.mira rejects non-lm analyses and warns for ignored lmobject", {
  testthat::local_mocked_bindings(
    pool = function(object) structure(list(), class = "fakepool"),
    summary = function(object, ...) {
      if (inherits(object, "fakepool")) fakepool_summary() else base::summary(object, ...)
    },
    .package = "JWileymisc")

  bad <- structure(
    list(analyses = list(structure(list(), class = "notlm"))),
    class = "mira")
  expect_error(APAStyler(bad, print = FALSE), "mira object")

  glm_mira <- structure(
    list(analyses = list(stats::glm(am ~ hp, data = mtcars, family = binomial()))),
    class = "mira")
  expect_warning(
    out <- APAStyler(
      glm_mira,
      lmobject = stats::lm(mpg ~ hp, data = mtcars),
      print = FALSE),
    "ignored")
  expect_false("R^2" %in% rownames(out))
})

test_that("APAStyler.SEMSummary writes and prints tables", {
  f <- tempfile()
  s <- SEMSummary(~ mpg + hp, data = mtcars)

  cov_stars <- APAStyler(s, type = "cov", stars = TRUE, file = FALSE, print = FALSE)
  expect_match(cov_stars$table[1, 5], "\\*")

  expect_output(
    out <- APAStyler(s, type = "both", stars = TRUE, file = f, print = TRUE),
    "Percentage of coverage")
  expect_true(file.exists(f))
  expect_equal(names(out), c("table", "coverage"))
})

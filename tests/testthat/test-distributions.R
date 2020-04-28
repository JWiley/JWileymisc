context("testDistribution")

test_that("is.testDistribution and as.testDistribution work", {
  expect_false(is.testDistribution(mtcars))
  expect_error(as.testDistribution(1))
  expect_error(as.testDistribution(list(1, 2, 3)))
})

test_that("testDistribution works with a normal distribution", {
  expect_message(m <- testDistribution(1:10, "normal",
   use = "pairwise.complete.obs", robust = TRUE))
  expect_is(m,
            "testDistribution")

  expect_invisible(td <- plot(m, plot = FALSE))
  expect_length(td, 4)
  expect_is(td$testDistribution, "testDistribution")
  expect_invisible(plot(m, plot = TRUE))

  m <- testDistribution(1:10, "normal")
  expect_that(m$Data, is_a("data.table"))

  ## original order works
  expect_equal(
    testDistribution(c(1, 99, 2, 10:1),
                     extremevalues = "theoretical")$Data[isEV == "Yes", OriginalOrder],
    2)

  ## original order works with missing
  expect_equal(
    testDistribution(c(1, 2, NA, 99, 10:1),
                     extremevalues = "theoretical")$Data[isEV == "Yes", OriginalOrder],
    4)

  expect_is(
    testDistribution(c(1, 2, NA, 99, 10:1),
                     extremevalues = "empirical"),
    "testDistribution")

  expect_error(testDistribution(c(1, 2, 3, NA), na.rm=FALSE))

})

test_that("testDistribution works with a multivariate normal distribution", {
  expect_is(
    testDistribution(mtcars[, 1:3], distr = "mvnormal"),
    "testDistribution")

  expect_is(
    testDistribution(mtcars[, 1:3], distr = "mvnormal",
                     robust = TRUE),
    "testDistribution")


  expect_is(
    testDistribution(matrix(c(1:8, NA), 3), distr = "mvnormal"),
    "testDistribution")

  expect_is(
    testDistribution(
      matrix(c(1:8, NA), 3), distr = "mvnormal",
      mu = c(1, 2, 3), sigma = diag(3)),
    "testDistribution")

  x <- as.data.frame(scale(mtcars[, 1:3]))
  x[1, 1] <- NA

  expect_is(
    testDistribution(x, distr = "mvnormal", use = "pairwise.complete.obs"),
    "testDistribution")

  expect_is(
    testDistribution(
      matrix(c(1:8, NA), 3), distr = "mvnormal",
      mu = c(1, 2, 3), sigma = diag(0, 3, 3)),
    "testDistribution")
})

test_that("testDistribution works with a multivariate normal distribution and missing data", {
  skip_on_cran()
  x <- as.data.frame(scale(mtcars[, 1:3]))
  x[1, 1] <- NA
  m <- testDistribution(x, distr = "mvnormal", use = "fiml")
  expect_is(m, "testDistribution")

  expect_invisible(td <- plot(m, plot = FALSE))
  expect_length(td, 4)
  expect_is(td$testDistribution, "testDistribution")
  expect_invisible(plot(m, plot = TRUE))
})

test_that("testDistribution works with a beta distribution", {
  x <- seq(.1, .9, length.out = 10)
  m <- testDistribution(x, "beta", starts = list(shape1 = 1, shape2 = 1))
  expect_that(m$Data, is_a("data.table"))

  expect_error(testDistribution(x, "beta"))
})

test_that("testDistribution works with a chi-squared distribution", {
  x <- seq(1, 10, length.out = 10)
  m <- testDistribution(x, "chisq", starts = list(df = 3))
  expect_that(m$Data, is_a("data.table"))

  expect_error(testDistribution(x, "chisq"))
})

test_that("testDistribution works with a F distribution", {
  set.seed(1234)
  x <- rf(200, 3, 10)
  m <- testDistribution(x, "f", starts = list(df1 = 3, df2 = 10))
  expect_that(m$Data, is_a("data.table"))

  expect_error(testDistribution(x, "f"))
})

test_that("testDistribution works with a gamma distribution", {
  x <- seq(1, 10, length.out = 10)
  m <- testDistribution(x, "gamma")
  expect_that(m$Data, is_a("data.table"))
})

test_that("testDistribution works with a poisson distribution", {
  x <- seq(1, 10, length.out = 10)
  m <- testDistribution(x, "poisson")
  expect_that(m$Data, is_a("data.table"))

  expect_invisible(td <- plot(m, plot = FALSE))
  expect_length(td, 4)
  expect_is(td$testDistribution, "testDistribution")
  expect_invisible(plot(m, plot = TRUE))

  expect_warning(testDistribution(c(-.2, .2, .5, 55.23), "poisson"))
})

test_that("testDistribution works with a negative binomial distribution", {
  x <- seq(1, 10, length.out = 10)
  m <- testDistribution(x, "nbinom")
  expect_that(m$Data, is_a("data.table"))

})


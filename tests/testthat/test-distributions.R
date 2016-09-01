context("testdistr")

test_that("testdistr works with a normal distribution", {
  x <- 1:10
  m <- testdistr(x, "normal", plot = FALSE)

  expect_that(m$DensityPlot, is_a("ggplot"))
  expect_that(m$QQPlot, is_a("ggplot"))
  expect_that(m$Data, is_a("data.table"))
  expect_that(m$Distribution, is_a("list"))


  expect_identical(nrow(m$Data), length(x))
  expect_identical(length(m$Distribution), 5L)
})


test_that("testdistr works with a beta distribution", {
  x <- seq(.1, .9, length.out = 10)

  expect_error(testdistr(x, "beta", plot = FALSE))

  m <- testdistr(x, "beta", starts = list(shape1 = 1, shape2 = 1), plot = FALSE)

  expect_that(m$DensityPlot, is_a("ggplot"))
  expect_that(m$QQPlot, is_a("ggplot"))
  expect_that(m$Data, is_a("data.table"))
  expect_that(m$Distribution, is_a("list"))


  expect_identical(nrow(m$Data), length(x))
  expect_identical(length(m$Distribution), 5L)
})

test_that("testdistr works with a chi-squared distribution", {
  x <- seq(1, 10, length.out = 10)

  expect_error(testdistr(x, "chisq", plot = FALSE))

  m <- testdistr(x, "chisq", starts = list(df = 3), plot = FALSE)

  expect_that(m$DensityPlot, is_a("ggplot"))
  expect_that(m$QQPlot, is_a("ggplot"))
  expect_that(m$Data, is_a("data.table"))
  expect_that(m$Distribution, is_a("list"))


  expect_identical(nrow(m$Data), length(x))
  expect_identical(length(m$Distribution), 5L)
})

test_that("testdistr works with a F distribution", {
  set.seed(1234)
  x <- rf(200, 3, 10)

  expect_error(testdistr(x, "f", plot = FALSE))

  m <- testdistr(x, "f", starts = list(df1 = 3, df2 = 10), plot = FALSE)

  expect_that(m$DensityPlot, is_a("ggplot"))
  expect_that(m$QQPlot, is_a("ggplot"))
  expect_that(m$Data, is_a("data.table"))
  expect_that(m$Distribution, is_a("list"))


  expect_identical(nrow(m$Data), length(x))
  expect_identical(length(m$Distribution), 5L)
})

test_that("testdistr works with a gamma distribution", {
  x <- seq(1, 10, length.out = 10)

  m <- testdistr(x, "gamma", plot = FALSE)

  expect_that(m$DensityPlot, is_a("ggplot"))
  expect_that(m$QQPlot, is_a("ggplot"))
  expect_that(m$Data, is_a("data.table"))
  expect_that(m$Distribution, is_a("list"))


  expect_identical(nrow(m$Data), length(x))
  expect_identical(length(m$Distribution), 5L)
})

test_that("testdistr works with a poisson distribution", {
  expect_warning(testdistr(c(-.2, .2, .5, 55.23), "poisson", plot = FALSE))

  x <- seq(1, 10, length.out = 10)
  m <- testdistr(x, "poisson", plot = FALSE)

  expect_that(m$DensityPlot, is_a("ggplot"))
  expect_that(m$QQPlot, is_a("ggplot"))
  expect_that(m$Data, is_a("data.table"))
  expect_that(m$Distribution, is_a("list"))


  expect_identical(nrow(m$Data), length(x))
  expect_identical(length(m$Distribution), 5L)
})

test_that("testdistr works with a negative binomial distribution", {
  x <- seq(1, 10, length.out = 10)

  m <- testdistr(x, "nbinom", plot = FALSE)

  expect_that(m$DensityPlot, is_a("ggplot"))
  expect_that(m$QQPlot, is_a("ggplot"))
  expect_that(m$Data, is_a("data.table"))
  expect_that(m$Distribution, is_a("list"))


  expect_identical(nrow(m$Data), length(x))
  expect_identical(length(m$Distribution), 5L)
})

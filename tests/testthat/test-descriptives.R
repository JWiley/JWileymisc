context("Descriptives")

test_that("roundedfivenum works", {
  expect_equivalent(
    JWileymisc:::roundedfivenum(1:5),
    1:5)
  expect_equivalent(
    JWileymisc:::roundedfivenum(c(.1, .3, .5, .7, .9), round = 0),
    c(0, 0, 0, 1, 1))
  expect_equivalent(
    JWileymisc:::roundedfivenum(mtcars$mpg, round = 2, sig = 1),
    c(10, 20, 20, 20, 30))
})

test_that("f.r2 works", {
  expect_equivalent(
    length(JWileymisc:::f.r2(.5, 4, 100)),
    4)
})

test_that("meanCircular works", {
  expect_equivalent(
    meanCircular(c(23, 1), max = 24),
    0)
  expect_equivalent(
    meanCircular(c(23, 1, NA), max = 24, na.rm = TRUE),
    0)
  expect_equivalent(
    meanCircular(c(NA_real_), max = 24, na.rm = TRUE),
    NA_real_)
  expect_equivalent(
    meanCircular(c(23, 1, NA), max = 24, na.rm = FALSE),
    NA_real_)
  expect_equivalent(
    meanCircular(24, max = 24),
    0)
  expect_error(meanCircular("a"))
  expect_error(meanCircular(99, max = 24))
  expect_error(meanCircular(-1, max = 24))
})

test_that("cramerV works", {
  expect_error(cramerV(xtabs(~ am + vs + cyl, data = mtcars)))
  expect_equivalent(
    length(cramerV(xtabs(~ am + vs, data = mtcars))),
    1L)
  expect_equivalent(
    length(cramerV(xtabs(~ am + cyl, data = mtcars))),
    1L)
})

test_that("smd works", {
  expect_equivalent(
    smd(c(1:3, 4:6), rep(1:2, each = 3)),
    3)
  expect_equivalent(
    smd(c(1:3, 4:6), factor(rep(1:2, each = 3))),
    3)
  expect_equivalent(
    smd(c(1:3, 3, 5, 7), rep(1:2, each = 3), index = "1"),
    3)
  expect_equivalent(
    smd(c(0, 2, 4, 4:6), rep(1:2, each = 3), index = "2"),
    3)
  expect_error(smd(mtcars$mpg, mtcars$cyl))
})

test_that("egltable works", {
  t1 <- egltable("mpg", data = mtcars)
  expect_is(t1, "data.table")
  expect_equal(nrow(t1), 1L)
  expect_equal(dim(t1), c(1L, 2L))

  t2 <- egltable("mpg", "vs", data = mtcars)
  expect_is(t2, "data.table")
  expect_equal(nrow(t2), 1L)
  expect_equal(dim(t2), c(1L, 4L))

  t3 <- egltable("mpg", "cyl", data = mtcars)
  expect_is(t3, "data.table")
  expect_equal(nrow(t3), 1L)
  expect_equal(dim(t3), c(1L, 5L))

  t4 <- egltable(c("mpg", "am"), data = mtcars)
  expect_is(t4, "data.table")
  expect_equal(nrow(t4), 2L)
  expect_equal(dim(t4), c(2L, 2L))

  t5 <- egltable(c("mpg", "am"), data = mtcars, strict = FALSE)
  expect_is(t5, "data.table")
  expect_equal(nrow(t5), 4L)
  expect_equal(dim(t5), c(4L, 2L))

  t6 <- egltable(mtcars$mpg)
  expect_is(t6, "data.table")
  expect_equal(nrow(t6), 1L)
  expect_equal(dim(t6), c(1L, 2L))

  t7 <- egltable(mtcars[, c("mpg", "hp")])
  expect_is(t7, "data.table")
  expect_equal(nrow(t7), 2L)
  expect_equal(dim(t7), c(2L, 2L))

  t8 <- egltable(mtcars[, c("mpg", "hp")], parametric = FALSE)
  expect_is(t8, "data.table")
  expect_equal(nrow(t8), 2L)
  expect_equal(dim(t8), c(2L, 2L))

  t9 <- egltable(mtcars[, c("mpg", "cyl")], strict = FALSE, parametric = FALSE)
  expect_is(t9, "data.table")
  expect_equal(nrow(t9), 5L)
  expect_equal(dim(t9), c(5L, 2L))

  t10 <- egltable(c("mpg", "cyl"), data = data.table::as.data.table(mtcars),
                  strict = FALSE, parametric = FALSE)
  expect_is(t10, "data.table")
  expect_equal(nrow(t10), 5L)
  expect_equal(dim(t10), c(5L, 2L))

  t11 <- egltable(c("cyl"), data = mtcars,
                  strict = FALSE, parametric = FALSE)
  expect_is(t11, "data.table")
  expect_equal(nrow(t11), 4L)
  expect_equal(dim(t11), c(4L, 2L))

  t12 <- egltable(c("mpg", "hp"), "cyl", data = mtcars, parametric = FALSE)
  expect_is(t12, "data.table")
  expect_equal(nrow(t12), 2L)
  expect_equal(dim(t12), c(2L, 5L))

  expect_warning(t13 <- egltable(c("am"), "cyl", data = mtcars, strict = FALSE))
  expect_is(t13, "data.table")
  expect_equal(nrow(t13), 3L)
  expect_equal(dim(t13), c(3L, 5L))


})

context("winsorizor")

test_that("winsorizor uses percentiles correctly for vectors and data frames / matrices", {
              d1 <- 0:100
              e1 <- c(1, 99)
              expect_equivalent(range(winsorizor(d1, .01)), e1)

              d2 <- data.frame(a = 0:100, b = 0:100)
              e2 <- cbind(a = c(1, 99), b = c(1, 99))
              expect_equivalent(apply(winsorizor(d2, .01), 2, range), e2)

              d3 <- cbind(a = 0:100, b = 0:100)
              e3 <- cbind(a = c(1, 99), b = c(1, 99))
              expect_equivalent(apply(winsorizor(d3, .01), 2, range), e3)

              expect_is(winsorizor(data.table::as.data.table(mtcars[, 1:3]), .1), "data.table")
              expect_is(
                winsorizor(data.table::as.data.table(mtcars[, 1:3]),
                           values = data.frame(low = 1, high = 50)),
                "data.table")
})

test_that("winsorizor removes missing values", {
              d1 <- c(NA, 0:100)
              e1 <- c(1, 99)
              expect_equivalent(range(winsorizor(d1, .01), na.rm = TRUE), e1)

              d2 <- data.frame(a = c(NA, 0:100), b = c(NA, 0:100))
              e2 <- cbind(a = c(1, 99), b = c(1, 99))
              expect_equivalent(apply(winsorizor(d2, .01), 2, range, na.rm = TRUE), e2)

              d3 <- cbind(a = c(NA, 0:100), b = c(NA, 0:100))
              e3 <- cbind(a = c(1, 99), b = c(1, 99))
              expect_equivalent(apply(winsorizor(d3, .01), 2, range, na.rm = TRUE), e3)
})

test_that("winsorizor can use specified values", {
              d1 <- 0:100
              v1 <- data.frame(low = 2.5, high = 97.5)
              e1 <- c(2.5, 97.5)
              expect_equivalent(range(winsorizor(d1, values = v1)), e1)

              d2 <- data.frame(a = 0:100, b = 0:100)
              v2 <- data.frame(low = c(2.5, 3.5), high = c(97.5, 96.5))
              e2 <- cbind(a = c(2.5, 97.5), b = c(3.5, 96.5))
              expect_equivalent(apply(winsorizor(d2, values = v2), 2, range), e2)
})

test_that("winsorizor errors, warnings and missings", {
  expect_error(winsorizor(1, percentile = 2))
  expect_warning(winsorizor(NULL))

  expect_equivalent(winsorizor(1), NA_real_)
})



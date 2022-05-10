test_that(
  "winsorizor uses percentiles correctly for vectors and data frames / matrices", {
    d1 <- 0:100
    e1 <- c(1, 99)
    expect_equal(range(winsorizor(d1, .01)),
                 e1, ignore_attr = TRUE)

    d2 <- data.frame(a = 0:100, b = 0:100)
    e2 <- cbind(a = c(1, 99), b = c(1, 99))
    expect_equal(apply(winsorizor(d2, .01), 2, range),
                 e2, ignore_attr = TRUE)

    d3 <- cbind(a = 0:100, b = 0:100)
    e3 <- cbind(a = c(1, 99), b = c(1, 99))
    expect_equal(apply(winsorizor(d3, .01), 2, range),
                 e3, ignore_attr = TRUE)

    expect_s3_class(
      winsorizor(data.table::as.data.table(mtcars[, 1:3]), .1),
      "data.table")
    expect_s3_class(
      winsorizor(data.table::as.data.table(mtcars[, 1:3]),
                 values = data.frame(low = 1, high = 50)),
      "data.table")
  })

test_that(
  "winsorizor uses a vector of percentiles without warning data frames", {
    d2 <- data.frame(a = 0:100, b = 0:100)
    e2 <- cbind(a = c(1, 99), b = c(10, 90))    
    expect_equal(apply(winsorizor(d2, c(.01, .10)), 2, range),
                 e2, ignore_attr = TRUE)
  })

test_that("winsorizor removes missing values", {
  d1 <- c(NA, 0:100)
  e1 <- c(1, 99)
  expect_equal(range(winsorizor(d1, .01), na.rm = TRUE),
               e1, ignore_attr = TRUE)

  d2 <- data.frame(a = c(NA, 0:100), b = c(NA, 0:100))
  e2 <- cbind(a = c(1, 99), b = c(1, 99))
  expect_equal(apply(winsorizor(d2, .01), 2, range, na.rm = TRUE),
               e2, ignore_attr = TRUE)

  d3 <- cbind(a = c(NA, 0:100), b = c(NA, 0:100))
  e3 <- cbind(a = c(1, 99), b = c(1, 99))
  expect_equal(apply(winsorizor(d3, .01), 2, range, na.rm = TRUE),
               e3, ignore_attr = TRUE)
})

test_that("winsorizor can use specified values", {
  d1 <- 0:100
  v1 <- data.frame(low = 2.5, high = 97.5)
  e1 <- c(2.5, 97.5)
  expect_equal(range(winsorizor(d1, values = v1)),
               e1, ignore_attr = TRUE)

  d2 <- data.frame(a = 0:100, b = 0:100)
  v2 <- data.frame(low = c(2.5, 3.5), high = c(97.5, 96.5))
  e2 <- cbind(a = c(2.5, 97.5), b = c(3.5, 96.5))
  expect_equal(apply(winsorizor(d2, values = v2), 2, range),
               e2, ignore_attr = TRUE)
})

test_that("winsorizor errors, warnings and missings", {
  expect_error(winsorizor(1, percentile = 2))
  expect_warning(winsorizor(NULL))

  expect_equal(winsorizor(1), NA_real_, ignore_attr = TRUE)
})

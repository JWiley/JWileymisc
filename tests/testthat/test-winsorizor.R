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



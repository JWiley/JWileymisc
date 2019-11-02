context("CheckVals")

test_that("CheckVals works", {
  expect_true(CheckVals(mtcars$cyl, c(4, 6, 8)))
  expect_true(CheckVals(mtcars[, c("cyl", "am")], c(0, 1, 4, 6, 8)))
  expect_true(CheckVals(as.matrix(mtcars[, c("cyl", "am")]), c(0, 1, 4, 6, 8)))
  expect_error(CheckVals(mtcars[, c("cyl", "am")], c(0, 4, 6, 8)))
})

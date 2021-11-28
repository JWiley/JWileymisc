test_that("lm2 works", {
  mtcars$cyl <- factor(mtcars$cyl)
  m <- lm(mpg ~ hp * cyl, data = mtcars)

  x <- model.matrix(m)
  y <- mtcars$mpg
  m2 <- JWileymisc:::lm2(mpg ~ 1 + cyl + hp:cyl, data = mtcars,
                         designMatrix = x[, -2, drop = FALSE],
                         yObserved = y)

  expect_true(inherits(m2, "lm"))
  expect_null(m2[["x"]])
  expect_null(m2[["y"]])
  expect_false(is.null(m2[["qr"]]))

  m2 <- JWileymisc:::lm2(mpg ~ 1 + cyl + hp:cyl, data = mtcars,
                         designMatrix = x[, -2, drop = FALSE],
                         yObserved = y,
                         x = TRUE, y = TRUE, qr = FALSE)

  expect_true(inherits(m2, "lm"))
  expect_true(inherits(m2[["x"]], "matrix"))
  expect_type(m2[["y"]], "double")
  expect_null(m2[["qr"]])

  m2 <- JWileymisc:::lm2(mpg ~ 0, data = mtcars,
                         designMatrix = x[, -2, drop = FALSE],
                         yObserved = y,
                         offset = mtcars$hp)

  expect_true(inherits(m2, "lm"))
})

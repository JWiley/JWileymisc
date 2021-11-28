test_that("APAStyler works with linear models", {
  m0 <- stats::lm(mpg ~  1, data = mtcars)
  m1 <- stats::lm(mpg ~  hp, data = mtcars)

  expect_invisible(APAStyler(m0))
  expect_output(APAStyler(m1, file = ""))

  expect_type(
    APAStyler(list(m0, m1)),
    "list")

  m2 <- stats::lm(mpg ~  factor(cyl), data = mtcars)
  out <- APAStyler(m2)
  expect_true(inherits(out, "matrix"))
  expect_equal(nrow(out), 5)

  out <- APAStyler(m2, pcontrol = list(stars = FALSE))
  expect_true(inherits(out, "matrix"))
  expect_equal(nrow(out), 5)
})

test_that("APAStyler works with vglm model tests", {
  set.seed(1234)
  JWileymisc_Sample_Test_Data <- data.frame(
    Outcome = factor(sample(letters[1:3], 20 * 9, TRUE)),
    C1 = rnorm(20 * 9),
    D3 = sample(paste0("L", 1:3), 20 * 9, TRUE))

  JWileymisc_Sample_Test_Data <<- JWileymisc_Sample_Test_Data

  m <- VGAM::vglm(Outcome ~ factor(D3),
                  family = VGAM::multinomial(),
                  data = JWileymisc_Sample_Test_Data)
  mt <- modelTest(m)

  out <- APAStyler(mt)
  expect_s3_class(out, "data.table")

  ## works without stars too
  out <- APAStyler(mt, pcontrol = list(
    digits = 3, includeP = TRUE, includeSign = TRUE,
    dropLeadingZero = TRUE, stars = FALSE))
  expect_s3_class(out, "data.table")
})

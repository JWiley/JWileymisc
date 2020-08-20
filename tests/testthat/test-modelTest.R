context("modelPerformance and R2")

test_that("is.modelPerformance and as.modelPerformance work.", {
  expect_false(is.modelPerformance(mtcars))
  expect_error(as.modelPerformance(1))
  expect_error(as.modelPerformance(list(1)))
  expect_error(as.modelPerformance(list(data.frame(Model = "test"))))
})

test_that("modelPerformance and R2 work with linear models", {
  expect_is(
    modelPerformance(lm(mpg ~ 1, data = mtcars))$Performance,
    "data.table")
  expect_is(
    modelPerformance(lm(mpg ~ hp, data = mtcars))$Performance,
    "data.table")

  expect_equivalent(
    R2(lm(mpg ~ 1, data = mtcars)),
    c(0, 0))
})

context("modelCompare")

test_that("is.modelCompare and as.modelCompare work.", {
  expect_false(is.modelCompare(mtcars))
  expect_error(as.modelCompare(1))
  expect_error(as.modelCompare(list(1)))
  expect_error(as.modelCompare(list(data.frame(Model = "test"))))
  expect_error(as.modelCompare(list(data.table::data.table(Model = "test"))))
})

test_that("modelCompare works with linear models", {
  m1 <- lm(mpg ~ 1, data = mtcars)
  m2 <- lm(mpg ~ hp, data = mtcars)

  expect_error(modelCompare(m1, mtcars))

  expect_error(modelCompare(m1, m1))

  c1 <- modelCompare(m1, m2)
  expect_is(c1$Comparison, "data.table")
  expect_equivalent(
    c1$Comparison$N_Obs,
    c(32, 32, 0))
  expect_equivalent(
    c1$Comparison$FNumDF,
    c(NA, 1, 1))

  c2 <- modelCompare(m2, m1)
  expect_is(c2$Comparison, "data.table")
  expect_equivalent(
    c2$Comparison$N_Obs,
    c(32, 32, 0))
  expect_equivalent(
    c2$Comparison$FNumDF,
    c(NA, 1, 1))
})



context("modelTest")

test_that("is.modelTest and as.modelTest work.", {
  expect_false(is.modelTest(mtcars))
  expect_error(as.modelTest(1))
  expect_error(as.modelTest(list(1)))
  expect_is(
    as.modelTest(list(1, 2, 3, 4)),
    "modelTest")
})

test_that("modelTest works with lm objects.", {
  mt1 <- modelTest(lm(mpg ~ hp, data = mtcars))
  expect_is(
    mt1,
    "modelTest.lm")
  expect_is(mt1$FixedEffects,
            "data.table")
  expect_equivalent(
    nrow(mt1$FixedEffects),
    2L)
  expect_equivalent(
    nrow(mt1$EffectSizes),
    1L)

  out <- APAStyler(mt1)
  expect_is(out, "data.table")
  expect_equivalent(
    dim(out), c(11L, 3L))

  ## APAStyler on lists works
  mt2 <- modelTest(lm(mpg ~ wt, data = mtcars))
  expect_is(
    APAStyler(list(mt1, mt2), pcontrol = list(
                                digits = 3, stars = FALSE,
                                includeP = TRUE, includeSign = TRUE,
                                dropLeadingZero = TRUE)),
    "data.table") 
})

test_that("modelTest errors with on the fly variable creation", {
  expect_error(modelTest(lm(mpg ~ I(wt * am), data = mtcars)))
})

test_that("modelTest works with vglm objects.", {
  mtcars$cyl <- factor(mtcars$cyl)
  m <- VGAM::vglm(cyl ~ qsec,
                  family = VGAM::multinomial(), data = mtcars)
  mt <- modelTest(m)
  expect_is(mt, "modelTest.vglm")
  expect_is(APAStyler(mt), "data.table")
  expect_is(APAStyler(mt, OR = FALSE), "data.table")
})


test_that("modelTest works with vglm objects with multiple predictors.", {
  mtcars$cyl <- factor(mtcars$cyl)
  set.seed(1234)
  expect_warning(
    m <- VGAM::vglm(cyl ~ jitter(qsec) + jitter(hp, 2),
                    family = VGAM::multinomial(), data = mtcars))
  expect_warning(mt <- modelTest(m))
  expect_is(mt, "modelTest.vglm")
  expect_is(APAStyler(mt), "data.table")
  expect_is(APAStyler(mt, OR = FALSE), "data.table")
})

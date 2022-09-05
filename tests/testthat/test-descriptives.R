test_that("roundedfivenum works", {
  expect_equal(
    JWileymisc:::roundedfivenum(1:5),
    1:5, ignore_attr = TRUE)
  expect_equal(
    JWileymisc:::roundedfivenum(c(.1, .3, .5, .7, .9), round = 0),
    c(0, 0, 0, 1, 1), ignore_attr = TRUE)
  expect_equal(
    JWileymisc:::roundedfivenum(mtcars$mpg, round = 2, sig = 1),
    c(10, 20, 20, 20, 30), ignore_attr = TRUE)
})

test_that("f.r2 works", {
  expect_equal(
    length(JWileymisc:::f.r2(.5, 4, 100)),
    4L)
})

test_that("meanCircular works", {
  expect_equal(
    meanCircular(c(23, 1), max = 24),
    0, ignore_attr = TRUE)
  expect_equal(
    meanCircular(c(23, 1, NA), max = 24, na.rm = TRUE),
    0, ignore_attr = TRUE)
  expect_equal(
    meanCircular(1:3, max = 24, na.rm = TRUE),
    2, ignore_attr = TRUE)
  expect_equal(
    meanCircular(21:23, max = 24, na.rm = TRUE),
    22, ignore_attr = TRUE)
  expect_equal(
    meanCircular(c(6, 21), max = 24),
    1.5, ignore_attr = TRUE)
  expect_equal(
    meanCircular(c(6, 23), max = 24),
    2.5, ignore_attr = TRUE)
  expect_equal(
    meanCircular(c(NA_real_), max = 24, na.rm = TRUE),
    NA_real_, ignore_attr = TRUE)
  expect_equal(
    meanCircular(c(23, 1, NA), max = 24, na.rm = FALSE),
    NA_real_, ignore_attr = TRUE)
  expect_equal(
    meanCircular(24, max = 24),
    0, ignore_attr = TRUE)
  expect_equal(
    meanCircular(c(355, 5, 15), max = 360),
    5, ignore_attr = TRUE)
  expect_error(meanCircular("a"))
  expect_error(meanCircular(99, max = 24))
  expect_error(meanCircular(-1, max = 24))
})

test_that("diffCircular works", {
  expect_equal(
    diffCircular(23, 1, max = 24),
    2, ignore_attr = TRUE)
  expect_equal(
    diffCircular(22, 1, max = 24),
    3, ignore_attr = TRUE)
  expect_equal(
    diffCircular(1, 1, max = 24),
    0, ignore_attr = TRUE)
  expect_equal(
    diffCircular(24, 24, max = 24),
    0, ignore_attr = TRUE)
  expect_equal(
    diffCircular(0, 0, max = 24),
    0, ignore_attr = TRUE)
  expect_equal(
    diffCircular(1, 3, max = 24),
    2, ignore_attr = TRUE)
  expect_error(
    diffCircular(1, 33, max = 24))
  expect_error(
    diffCircular("a", 3, max = 24))
  expect_error(
    diffCircular(3, "b", max = 24))
  expect_error(
    diffCircular(-3, 3, max = 24))
})

test_that("cramerV works", {
  expect_error(cramerV(xtabs(~ am + vs + cyl, data = mtcars)))
  expect_equal(
    length(cramerV(xtabs(~ am + vs, data = mtcars))),
    1L)
  expect_equal(
    length(cramerV(xtabs(~ am + cyl, data = mtcars))),
    1L)
})

test_that("smd works", {
  expect_equal(
    smd(c(1:3, 4:6), rep(1:2, each = 3)),
    3, ignore_attr = TRUE)
  expect_equal(
    smd(c(1:3, 4:6), factor(rep(1:2, each = 3))),
    3, ignore_attr = TRUE)
  expect_equal(
    smd(c(1:3, 3, 5, 7), rep(1:2, each = 3), index = "1"),
    3, ignore_attr = TRUE)
  expect_equal(
    smd(c(0, 2, 4, 4:6), rep(1:2, each = 3), index = "2"),
    3, ignore_attr = TRUE)
  expect_error(smd(mtcars$mpg, mtcars$cyl))
})

test_that("egltable works", {
  t1 <- egltable("mpg", data = mtcars)
  expect_s3_class(t1, "data.table")
  expect_equal(nrow(t1), 1L)
  expect_equal(dim(t1), c(1L, 2L))

  t2 <- egltable("mpg", "vs", data = mtcars)
  expect_s3_class(t2, "data.table")
  expect_equal(nrow(t2), 1L)
  expect_equal(dim(t2), c(1L, 4L))

  t3 <- egltable("mpg", "cyl", data = mtcars)
  expect_s3_class(t3, "data.table")
  expect_equal(nrow(t3), 1L)
  expect_equal(dim(t3), c(1L, 5L))

  t4 <- egltable(c("mpg", "am"), data = mtcars)
  expect_s3_class(t4, "data.table")
  expect_equal(nrow(t4), 2L)
  expect_equal(dim(t4), c(2L, 2L))

  t5 <- egltable(c("mpg", "am"), data = mtcars, strict = FALSE)
  expect_s3_class(t5, "data.table")
  expect_equal(nrow(t5), 4L)
  expect_equal(dim(t5), c(4L, 2L))

  t6 <- egltable(mtcars$mpg)
  expect_s3_class(t6, "data.table")
  expect_equal(nrow(t6), 1L)
  expect_equal(dim(t6), c(1L, 2L))

  t7 <- egltable(mtcars[, c("mpg", "hp")])
  expect_s3_class(t7, "data.table")
  expect_equal(nrow(t7), 2L)
  expect_equal(dim(t7), c(2L, 2L))

  t8 <- egltable(mtcars[, c("mpg", "hp")], parametric = FALSE)
  expect_s3_class(t8, "data.table")
  expect_equal(nrow(t8), 2L)
  expect_equal(dim(t8), c(2L, 2L))

  t9 <- egltable(mtcars[, c("mpg", "cyl")], strict = FALSE, parametric = FALSE)
  expect_s3_class(t9, "data.table")
  expect_equal(nrow(t9), 5L)
  expect_equal(dim(t9), c(5L, 2L))

  t10 <- egltable(c("mpg", "cyl"), data = data.table::as.data.table(mtcars),
                  strict = FALSE, parametric = FALSE)
  expect_s3_class(t10, "data.table")
  expect_equal(nrow(t10), 5L)
  expect_equal(dim(t10), c(5L, 2L))

  t11 <- egltable(c("cyl"), data = mtcars,
                  strict = FALSE, parametric = FALSE)
  expect_s3_class(t11, "data.table")
  expect_equal(nrow(t11), 4L)
  expect_equal(dim(t11), c(4L, 2L))

  t12 <- egltable(c("mpg", "hp"), "cyl", data = mtcars, parametric = FALSE)
  expect_s3_class(t12, "data.table")
  expect_equal(nrow(t12), 2L)
  expect_equal(dim(t12), c(2L, 5L))

  expect_warning(t13 <- egltable(c("am"), "cyl", data = mtcars, strict = FALSE))
  expect_s3_class(t13, "data.table")
  expect_equal(nrow(t13), 3L)
  expect_equal(dim(t13), c(3L, 5L))


  tmp <- subset(ChickWeight, Time %in% c(0, 20))
  tmp$WeightTertile <- cut(tmp$weight,
                           breaks = quantile(tmp$weight, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                           include.lowest = TRUE)
  t14 <- egltable(c("weight", "WeightTertile"), g = "Time",
                  data = tmp,
                  idvar = "Chick", paired = TRUE)

  expect_s3_class(t14, "data.table")
  expect_true(any(grepl("McNemar", t14$Test)))
  expect_true(any(grepl("p < .001", t14$Test)))

  expect_warning(t15 <- egltable(c("weight", "WeightTertile"), g = "Time",
                  data = tmp,
                  idvar = "Chick", paired = TRUE,
                  parametric = FALSE))

  expect_s3_class(t15, "data.table")
  expect_true(any(grepl("Wilcoxon", t15$Test)))
  expect_true(any(grepl("McNemar", t15$Test)))
  expect_true(any(grepl("p < .001", t15$Test)))

  tmp$Chick[1] <- NA
  expect_error(t16 <- egltable(c("weight", "WeightTertile"), g = "Time",
                  data = tmp,
                  idvar = "Chick", paired = TRUE))
})

test_that("egltable errors when it should", {
  expect_error(egltable("a", data = data.frame(a = NA)))
  expect_error(egltable("a", g = "g", data = data.frame(a = c(1, NA), g = c(1, 2))))
})

test_that("empirical_pvalue returns correct p-values", {
  expect_equal(
    empirical_pvalue(c(-1, 1:3))["p-value"],
    .5, ignore_attr = TRUE)
})

test_that("cd changes directory", {
  randomDIR <- paste(sample(c(letters, 0:9), 30), collapse = "")
  while (file.exists(randomDIR)) {
    randomDIR <- paste(sample(c(letters, 0:9), 30), collapse = "")
  }

  dir.create(randomDIR)
  cd(randomDIR)
  expect_true(grepl(randomDIR, getwd()))
  setwd("../")
  unlink(randomDIR, TRUE, TRUE)

  ## cd() can create a new directory
  cd(randomDIR)
  expect_true(grepl(randomDIR, getwd()))
  setwd("../")
  unlink(randomDIR, TRUE, TRUE)

  curdir <- getwd()
  tdir <- tempdir()
  setwd(tdir)
  newdir <- file.path(tdir, paste0("abc", 2))
  cd(tdir, "abc", 2)
  expect_equal(
    normalizePath(newdir, mustWork = FALSE),
    normalizePath(getwd(), mustWork = FALSE))
  unlink(newdir, TRUE, TRUE)
  setwd(curdir)
})

test_that("cor2cov is equivalent to covariance matrix", {
  expect_equal(
    cov(mtcars[, 1:4]),
    cor2cov(cor(mtcars[, 1:4]), sapply(mtcars[, 1:4], sd)))
})

test_that("cor2cov catches errors", {
  expect_error(cor2cov("a"))
  expect_error(cor2cov(cor(mtcars[, 1:4]), 1:2))
  expect_warning(cor2cov(matrix(1:9, 3, 3), 1:3))
})

test_that("corOK removes missing values correctly", {
  cormat <- cor(iris[, -5])
  cormat[cbind(c(1, 2), c(2, 1))] <- NA
  cormat <- corOK(cormat)
  expect_equal(dim(cormat$x), c(3, 3))
  expect_equal(cormat$keep.indices, c(2, 3, 4))

  cormat <- cor(iris[, -5])
  cormat[cbind(c(1, 2), c(2, 1))] <- NA
  expect_warning(corOK(cormat, 1))
})

test_that("as.na converts to the correct class of missing", {
  expect_equal(as.na(1.5), NA_real_)
  expect_equal(as.na(TRUE), NA)
  expect_equal(as.na(1L), NA_integer_)
  expect_equal(as.na("x"), NA_character_)

  expect_true(is.factor(as.na(factor("x"))))
  expect_s3_class(as.na(as.POSIXct("1990-01-01 10:40:04")), c("POSIXct", "POSIXt"))
  expect_s3_class(as.na(as.POSIXct("1990-01-01 10:40:04")), c("POSIXct", "POSIXt"))
  expect_s3_class(as.na(as.POSIXlt("1990-01-01 10:40:04")), c("POSIXlt", "POSIXt"))

  ## check times (from chron package) convert properly
  x <- structure(0.819513888888889, format = "h:m:s", class = "times")
  expect_true(all(is.na(as.na(x))))
  expect_equal(class(as.na(x)), class(x))

  ## check dates convert
  expect_equal(class(as.na(as.Date("2020-01-01"))), class(as.Date("2020-01-01")))

  ## check zoo converts properly
  x <- structure(1:3, index = 1:3, class = "zoo")
  expect_true(all(is.na(as.na(x))))
  expect_equal(class(as.na(x)), class(x))

  ## check haven labelleds convert properly
  x <- structure(c(7, 3, 4),
    label = "How would you rate your stress levels today?",
    format.spss = "F40.0", display_width = 5L, labels = c(`Very low` = 1,  
    Low = 2, `Somewhat low` = 3, `Neither low nor high` = 4, `Somewhat high` = 5,        
    High = 6, `Very High` = 7),
    class = c("haven_labelled", "vctrs_vctr", "double"))
  expect_equal(class(as.na(x)), class(x))

  ## check error if unknown class
  x <- structure(1, class = "whoknows")
  expect_error(as.na(x))
})

test_that("is.naz identifies missing, non finite, and zero length characters", {
  expect_identical(is.naz(c(1.5, NA, Inf)), c(FALSE, TRUE, TRUE))
  expect_identical(is.naz(c(1L, NA, 2L)), c(FALSE, TRUE, FALSE))
  expect_identical(is.naz(c("test", "", NA_character_)), c(FALSE, TRUE, TRUE))
})

test_that("naz.omit removes missing, nan, and zero length characters", {
  expect_identical(length(naz.omit(c("test", "", NA_character_))), 1L)
  expect_identical(length(naz.omit(c(1, NA))), 1L)
})

test_that(".allmissing returns FALSE if nothing missing", {
  expect_false(JWileymisc:::.allmissing(mtcars))
})

test_that(".allmissing message if anything all missing", {
  expect_true(is.character(JWileymisc:::.allmissing(data.frame(a = NA, b = 1))))
})

test_that("lagk lags by k", {
  expect_equal(lagk(1:3, 1), c(NA, 1, 2))
  expect_equal(lagk(1:3, 2), c(NA, NA, 1))
  expect_equal(
    lagk(1:4, 1, factor(c("a", "a", "b", "b"))),
    c(a1 = NA, a2 = 1,
      b1 = NA, b2 = 3))

  expect_true(all(is.na(lagk(1:3, k = 10))))
})

test_that("timeshift works", {
  expect_equal(
    timeshift(c(0, 3, 6), center = 0, min = 0, max = 6),
    c(0, 3, 6))

  expect_equal(
    timeshift(c(0, 3, 6), center = 3, min = 0, max = 6),
    c(3, 0, 3))

  expect_equal(
    timeshift(c(0, .5, .9), center = .5),
    c(.5, 0, .4))

  expect_equal(
    timeshift(c(.5, 0, .4), center = .5, inverse = TRUE),
    c(0, .5, .9))

  expect_error(timeshift(c(0, 3, 6), center = 0, min = 1, max = 6))
  expect_error(timeshift(c(0, 3, 6), center = 3, min = 1, max = 6))
  expect_error(timeshift(c(0, 3, 7), center = 3, min = 0, max = 6))

  expect_error(timeshift(c(0, 3, 5), center = 0, min = 0, max = 6, inverse = "what?"))

})

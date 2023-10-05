test_that("moments works with missing data.", {
     Xmiss <- as.matrix(iris[, -5])
     Xmiss <- withr::with_seed(seed = 10, {
       Xmiss[sample(length(Xmiss), length(Xmiss) * .25)] <- NA
       Xmiss
     })
     Xmiss <- as.data.frame(Xmiss)

     m <- moments(Xmiss)

     expect_type(m, "list")
     expect_true(inherits(m$sigma, "matrix"))
     expect_type(m$mu, "double")

     expect_identical(length(m$mu), 4L)
     expect_identical(dim(m$sigma), c(4L, 4L))

     expect_false(anyNA(m$mu))
     expect_false(anyNA(m$sigma))
})

test_that("moments works with complete data.", {
     m <- moments(iris[, -5])

     expect_type(m, "list")
     expect_true(inherits(m$sigma, "matrix"))
     expect_type(m$mu, "double")

     expect_identical(length(m$mu), 4L)
     expect_identical(dim(m$sigma), c(4L, 4L))

     expect_false(anyNA(m$mu))
     expect_false(anyNA(m$sigma))
})

test_that("moments works with complete matrix data.", {
     expect_warning(m <- moments(matrix(1:100, ncol = 4)))

     expect_type(m, "list")
     expect_true(inherits(m$sigma, "matrix"))
     expect_type(m$mu, "double")

     expect_identical(length(m$mu), 4L)
     expect_identical(dim(m$sigma), c(4L, 4L))

     expect_false(anyNA(m$mu))
     expect_false(anyNA(m$sigma))
})

test_that("SEMSummary works with complete matrix data.", {
  s <- SEMSummary(~., data = matrix(1:50, ncol = 2))
  expect_s3_class(s, "SEMSummary")
  expect_true(inherits(s$Sigma, "matrix"))
  expect_identical(dim(s$Sigma), c(2L, 2L))

  expect_s3_class(
    corplot(s$sSigma, order = "cluster"),
    "ggplot")
  expect_s3_class(
    corplot(s$sSigma, coverage = s$coverage, pvalues = s$pvalue,
            order = "asis"),
    "ggplot")
  expect_s3_class(
    corplot(s$sSigma, coverage = s$coverage, pvalues = s$pvalue,
            type = "p", order = "asis"),
    "ggplot")
  expect_s3_class(
    corplot(s$sSigma, coverage = s$coverage, pvalues = s$pvalue,
            type = "coverage", order = "asis"),
    "ggplot")

})


test_that("SEMSummary works with complete data.", {
  s <- SEMSummary(~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)

  expect_s3_class(s, "SEMSummary")
  expect_true(inherits(s$Sigma, "matrix"))
  expect_identical(dim(s$Sigma), c(3L, 3L))
})


test_that("SEMSummary works with missing data.", {
     Xmiss <- as.matrix(iris[, -5])
     set.seed(10)
     Xmiss[sample(length(Xmiss), length(Xmiss) * .25)] <- NA
     Xmiss <- as.data.frame(Xmiss)

     s <- SEMSummary(~ ., data = Xmiss, use = "fiml")
     expect_s3_class(s, "SEMSummary")
     expect_true(inherits(s$Sigma, "matrix"))
     expect_identical(dim(s$Sigma), c(4L, 4L))

     expect_s3_class(
       corplot(s$sSigma, coverage = s$coverage, pvalues = s$pvalue,
               type = "coverage", order = "asis"),
       "ggplot")

     expect_s3_class(plot(s), "ggplot")

     s <- SEMSummary(~ ., data = Xmiss, use = "pairwise.complete.obs")
     expect_s3_class(s, "SEMSummary")
     expect_true(inherits(s$Sigma, "matrix"))
     expect_identical(dim(s$Sigma), c(4L, 4L))

     s <- SEMSummary(~ ., data = Xmiss, use = "complete.obs")
     expect_s3_class(s, "SEMSummary")
     expect_true(inherits(s$Sigma, "matrix"))
     expect_identical(dim(s$Sigma), c(4L, 4L))

  ## styler methods work
     expect_invisible(
       APAStyler(s, type = "cor", stars = TRUE,
                 file = FALSE, print = FALSE))
    
     expect_output(
       APAStyler(s, type = "cor", stars = TRUE,
                 file = "", print = FALSE),
       "Sepal.Length")
})

test_that("SEMSummary works by group.", {
  s <- SEMSummary(~ Sepal.Length + Sepal.Width | Species, data = iris)

  expect_s3_class(s, "SEMSummary.list")
  expect_true(inherits(s[[1]]$Sigma, "matrix"))
  expect_identical(dim(s[[1]]$Sigma), c(2L, 2L))

  ## plot methods work
  expect_invisible(sp <- plot(s, plot = FALSE))
  expect_type(sp, "list")
  expect_length(sp, 3)
  expect_s3_class(sp[[1]], "ggplot")
  expect_s3_class(sp[[2]], "ggplot")
  expect_s3_class(sp[[3]], "ggplot")

  expect_invisible(plot(s, plot = TRUE))
  expect_invisible(plot(s, which = 1, plot = FALSE))

  ## styler methods work
  expect_invisible(APAStyler(s[[1]], type = "cor", stars = TRUE, file = FALSE, print = FALSE))
  expect_invisible(APAStyler(s[[2]], type = "cov", stars = FALSE, file = FALSE, print = FALSE))
  expect_invisible(APAStyler(s[[3]], type = "both", stars = TRUE, file = FALSE, print = FALSE))


})

test_that("SEMSummary or SEMSummary.fit errors.", {
  expect_error(SEMSummary(~ Sepal.Length + Sepal.Width | ., data = iris))

  expect_error(SEMSummary(~ Sepal.Length, data = iris))

})

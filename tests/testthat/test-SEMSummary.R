context("SEMSummary")

test_that("moments works with missing data.", {
     Xmiss <- as.matrix(iris[, -5])
     set.seed(10)
     Xmiss[sample(length(Xmiss), length(Xmiss) * .25)] <- NA
     Xmiss <- as.data.frame(Xmiss)

     m <- moments(Xmiss)

     expect_that(m, is_a("list"))
     expect_that(m$sigma, is_a("matrix"))
     expect_that(m$mu, is_a("numeric"))

     expect_identical(length(m$mu), 4L)
     expect_identical(dim(m$sigma), c(4L, 4L))

     expect_false(anyNA(m$mu))
     expect_false(anyNA(m$sigma))
})

test_that("moments works with complete data.", {
     m <- moments(iris[, -5])

     expect_that(m, is_a("list"))
     expect_that(m$sigma, is_a("matrix"))
     expect_that(m$mu, is_a("numeric"))

     expect_identical(length(m$mu), 4L)
     expect_identical(dim(m$sigma), c(4L, 4L))

     expect_false(anyNA(m$mu))
     expect_false(anyNA(m$sigma))
})


test_that("SEMSummary works with complete data.", {
  s <- SEMSummary(~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)

  expect_that(s, is_a("SEMSummary"))
  expect_that(s$Sigma, is_a("matrix"))
  expect_identical(dim(s$Sigma), c(3L, 3L))
})


test_that("SEMSummary works with missing data.", {
     Xmiss <- as.matrix(iris[, -5])
     set.seed(10)
     Xmiss[sample(length(Xmiss), length(Xmiss) * .25)] <- NA
     Xmiss <- as.data.frame(Xmiss)

     s <- SEMSummary(~ ., data = Xmiss, use = "fiml")

     expect_that(s, is_a("SEMSummary"))
     expect_that(s$Sigma, is_a("matrix"))
     expect_identical(dim(s$Sigma), c(4L, 4L))
})

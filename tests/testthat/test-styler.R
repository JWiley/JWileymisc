context("APAStyler")

test_that("APAStyler works with linear models", {
  m0 <- stats::lm(mpg ~  1, data = mtcars)
  m1 <- stats::lm(mpg ~  hp, data = mtcars)

  expect_invisible(APAStyler(m0))
  expect_output(APAStyler(m1, file = ""))

  expect_is(
    APAStyler(list(m0, m1)),
    "list")

  m2 <- stats::lm(mpg ~  factor(cyl), data = mtcars)
  out <- APAStyler(m2)
  expect_is(out, "matrix")
  expect_equal(nrow(out), 5)

  out <- APAStyler(m2, pcontrol = list(stars = FALSE))
  expect_is(out, "matrix")
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
  expect_is(out, "data.table")

  ## works without stars too
  out <- APAStyler(mt, pcontrol = list(digits = 3, includeP=TRUE, includeSign = TRUE,
                                       dropLeadingZero=TRUE, stars = FALSE))
  expect_is(out, "data.table")
})


context("star")

test_that("star works", {
  expect_equivalent(
    star(c(.06, .05, .004)),
    noquote(c("", "*", "**")))

  expect_equivalent(
    star(c(.06, .05, .004), includeMarginal = TRUE),
    noquote(c("^", "*", "**")))
})


context("formatPval")

test_that("formatPval works", {
  expect_equivalent(
    formatPval(c(.00052456, .01035, .534946)),
    c("< .001", ".010", ".535"))

  expect_equivalent(
    formatPval(c(.00052456, .000000124, .01035, .030489, .534946),
               d = 3, sd = 3,
               includeP = FALSE,
               includeSign = TRUE),
    c("< .001", "< .001", "= .010", "= .030", "= .535"))

  expect_equivalent(
    formatPval(c(.00052456, .000000124, .01035, .030489, .534946),
               d = 3, sd = 3,
               includeP = TRUE,
               includeSign = TRUE),
    c("p < .001", "p < .001", "p = .010", "p = .030", "p = .535"))

  expect_equivalent(
    formatPval(c(.00052456, .01035, .534946), d = 5),
    c(".00052", ".01035", ".53495"))
})

context("param_summary")

test_that("empirical_pvalue works", {
  expect_equivalent(
    empirical_pvalue(c(-1, 1))[["p-value"]],
    1)
  expect_equivalent(
    empirical_pvalue(rep(c(-1, 1), c(3, 1)))[["p-value"]],
    .5)
  expect_equivalent(
    empirical_pvalue(rep(c(-1, 1), c(9, 1)))[["p-value"]],
    .2)

})

test_that("param_summary works", {
  expect_is(
    param_summary(1:10),
    "data.table")

  out <- param_summary_format(param_summary(1:9), digits = 2, pretty = TRUE)
  expect_is(out, "character")
  expect_true(grepl("5.0", out))

  expect_is(
    param_summary_format(param_summary(1:10), digits = 2, pretty = FALSE),
    "data.table")
})

context("formatHtest")

test_that("formatHtest works", {
  expect_true(
    grepl("^t\\(df",
          formatHtest(t.test(extra ~ group, data = sleep), type = "t")))

  expect_true(
    grepl("^F\\(2",
          formatHtest(anova(aov(mpg ~ factor(cyl), data = mtcars)), type = "F")))

  expect_true(
    grepl("^Chi-square\\(df",
          formatHtest(chisq.test(c(A = 20, B = 15, C = 25)), type = "chisq")))

  expect_true(
    grepl("^Kruskal-Wallis chi-square\\(df",
          formatHtest(kruskal.test(Ozone ~ Month, data = airquality), type = "kw")))

  expect_true(
    grepl("^Mantel-Haenszel chi-square\\(df",
          formatHtest(mantelhaen.test(UCBAdmissions), type = "mh")))

  expect_true(
    grepl("^r = -",
          formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "pearson"),
                      type = "r_pearson")))

  expect_warning(
    kr <- formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "kendall"),
                      type = "r_kendall"))
  expect_true(grepl("^tau = -", kr))

  expect_warning(
    sr <- formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "spearman"),
                      type = "r_spearman"))
  expect_true(grepl("rho = -", sr))
})

context("formatting and style")

test_that("formatMedIQR works", {
  expect_is(
    formatMedIQR(1:10),
    "character")
  expect_true(grepl("^5.50", formatMedIQR(1:10)))
})

test_that(".fround works", {
  expect_equivalent(
    .fround(5.2453, 2),
    "5.25")
  expect_equivalent(
    .fround(5.000014, 2),
    "5.00")
})

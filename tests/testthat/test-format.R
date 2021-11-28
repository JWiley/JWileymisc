test_that("formatPval works", {
  expect_equal(
    formatPval(c(.00052456, .01035, .534946)),
    c("< .001", ".010", ".535"))

  expect_equal(
    formatPval(c(.00052456, .000000124, .01035, .030489, .534946),
               d = 3, sd = 3,
               includeP = FALSE,
               includeSign = TRUE),
    c("< .001", "< .001", "= .010", "= .030", "= .535"))

  expect_equal(
    formatPval(c(.00052456, .000000124, .01035, .030489, .534946),
               d = 3, sd = 3,
               includeP = TRUE,
               includeSign = TRUE),
    c("p < .001", "p < .001", "p = .010", "p = .030", "p = .535"))

  expect_equal(
    formatPval(c(.00052456, .01035, .534946), d = 5),
    c(".00052", ".01035", ".53495"))
})

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

test_that("formatMedIQR works", {
  expect_type(
    formatMedIQR(1:10),
    "character")
  expect_true(grepl("^5.50", formatMedIQR(1:10)))
})

test_that(".fround works", {
  expect_equal(
    .fround(5.2453, 2),
    "5.25")
  expect_equal(
    .fround(5.000014, 2),
    "5.00")
})

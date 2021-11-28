test_that("empirical_pvalue works", {
  expect_equal(
    empirical_pvalue(c(-1, 1))[["p-value"]],
    1)
  expect_equal(
    empirical_pvalue(rep(c(-1, 1), c(3, 1)))[["p-value"]],
    .5)
  expect_equal(
    empirical_pvalue(rep(c(-1, 1), c(9, 1)))[["p-value"]],
    .2)

})

test_that("param_summary works", {
  expect_s3_class(
    param_summary(1:10),
    "data.table")

  out <- param_summary_format(param_summary(1:9), digits = 2, pretty = TRUE)
  expect_type(out, "character")
  expect_true(grepl("5.0", out))

  expect_s3_class(
    param_summary_format(param_summary(1:10), digits = 2, pretty = FALSE),
    "data.table")
})

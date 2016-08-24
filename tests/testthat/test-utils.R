context("Utilities")

test_that("empirical_pvalue returns correct p-values", {
  expect_that(empirical_pvalue(c(-1, 1:3))["p-value"], is_equivalent_to(.5))
})

test_that("star works", {
  expect_equal(
    star(c(.06, .05, .004)),
    noquote(c("", "*", "**")))

  expect_equal(
    star(c(.06, .05, .004), includeMarginal = TRUE),
    noquote(c("^", "*", "**")))
})

test_that("egltable reports absent variables by name", {
  # The error should name every requested column missing from data.
  expect_error(
    egltable(c("mpg", "not_here"), data = mtcars),
    "not_here")
})

test_that("winsorizor coerces atomic non-vector inputs with a warning", {
  # Factors are atomic but not plain vectors, so this covers the coercion branch.
  expect_warning(
    w <- winsorizor(factor(c("1", "100")), percentile = .1),
    "Atomic type")
  expect_equal(w, c(1.1, 1.9), ignore_attr = TRUE)
})

test_that("egltable marks mixed parametric summaries", {
  # Different parametric flags across continuous variables use mixed labels.
  out <- egltable(c("mpg", "hp"), data = mtcars, parametric = c(TRUE, FALSE))
  expect_true(any(grepl("M \\(SD\\)", out[[1]])))
  expect_true(any(grepl("Mdn \\(IQR\\)", out[[1]])))
})

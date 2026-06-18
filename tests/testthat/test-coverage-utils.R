test_that("lagk handles non-factor groups with all-missing short groups", {
  # Covers conversion of grouping variables and groups shorter than k.
  expect_true(all(is.na(lagk(1:4, k = 2, by = c("a", "a", "b", "b")))))
})

test_that("hashDataset returns and writes dataset and variable hashes", {
  d <- data.frame(a = 1:2, b = c("x", "y"))
  f <- tempfile()

  h <- hashDataset(d)
  expect_length(h, ncol(d) + 1L)
  expect_match(h[1], "dataset: 'd', MD5:")
  expect_match(h[2], "'a' \\(integer\\)")

  expect_invisible(hw <- hashDataset(d, file = f))
  expect_identical(readLines(f), hw)
})

test_that("saveRDSfst and readRDSfst validate inputs and round-trip objects", {
  f <- tempfile(fileext = ".rds")
  x <- list(a = 1:3, b = "ok")

  saveRDSfst(x, f, compression = 0, algorithm = "LZ4")
  expect_identical(readRDSfst(f), x)

  # Each expectation targets one defensive input branch.
  expect_error(saveRDSfst(x, 1), "filename")
  expect_error(saveRDSfst(x, c("a", "b")), "length one")
  expect_error(saveRDSfst(x, ""), "non zero")
  expect_error(saveRDSfst(x, f, algorithm = 1), "algorithm")
  expect_error(saveRDSfst(x, f, algorithm = c("ZSTD", "LZ4")), "length one")
  expect_error(saveRDSfst(x, f, algorithm = "bad"), "algorithm")
  expect_error(saveRDSfst(x, f, compression = "high"), "compression")
  expect_error(saveRDSfst(x, f, compression = c(1, 2)), "length one")
  expect_error(saveRDSfst(x, f, compression = 101), "between 0 and 100")

  expect_error(readRDSfst(1), "filename")
  expect_error(readRDSfst(c("a", "b")), "length one")
  expect_error(readRDSfst(""), "non zero")
})

test_that("density_inversion validates input and produces seeded samples", {
  expect_error(density_inversion("x", n = 2))
  expect_error(density_inversion(c(1, Inf), n = 2))
  expect_error(density_inversion(1, n = 2))

  expect_equal(
    density_inversion(1:5, n = 4, KDEn = 32, seed = 1),
    c(1.789814, 2.351721, 3.367821, 5.343321),
    tolerance = 1e-6)
})

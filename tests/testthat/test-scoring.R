test_that("CheckVals works", {
  expect_true(CheckVals(mtcars$cyl, c(4, 6, 8)))
  expect_true(CheckVals(mtcars[, c("cyl", "am")], c(0, 1, 4, 6, 8)))
  expect_true(CheckVals(as.matrix(mtcars[, c("cyl", "am")]), c(0, 1, 4, 6, 8)))
  expect_error(CheckVals(mtcars[, c("cyl", "am")], c(0, 4, 6, 8)))

  expect_error(
    CheckVals(as.POSIXct(1:10, origin = "1970-01-01 00:00:00"),
              okay = 1:10),
    "not a valid class")
})


test_that("score warnings and errors", {
  expect_warning(expect_warning(expect_warning(
    score(mtcars[, 1:3], mean = FALSE, na.rm = TRUE),
    "Summing is not meaningful for missing values.")))

  expect_error(
    score(mtcars[, 1:3],
          limits = c(-3, 30),
          rev = 2),
    "Cannot reverse score scale that can take on negative values.")

  expect_type(
    score(mtcars[, 1:3], mean = FALSE, reliability = FALSE,
          na.rm = FALSE),
    "list")
})

test_that(".scoreCESD works", {
  set.seed(1234)
  x <- matrix(sample(0:3, size = 20 * 5, TRUE), ncol = 20)
  xc <- suppressMessages(suppressWarnings(.scoreCESD(x)))
  expect_type(xc, "list")

  expect_type(.scoreCESD(x, reliability = FALSE), "list")

  expect_type(scaleScore(x, type = "CESD", reliability = FALSE), "list")

  expect_type(scaleScore(data.table::as.data.table(x),
                       type = "CESD", reliability = FALSE), "list")

  expect_match(names(xc), "score|reliability")
})

test_that(".scoreLOTR and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(1:5, size = 6 * 5, TRUE), ncol = 6)
  xc <- suppressMessages(suppressWarnings(.scoreLOTR(x)))
  expect_type(xc, "list")

  expect_type(.scoreLOTR(x, reliability = FALSE), "list")

  expect_type(scaleScore(x, type = "LOTR", reliability = FALSE), "list")

  expect_type(scaleScore(data.table::as.data.table(x),
                       type = "LOTR", reliability = FALSE), "list")
  expect_match(names(xc), "score|reliability")
})

test_that(".scoreMastery and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(1:4, size = 7 * 5, TRUE), ncol = 7)
  xc <- suppressMessages(suppressWarnings(.scoreMastery(x)))
  expect_type(xc, "list")

  expect_type(.scoreMastery(x, reliability = FALSE), "list")

  expect_type(scaleScore(x, type = "Mastery", reliability = FALSE), "list")

  expect_type(scaleScore(data.table::as.data.table(x),
                       type = "Mastery", reliability = FALSE), "list")
  expect_match(names(xc), "score|reliability")
})

test_that(".scoreMOSSSS and scaleScore work", {
  set.seed(5234)
  x <- MASS::mvrnorm(n = 200, mu = rep(0, 20),
                     Sigma = matrix(.5, 20, 20) + diag(20),
                     empirical = TRUE)
  x <- apply(x, 2, function(y) {
    as.integer(cut(y, c(-Inf, -1, -.3, .3, 1, Inf),
                   labels = 1:5))
  })

  xc <- suppressMessages(suppressWarnings(.scoreMOSSSS(x)))
  expect_type(xc, "list")

  expect_match(names(xc), "score|reliability")
  xc <- suppressWarnings(scaleScore(x, type = "MOSSSS"))  
})

test_that(".scorePANAS and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(1:5, size = 20 * 5, TRUE), ncol = 20)
  if (isTRUE(capabilities("long.double"))) {
    xc <- suppressMessages(suppressWarnings(.scorePANAS(x)))
    expect_type(xc, "list")
    expect_match(names(xc), "score|reliability")
  }

  expect_type(.scorePANAS(x, reliability = FALSE), "list")

  expect_type(scaleScore(x, type = "PANAS", reliability = FALSE), "list")

  expect_type(scaleScore(data.table::as.data.table(x),
                       type = "PANAS", reliability = FALSE), "list")
})

test_that(".scoreRSES and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(0:3, size = 10 * 5, TRUE), ncol = 10)
  xc <- suppressMessages(suppressWarnings(.scoreRSES(x)))
  expect_type(xc, "list")

  expect_type(.scoreRSES(x, reliability = FALSE), "list")

  expect_type(scaleScore(x, type = "RSES", reliability = FALSE), "list")

  expect_type(scaleScore(data.table::as.data.table(x),
                       type = "RSES", reliability = FALSE), "list")
  expect_match(names(xc), "score|reliability")
})

## weakest tests as not sure what MOOD scores should be??
test_that(".scoreMOOD and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(0:3, size = 21 * 5, TRUE), ncol = 21)
  xc <- suppressMessages(suppressWarnings(.scoreMOOD(x)))
  expect_type(xc, "list")
})

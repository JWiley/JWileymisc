test_that("modelTest.vglm rejects unsupported and too-small outcomes", {
  expect_error(
    modelTest(VGAM::vglm(mpg ~ hp, family = VGAM::uninormal(), data = mtcars)),
    "multinomial")

  d <- mtcars
  d$am <- factor(d$am)
  expect_error(
    modelTest(VGAM::vglm(am ~ hp, family = VGAM::multinomial(), data = d)),
    "at least 3 levels")
})

test_that("lm2 validates weights and offset lengths", {
  d <- data.frame(y = 1:4, x = 1:4, w = letters[1:4], off = 1:4)
  X <- stats::model.matrix(y ~ x, d)

  expect_error(
    JWileymisc:::lm2(y ~ x, data = d, weights = w, designMatrix = X, yObserved = d$y),
    "weights")

  expect_error(
    JWileymisc:::lm2(
      y ~ x + offset(off),
      data = d,
      designMatrix = X,
      yObserved = d$y[1:3]),
    "number of offsets")
})

test_that("internalcompareIV covers multivariate and no-foreach paths", {
  expect_error(
    JWileymisc:::internalcompareIV(
      dv = "mpg", type = "normal", iv = "hp",
      data = mtcars, multivariate = TRUE),
    "single IV")

  foreach::registerDoSEQ()
  expect_message(
    out <- compareIVs(
      dv = "mpg", type = "normal",
      iv = c("hp", "qsec"),
      covariates = "am",
      data = mtcars,
      multivariate = TRUE),
    "complete cases")
  expect_equal(nrow(out$OverallSummary), 6L)

  # A local copy lets this test exercise the fallback branch without changing
  # the locked package namespace or the installed foreach package.
  no_foreach <- JWileymisc:::internalcompareIV
  environment(no_foreach) <- list2env(
    list(requireNamespace = function(pkg, quietly = FALSE) FALSE),
    parent = environment(JWileymisc:::internalcompareIV))

  expect_null(no_foreach(
    dv = "mpg", type = "normal",
    iv = c("hp", "qsec"),
    covariates = "am",
    data = mtcars,
    multivariate = FALSE))
  expect_message(
    expect_null(no_foreach(
      dv = "mpg", type = "normal",
      iv = c("hp", "qsec"),
      covariates = "am",
      data = mtcars,
      multivariate = TRUE)),
    "complete cases")
})

local_rms_interaction <- function(slope, noise, intercept = 0, seed = 1, n = 60) {
  set.seed(seed)
  d <- data.frame(
    x = rep(seq(-2, 2, length.out = n), 2),
    g = rep(c("a", "b"), each = n))
  d$y <- 1 + d$x + ifelse(d$g == "b", intercept + slope * d$x, 0) +
    stats::rnorm(nrow(d), 0, noise)

  dd_name <- paste0("JWileymisc_test_datadist_", sample.int(1e7, 1))
  assign(dd_name, rms::datadist(d), envir = .GlobalEnv)
  withr::defer(
    if (exists(dd_name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = dd_name, envir = .GlobalEnv)
    },
    envir = parent.frame())
  withr::local_options(list(datadist = dd_name), .local_envir = parent.frame())

  rms::ols(y ~ x * g, data = d, x = TRUE, y = TRUE)
}

test_that("findSigRegions uses current rms contrast output", {
  m <- local_rms_interaction(slope = .5, noise = .5)

  # Real rms::contrast() output is used to find an alpha threshold.
  threshold <- findSigRegions(
    m,
    l1 = list(g = "a"),
    l2 = list(g = "b"),
    name.vary = "x",
    lower = -2,
    upper = 2,
    starts = 11)
  expect_s3_class(threshold, "data.table")
  expect_true(any(!is.na(threshold$Pvalue)))
  expect_equal(threshold$Pvalue[1], .05, tolerance = 1e-4)

  # An impossible alpha keeps the current invalid-result note path covered.
  invalid <- findSigRegions(
    m,
    l1 = list(g = "a"),
    l2 = list(g = "b"),
    name.vary = "x",
    lower = -2,
    upper = 2,
    alpha = -10,
    starts = 1)
  expect_true(grepl("Invalid result", invalid$Notes))

  # Non-finite objectives from rms/alpha are guarded and treated as invalid.
  nonfinite <- findSigRegions(
    m,
    l1 = list(g = "a"),
    l2 = list(g = "b"),
    name.vary = "x",
    lower = -2,
    upper = 2,
    alpha = NA_real_,
    starts = 1)
  expect_true(grepl("Invalid result", nonfinite$Notes))
})

test_that("intSigRegGraph builds graph output with real rms predictions", {
  m <- local_rms_interaction(slope = .5, noise = .5)
  pred <- list(x = seq(-2, 2, length.out = 5), g = c("a", "b"))
  contrasts <- list(
    list(c(x = -2, x = 2), list(g = "a")),
    list(c(x = -2, x = 2), list(g = "b")))

  out <- intSigRegGraph(
    m,
    predList = pred,
    contrastList = contrasts,
    xvar = "x",
    varyvar = "g",
    xlim = c(-2, 2),
    ylim = c(-3, 5),
    xbreaks = -2:2,
    starts = 11)
  expect_s3_class(out$Graph, "ggplot")
  expect_true(any(!is.na(out$significantThresholds$Pvalue)))
})

test_that("intSigRegGraph handles real rms predictions without thresholds", {
  m <- local_rms_interaction(slope = .05, noise = 1, seed = 2, n = 40)
  pred <- list(x = seq(-2, 2, length.out = 5), g = c("a", "b"))
  contrasts <- list(
    list(c(x = -2, x = 2), list(g = "a")),
    list(c(x = -2, x = 2), list(g = "b")))

  out <- intSigRegGraph(
    m,
    predList = pred,
    contrastList = contrasts,
    xvar = "x",
    varyvar = "g",
    xlim = c(-2, 2),
    ylim = c(-4, 5),
    xbreaks = -2:2,
    starts = 5)
  expect_s3_class(out$Graph, "ggplot")
  expect_true(all(is.na(out$significantThresholds$Pvalue)))
})

test_that("intSigRegGraph places labels at xmax when right-edge separation is larger", {
  m <- local_rms_interaction(slope = 1.5, intercept = .5, noise = .2)
  pred <- list(x = seq(-2, 2, length.out = 5), g = c("a", "b"))
  contrasts <- list(
    list(c(x = -2, x = 2), list(g = "a")),
    list(c(x = -2, x = 2), list(g = "b")))

  out <- intSigRegGraph(
    m,
    predList = pred,
    contrastList = contrasts,
    xvar = "x",
    varyvar = "g",
    xlim = c(-2, 2),
    ylim = c(-5, 8),
    xbreaks = -2:2,
    starts = 5)
  expect_true(all(out$simpleSlopes$x == 2))
  expect_true(grepl("hjust = TRUE", out$GraphCode, fixed = TRUE))
})

test_that("VAObject subsetting and display methods expose logMAR values", {
  va <- VAConverter(
    OS = c("20/20", "20/40"),
    OD = c("20/40", "20/20"),
    datatype = "snellen")

  # Subsetting returns the two-eye logMAR matrix used by print/show methods.
  expect_equal(
    va[],
    cbind(logMAROS = c(0, log10(2)), logMAROD = c(log10(2), 0)),
    tolerance = 1e-8)

  expect_output(print(va), "Chart values")
  expect_output(show(va), "logMAROS")
})

test_that("summary.VAObject handles equal weighting and VASummary display", {
  va <- VAConverter(
    OS = c("20/20", "20/40"),
    OD = c("20/40", "20/20"),
    datatype = "snellen")

  best_eye <- summary(va)
  expect_equal(best_eye@logMAR.combined, rep(log10(2) * .25, 2))

  expect_warning(
    s <- summary(va, weightbest = FALSE, w = c(.75, .25)),
    "Unequal weights")
  expect_s4_class(s, "VASummaryObject")
  expect_equal(s@logMAR.combined, c(log10(2) * .25, log10(2) * .75))
  expect_equal(s@snellen.combined, c("20/24", "20/34"))
  expect_output(show(s), "Summary Information")

  # Plotting writes to a temp device so the repository Rplots.pdf is untouched.
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit(grDevices::dev.off())
  expect_invisible(plot(s))
})

test_that("VAConverter covers chart override, decimal, logMAR, and zero cases", {
  chart <- c("20/80", "20/40", "20/20", "20/10")
  nletters <- c(5L, 5L, 5L, 5L)
  va <- VAConverter(
    OS = c("20/20 + 1", "LP", "20/40"),
    OD = c("20/40 - 1", "CF 10", "NLP"),
    chart.values = chart,
    chart.nletters = nletters,
    datatype = "snellen",
    zero = NA)

  # Custom chart metadata is stored and unsupported acuity strings become zero.
  expect_identical(va@chart.values, chart)
  expect_identical(va@chart.nletters, nletters)
  expect_true(is.na(va@logMAROS[2]))
  expect_true(is.na(va@logMAROD[3]))

  decimal <- VAConverter(c(1, .5), c(.5, 1), datatype = "decimal")
  expect_equal(decimal@logMAROS, c(0, log10(2)))

  logmar <- VAConverter(c(0, .3), c(.3, 0), datatype = "logMAR")
  expect_equal(logmar@logMAROD, c(.3, 0))

  expect_equal(JWileymisc:::logmar(c(0, .3), inverse = TRUE), c(20, 39.90525), tolerance = 1e-5)
})

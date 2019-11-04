context("visual acuity")

test_that("VAConverter works", {
  sampdat <- c("HM 12", "20/20 + 3", "20/50", "CF", "HM",
               "20/70 - 2", "LP", NA, "Prosthetic")
  tmp <- VAConverter(OS = sampdat, OD = rev(sampdat), datatype = "snellen")

  expect_is(tmp, "VAObject")
})


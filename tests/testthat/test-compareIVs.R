test_that("compareIVs works correctly for different numbers of DVs, IVs, Covariates, and Multivariate", {
  grid <- expand.grid(
    DV = 1:2,
    IV = 1:2,
    Covs = 1:2,
    Multivariate = 1:2)

  grid <- subset(grid, !(Multivariate == 2 & IV == 1))

  dv <- c("mpg", "disp")
  iv <- c("hp", "qsec")
  Covs <- c("am", "vs")
  Multivariate <- c(FALSE, TRUE)
  res <- vector("list", nrow(grid))

  for (i in 1:2) {
    foreach::registerDoSEQ() ## only needed to remove warning about sequential running
    res[[i]] <- compareIVs(
      dv = dv[1:grid$DV[i]],
      type = c("normal", "normal")[1:grid$DV[i]],
      iv = iv[1:grid$IV[1]],
      covariates = Covs[1:grid$Covs[i]],
      data = mtcars,
      multivariate = Multivariate[grid$Multivariate[i]])

    expect_type(res[[i]], "list")
    expect_length(res[[i]], grid$DV[i] + 1)
    lapply(res[[i]][-length(res[[i]])], function(x) {
      expect_length(x, grid$IV[i])
      NULL
    })
  }
})

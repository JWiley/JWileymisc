context("TukeyHSDgg")

test_that("TukeyHSDgg works with single level data", {

  mtcars$cyl <- factor(mtcars$cyl)

  expect_is(
    TukeyHSDgg("cyl", "mpg", mtcars),
    "ggplot")
  expect_warning(TukeyHSDgg("am", "mpg", mtcars))
})

test_that("TUkeyHSDgg works with multilevel data", {
  expect_warning(hsdp <- TukeyHSDgg("am", "mpg", mtcars, idvar = "cyl"))
  expect_is(
    hsdp,
    "ggplot")
})

context("gglikert")

test_that("gglikert works", {
  testdat <- data.table::data.table(
                           Var = 1:4,
                           Mean = c(1.5, 3, 2.2, 4.6),
                           Low = c("Happy", "Peaceful", "Excited", "Content"),
                           High = c("Sad", "Angry", "Hopeless", "Anxious"))

  expect_is(
    gglikert("Mean", "Var", "Low", "High", data = testdat, xlim = c(1, 5),
             title = "Example Plot of Average Affect Ratings"),
    "ggplot")
  expect_is(
    gglikert("Mean", "Var", "Low", "High",
             colour = "blue",
             data = testdat, xlim = c(1, 5),
             title = "Example Plot of Average Affect Ratings"),
    "ggplot")
  expect_is(
    gglikert("Mean", "Var", "Low", "High",
             colour = "Low",
             data = testdat, xlim = c(1, 5),
             title = "Example Plot of Average Affect Ratings"),
    "ggplot")

  testdat$Var <- as.character(testdat$Var)

  expect_message(
    gglikert("Mean", "Var", "Low", "High", data = testdat, xlim = c(1, 5),
             title = "Example Plot of Average Affect Ratings"))

})

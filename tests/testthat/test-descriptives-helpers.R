test_that(".styleaov works", {
  expect_equal(
    JWileymisc:::.styleaov(mtcars$mpg, mtcars$cyl, digits = 2L, pdigits = 3L),
    "F(1, 30) = 79.56, p < .001, Eta-squared = 0.73")
})

test_that(".style2sttest works", {
  expect_equal(
    JWileymisc:::.style2sttest(mtcars$mpg, mtcars$am, digits = 2L, pdigits = 3L),
    "t(df=30) = -4.11, p < .001, d = 1.48")
})

test_that(".stylepairedttest works", {
  expect_equal(
    JWileymisc:::.stylepairedttest(sleep$extra, sleep$group, sleep$ID, digits = 2L, pdigits = 3L),
    "t(df=9) = 4.06, p = .003, d = 1.28")
})

test_that(".stylepairedwilcox works", {
  if (getRversion() > "4.5.1") {
    res <- JWileymisc:::.stylepairedwilcox(sleep$extra, sleep$group, sleep$ID, digits = 2L, pdigits = 3L)
    expect_equal(
    res,
    "Wilcoxon Paired V = 54.00, p = .004")
  } else {
    expect_warning(expect_warning(
      res <- JWileymisc:::.stylepairedwilcox(sleep$extra, sleep$group, sleep$ID, digits = 2L, pdigits = 3L)))
    expect_equal(
    res,
    "Wilcoxon Paired V = 45.00, p = .009")
  }
})

test_that(".stylepairedmcnemar works", {
  exdata <- structure(list(
    ID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
           1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
    Time = c("base", "base", 
             "base", "base", "base", "base", "base", "base", "base", "base", 
             "post", "post", "post", "post", "post", "post", "post", "post", 
             "post", "post"),
    Rating = c("bad", "bad", "bad", "bad", "good", 
               "bad", "good", "good", "good", "bad", "bad", "bad", "bad", "good", 
               "bad", "bad", "bad", "good", "bad", "bad")),
    row.names = c(NA, -20L), class = c("data.frame"))

  res <- JWileymisc:::.stylepairedmcnemar(
    exdata$Rating, exdata$Time, exdata$ID, digits = 2L, pdigits = 3L)

  expect_equal(
    res,
    "McNemar's Chi-square = 0.25, df = 1, p = .617")
})

test_that(".stylekruskal works", {
  expect_equal(
    JWileymisc:::.stylekruskal(mtcars$mpg, mtcars$am, digits = 2L, pdigits = 3L),
    "KW chi-square = 9.79, df = 1, p = .002")

  expect_equal(
    JWileymisc:::.stylekruskal(mtcars$mpg, mtcars$cyl, digits = 2L, pdigits = 3L),
    "KW chi-square = 25.75, df = 2, p < .001")
})

test_that(".stylechisq works", {
  expect_warning(res <- JWileymisc:::.stylechisq(mtcars$cyl, mtcars$am, digits = 2L, pdigits = 3L))
  expect_equal(
    res,
    "Chi-square = 8.74, df = 2, p = .013, Cramer's V = 0.52")
})

test_that(".stylemsd works", {
  expect_equal(
    JWileymisc:::.stylemsd("Miles per Gallon", mtcars$mpg)$Res,
    "20.09 (6.03)")
})

test_that(".stylemdniqr works", {
  expect_equal(
    JWileymisc:::.stylemdniqr("Miles per Gallon", mtcars$mpg)$Res,
    "19.20 (7.38)")
})

test_that(".stylefreq works", {
  expect_equal(
    JWileymisc:::.stylefreq("Transmission", mtcars$am)$Res,
    c("", "19 (59.4%)", "13 (40.6%)"))
})

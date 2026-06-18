# Several internal functions to style inference tests

Several internal functions to style inference tests

## Usage

``` r
.styleaov(dv, g, digits = 2L, pdigits = 3L)

.style2sttest(dv, g, digits = 2, pdigits = 3)

.stylepairedttest(dv, g, ID, digits = 2, pdigits = 3)

.stylepairedwilcox(dv, g, ID, digits = 2, pdigits = 3, ...)

.stylepairedmcnemar(dv, g, ID, digits = 2, pdigits = 3)

.stylekruskal(dv, g, digits = 2, pdigits = 3)

.stylechisq(dv, g, digits = 2, pdigits = 3, simChisq = FALSE, sims = 10000)

.stylemsd(n, x, digits = 2, includeLabel = FALSE)

.stylemdniqr(n, x, digits = 2, includeLabel = FALSE)

.stylefreq(n, x)
```

## Arguments

- dv:

  An outcome variable

- g:

  A grouping/predictor variable

- digits:

  An integer indicating the number of significant digits to use.
  Defaults to `2`.

- pdigits:

  An integer indicating the number of digits for p values. Defaults to
  `3`.

- ...:

  Additional arguments passed to
  [`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html).

- simChisq:

  A logical value, whether or not to simulate chi-square values. Only
  applies to some functions. Defaults to `FALSE`.

- sims:

  An integer indicating the number of simulations to conduct. Only
  applies to some functions. Defaults to `10000`, but this is arbitrary
  and should be chosen.

## Value

A character string of the formatted results.

## Examples

``` r

JWileymisc:::.styleaov(mtcars$mpg, mtcars$cyl)
#> [1] "F(1, 30) = 79.56, p < .001, Eta-squared = 0.73"

JWileymisc:::.style2sttest(mtcars$mpg, mtcars$am)
#> [1] "t(df=30) = -4.11, p < .001, d = 1.48"

JWileymisc:::.stylepairedttest(sleep$extra, sleep$group, sleep$ID)
#> [1] "t(df=9) = 4.06, p = .003, d = 1.28"

JWileymisc:::.stylepairedwilcox(sleep$extra, sleep$group, sleep$ID)
#> [1] "Wilcoxon Paired V = 54.00, p = .004"

## example data
set.seed(1234)
exdata <- data.frame(
  ID = rep(1:10, 2),
  Time = rep(c("base", "post"), each = 10),
  Rating = sample(c("good", "bad"), size = 20, replace = TRUE))
JWileymisc:::.stylepairedmcnemar(exdata$Rating, exdata$Time, exdata$ID)
#> [1] "McNemar's Chi-square = 0.25, df = 1, p = .617"
rm(exdata) ## cleanup

JWileymisc:::.stylekruskal(mtcars$mpg, mtcars$am)
#> [1] "KW chi-square = 9.79, df = 1, p = .002"
JWileymisc:::.stylekruskal(mtcars$mpg, mtcars$cyl)
#> [1] "KW chi-square = 25.75, df = 2, p < .001"

JWileymisc:::.stylechisq(mtcars$cyl, mtcars$am)
#> Warning: Chi-squared approximation may be incorrect
#> [1] "Chi-square = 8.74, df = 2, p = .013, Cramer's V = 0.52"

JWileymisc:::.stylemsd("Miles per Gallon", mtcars$mpg)
#>            Variable          Res
#>              <char>       <char>
#> 1: Miles per Gallon 20.09 (6.03)
JWileymisc:::.stylemsd("Miles per Gallon", mtcars$mpg, includeLabel = TRUE)
#>                    Variable          Res
#>                      <char>       <char>
#> 1: Miles per Gallon, M (SD) 20.09 (6.03)

JWileymisc:::.stylemdniqr("Miles per Gallon", mtcars$mpg)
#>            Variable          Res
#>              <char>       <char>
#> 1: Miles per Gallon 19.20 (7.38)
JWileymisc:::.stylemdniqr("Miles per Gallon", mtcars$mpg, includeLabel = TRUE)
#>                       Variable          Res
#>                         <char>       <char>
#> 1: Miles per Gallon, Mdn (IQR) 19.20 (7.38)

JWileymisc:::.stylefreq("Transmission", mtcars$am)
#>        Variable        Res
#>          <char>     <char>
#> 1: Transmission           
#> 2:            0 19 (59.4%)
#> 3:            1 13 (40.6%)
JWileymisc:::.stylefreq("Transmission", mtcars$am)
#>        Variable        Res
#>          <char>     <char>
#> 1: Transmission           
#> 2:            0 19 (59.4%)
#> 3:            1 13 (40.6%)
```

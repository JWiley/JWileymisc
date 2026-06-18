# APAStyler method for model tests from a vglm multinomial model

APAStyler method for model tests from a vglm multinomial model

## Usage

``` r
# S3 method for class 'modelTest.vglm'
APAStyler(
  object,
  format = list(FixedEffects = c("%s%s [%s, %s]"), EffectSizes =
    c("Chi-square (df=%s) = %s, %s")),
  digits = 2,
  pcontrol = list(digits = 3, stars = TRUE, includeP = FALSE, includeSign = FALSE,
    dropLeadingZero = TRUE),
  OR = TRUE,
  ...
)
```

## Arguments

- object:

  A `modelTest.vglm` class object, results from running
  [`modelTest()`](https://joshuawiley.com/JWileymisc/reference/modelTest.md)
  function on a class `vglm` object with a multinomial family

- format:

  A list giving the formatting style to be used for the fixed effects
  and effect sizes.

- digits:

  A numeric value indicating the number of digits to print. This is
  still in early implementation stages and currently does not change all
  parts of the output (which default to 2 decimals per APA style).

- pcontrol:

  A list controlling how p values are formatted.

- OR:

  a logical value whether to report odds ratios and 95 percent
  confidence intervals, if `TRUE`, or regression coefficients on the
  logit scale with standard errors, if `FALSE`.

- ...:

  Additional arguments.

## Value

Styled results.

## Examples

``` r
mtcars$cyl <- factor(mtcars$cyl)
m <- VGAM::vglm(cyl ~ qsec,
  family = VGAM::multinomial(), data = mtcars)
mt <- modelTest(m)

APAStyler(mt)
#> Key: <Term>
#>      Term  Names           2 vs. 1             3 vs. 1           3 vs. 2
#>    <char> <char>            <char>              <char>            <char>
#> 1:   qsec   qsec 0.56 [0.26, 1.23] 0.28** [0.11, 0.69] 0.50 [0.23, 1.11]
#>                                   Test
#>                                 <char>
#> 1: Chi-square (df=2) = 14.21, p < .001

APAStyler(mt, OR = FALSE)
#> Key: <Term>
#>      Term  Names              2 vs. 1                3 vs. 1
#>    <char> <char>               <char>                 <char>
#> 1:   qsec   qsec -0.58 [-1.36,  0.21] -1.27** [-2.16, -0.38]
#>                 3 vs. 2                                Test
#>                  <char>                              <char>
#> 1: -0.69 [-1.48,  0.10] Chi-square (df=2) = 14.21, p < .001

## clean up
rm(m, mt, mtcars)

if (FALSE) { # \dontrun{
mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
m <- VGAM::vglm(cyl ~ qsec,
  family = VGAM::multinomial(), data = mtcars)
APAStyler(modelTest(m))

m <- VGAM::vglm(cyl ~ scale(qsec),
  family = VGAM::multinomial(), data = mtcars)
APAStyler(modelTest(m))

m2 <- VGAM::vglm(cyl ~ factor(vs) * scale(qsec),
  family = VGAM::multinomial(), data = mtcars)
APAStyler(modelTest(m2))

m <- VGAM::vglm(Species ~ Sepal.Length,
  family = VGAM::multinomial(), data = iris)
APAStyler(modelTest(m))

set.seed(1234)
sampdata <- data.frame(
  Outcome = factor(sample(letters[1:3], 20 * 9, TRUE)),
  C1 = rnorm(20 * 9),
  D3 = sample(paste0("L", 1:3), 20 * 9, TRUE))

m <- VGAM::vglm(Outcome ~ factor(D3),
  family = VGAM::multinomial(), data = sampdata)
APAStyler(modelTest(m))

m <- VGAM::vglm(Outcome ~ factor(D3) + C1,
  family = VGAM::multinomial(), data = sampdata)
APAStyler(modelTest(m))
} # }
```

# APAStyler method for model tests from a linear model

APAStyler method for model tests from a linear model

## Usage

``` r
# S3 method for class 'modelTest.lm'
APAStyler(
  object,
  format = list(FixedEffects = c("%s%s [%s, %s]"), EffectSizes = c("f2 = %s, %s")),
  digits = 2,
  pcontrol = list(digits = 3, stars = TRUE, includeP = FALSE, includeSign = FALSE,
    dropLeadingZero = TRUE),
  ...
)
```

## Arguments

- object:

  A `modelTest.lm` class object, results from running
  [`modelTest()`](https://joshuawiley.com/JWileymisc/reference/modelTest.md)
  function on a class `lm` object.

- format:

  A list giving the formatting style to be used for the fixed effecvts
  and effect sizes.

- digits:

  A numeric value indicating the number of digits to print. This is
  still in early implementation stages and currently does not change all
  parts of the output (which default to 2 decimals per APA style).

- pcontrol:

  A list controlling how p values are formatted.

- ...:

  Additional arguments.

## Value

Styled results.

## Examples

``` r
m1 <- lm(mpg ~ qsec * hp, data = mtcars)
APAStyler(modelTest(m1))
#>                 Term                      Est          Type
#>               <char>                   <char>        <char>
#>  1:      (Intercept)     8.52 [-17.15, 34.20] Fixed Effects
#>  2:             qsec    1.48* [  0.08,  2.87] Fixed Effects
#>  3:               hp   0.24** [  0.09,  0.39] Fixed Effects
#>  4:          qsec:hp -0.02*** [ -0.03, -0.01] Fixed Effects
#>  5: N (Observations)                       32 Overall Model
#>  6:        logLik DF                        5 Overall Model
#>  7:           logLik                   -77.75 Overall Model
#>  8:              AIC                   165.50 Overall Model
#>  9:              BIC                   172.83 Overall Model
#> 10:               F2                     3.66 Overall Model
#> 11:               R2                     0.79 Overall Model
#> 12:           Adj R2                     0.76 Overall Model
#> 13:             qsec      f2 = 0.17, p = .039  Effect Sizes
#> 14:               hp      f2 = 0.37, p = .003  Effect Sizes
#> 15:          qsec:hp      f2 = 0.69, p < .001  Effect Sizes

APAStyler(modelTest(m1),
format = list(
  FixedEffects = "%s, %s\n(%s, %s)",
  EffectSizes = "Cohen's f2 = %s (%s)"),
pcontrol = list(digits = 4,
  stars = FALSE, includeP = TRUE,
  includeSign = TRUE,
  dropLeadingZero = TRUE))
#>                 Term                               Est          Type
#>               <char>                            <char>        <char>
#>  1:      (Intercept)  8.52, p = .5020\n(-17.15, 34.20) Fixed Effects
#>  2:             qsec  1.48, p = .0386\n(  0.08,  2.87) Fixed Effects
#>  3:               hp  0.24, p = .0034\n(  0.09,  0.39) Fixed Effects
#>  4:          qsec:hp -0.02, p = .0001\n( -0.03, -0.01) Fixed Effects
#>  5: N (Observations)                                32 Overall Model
#>  6:        logLik DF                                 5 Overall Model
#>  7:           logLik                            -77.75 Overall Model
#>  8:              AIC                            165.50 Overall Model
#>  9:              BIC                            172.83 Overall Model
#> 10:               F2                              3.66 Overall Model
#> 11:               R2                              0.79 Overall Model
#> 12:           Adj R2                              0.76 Overall Model
#> 13:             qsec     Cohen's f2 = 0.17 (p = .0386)  Effect Sizes
#> 14:               hp     Cohen's f2 = 0.37 (p = .0034)  Effect Sizes
#> 15:          qsec:hp     Cohen's f2 = 0.69 (p = .0001)  Effect Sizes

## clean up
rm(m1)
```

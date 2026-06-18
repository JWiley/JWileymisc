# Return Indices of Model Performance

Generic function. Generally returns things like fit indices, absolute
error metrics, tests of overall model significance.

## Usage

``` r
modelPerformance(object, ...)

as.modelPerformance(x)

is.modelPerformance(x)

# S3 method for class 'lm'
modelPerformance(object, ...)
```

## Arguments

- object:

  A fitted model object. The class of the model determines which
  specific method is called.

- ...:

  Additional arguments passed to specific methods.

- x:

  A object (e.g., list or a modelPerformance object) to test or attempt
  coercing to a modelPerformance object.

## Value

A `data.table` with results.

A list with a `data.table` with the following elements:

- Model:

  A character string indicating the model type, here lm

- N_Obs:

  The number of observations

- AIC:

  Akaike Information Criterion

- BIC:

  Bayesian Information Criterion

- LL:

  log likelihood

- LLDF:

  log likelihood degrees of freedom

- Sigma:

  Residual variability

- R2:

  in sample variance explained

- F2:

  Cohen's F2 effect size R2 / (1 - R2)

- AdjR2:

  adjusted variance explained

- F:

  F value for overall model significance test

- FNumDF:

  numerator degrees of freedom for F test

- FDenDF:

  denominator degrees of freedom for F test

- P:

  p-value for overall model F test

## Details

For `lm` class objects, return number of observations, AIC, BIC, log
likelihood, R2, overall model F test, and p-value.

## Examples

``` r
modelPerformance(lm(mpg ~ qsec * hp, data = mtcars))
#> $Performance
#>     Model N_Obs      AIC      BIC        LL  LLDF    Sigma        R2       F2
#>    <char> <num>    <num>    <num>     <num> <num>    <num>     <num>    <num>
#> 1:     lm    32 165.4972 172.8259 -77.74861     5 2.937243 0.7854734 3.661427
#>        AdjR2        F FNumDF FDenDF            P
#>        <num>    <num>  <num>  <num>        <num>
#> 1: 0.7624884 34.17332      3     28 1.694676e-09
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   

modelPerformance(lm(mpg ~ hp, data = mtcars))
#> $Performance
#>     Model N_Obs      AIC      BIC        LL  LLDF    Sigma        R2       F2
#>    <char> <num>    <num>    <num>     <num> <num>    <num>     <num>    <num>
#> 1:     lm    32 181.2386 185.6358 -87.61931     3 3.862962 0.6024373 1.515327
#>        AdjR2       F FNumDF FDenDF            P
#>        <num>   <num>  <num>  <num>        <num>
#> 1: 0.5891853 45.4598      1     30 1.787835e-07
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   

if (FALSE) { # \dontrun{
modelPerformance(lm(mpg ~ 0 + hp, data = mtcars))
modelPerformance(lm(mpg ~ 1, data = mtcars))
modelPerformance(lm(mpg ~ 0, data = mtcars))
} # }
```

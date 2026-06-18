# Compare Two Models

Generic function.

## Usage

``` r
modelCompare(model1, model2, ...)

as.modelCompare(x)

is.modelCompare(x)

# S3 method for class 'lm'
modelCompare(model1, model2, ...)
```

## Arguments

- model1:

  A fitted model object.

- model2:

  A fitted model object to compare to `model1`

- ...:

  Additional arguments passed to specific methods.

- x:

  An object (e.g., list or a modelCompare object) to test or attempt
  coercing to a modelCompare object.

## Value

Depends on the method dispatch.

## Examples

``` r
m1 <- lm(mpg ~ qsec * hp, data = mtcars)

m2 <- lm(mpg ~ am, data = mtcars)

modelCompare(m1, m2)
#> $Comparison
#>         Model N_Obs       AIC       BIC        LL  LLDF     Sigma        R2
#>        <char> <num>     <num>     <num>     <num> <num>     <num>     <num>
#> 1:    Reduced    32 196.48438 200.88159 -95.24219     3  4.902029 0.3597989
#> 2:       Full    32 165.49722 172.82590 -77.74861     5  2.937243 0.7854734
#> 3: Difference     0 -30.98716 -28.05569  17.49358     2 -1.964786 0.4256745
#>           F2     AdjR2        F FNumDF FDenDF            P
#>        <num>     <num>    <num>  <num>  <num>        <num>
#> 1: 0.5620093 0.3384589 16.86028      1     30 2.850207e-04
#> 2: 3.6614271 0.7624884 34.17332      3     28 1.694676e-09
#> 3: 1.9842506 0.4240295 27.77951      2     28 2.250640e-07
#> 
#> attr(,"class")
#> [1] "modelCompare.lm" "modelCompare"   

## cleanup
rm(m1, m2)

if (FALSE) { # \dontrun{
m3 <- lm(mpg ~ 1, data = mtcars)
m4 <- lm(mpg ~ 0, data = mtcars)
modelCompare(m3, m4)

## cleanup
rm(m3, m4)
} # }
```

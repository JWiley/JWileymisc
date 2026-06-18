# Model Performance and Tests

To start, load the package.

``` r

library(JWileymisc)
#> Registered S3 method overwritten by 'lme4':
#>   method           from
#>   na.action.merMod car
```

## Model Performance

[`modelPerformance()`](https://joshuawiley.com/JWileymisc/reference/modelPerformance.md)
is a generic function that can be used to calculate performance metrics
for a model. `JWileymisc` implements methods for `lm` class objects. The
output is a named list, with a data table containing results. For linear
models, current performance metrics include:

- AIC (Akaike Information Criterion)
- BIC (Bayesian Information Criterion)
- LL (Log Likelihood)
- LLDF (degrees of freedom for log likelihood)
- Sigma (residual standard deviation)
- R2 (\\R^2\\ variance accounted for in the sample)
- F2 (Cohen’s \\f^2\\ effect size, calculated as \\\frac{R^{2}}{1 -
  R^{2}}\\)
- AdjR2 (Sample size adjusted \\R^2\\, a better estimate of population
  variance accounted for)
- F (model F test)
- FNumDF (numerator degrees of freedom for model F test)
- FDenDF (denominator degrees of freedom for model F test)
- P (p value for model F test)

``` r


mtcars$cyl <- factor(mtcars$cyl)
m <- stats::lm(mpg ~ hp + cyl, data = mtcars)

mp <- modelPerformance(m)
print(mp)
#> $Performance
#>     Model N_Obs      AIC      BIC        LL  LLDF    Sigma        R2       F2
#>    <char> <num>    <num>    <num>     <num> <num>    <num>     <num>    <num>
#> 1:     lm    32 169.8964 177.2251 -79.94822     5 3.146243 0.7538578 3.062692
#>        AdjR2        F FNumDF FDenDF           P
#>        <num>    <num>  <num>  <num>       <num>
#> 1: 0.7274854 28.58513      3     28 1.13969e-08
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"
```

If only certain metrics are desired, these can be found by extracting
the “Performance” list element and then the correct column from the data
table.

``` r


## Cohen's f^2 effect size
mp$Performance[, F2]
#> [1] 3.062692
```

Another function,
[`modelTest()`](https://joshuawiley.com/JWileymisc/reference/modelTest.md)
is a generic providing a comprehensive series of tests for a model.
Currently methods are implemented for both `lm` class models and `vglm`
class models from the `VGAM` package with a multinomial family.

[`modelTest()`](https://joshuawiley.com/JWileymisc/reference/modelTest.md)

``` r


mt <- modelTest(m)
print(mt)
#> $FixedEffects
#>           Term         Est           LL           UL         Pval
#>         <char>       <num>        <num>        <num>        <num>
#> 1: (Intercept) 28.65011816  25.39768395 31.902552374 5.921199e-17
#> 2:          hp -0.02403883  -0.05560048  0.007522814 1.299540e-01
#> 3:        cyl6 -5.96765508  -9.32556307 -2.609747083 1.092089e-03
#> 4:        cyl8 -8.52085075 -13.28559928 -3.756102224 1.028617e-03
#> 
#> $RandomEffects
#> [1] NA
#> 
#> $EffectSizes
#>      Term N_Obs         AIC        BIC       LL  LLDF       Sigma         R2
#>    <char> <num>       <num>      <num>    <num> <num>       <num>      <num>
#> 1:     hp     0  -0.6675031  0.7982328 1.333752     1 -0.07685536 0.02139775
#> 2:    cyl     0 -11.3421811 -8.4107093 7.671091     2 -0.71671885 0.15142046
#>            F2     AdjR2        F FNumDF FDenDF           P   Type
#>         <num>     <num>    <num>  <num>  <num>       <num> <char>
#> 1: 0.08693246 0.0134764 2.434109      1     28 0.129954045  Fixed
#> 2: 0.61517476 0.1383002 8.612447      2     28 0.001215981  Fixed
#> 
#> $OverallModel
#> $Performance
#>     Model N_Obs      AIC      BIC        LL  LLDF    Sigma        R2       F2
#>    <char> <num>    <num>    <num>     <num> <num>    <num>     <num>    <num>
#> 1:     lm    32 169.8964 177.2251 -79.94822     5 3.146243 0.7538578 3.062692
#>        AdjR2        F FNumDF FDenDF           P
#>        <num>    <num>  <num>  <num>       <num>
#> 1: 0.7274854 28.58513      3     28 1.13969e-08
#> 
#> attr(,"class")
#> [1] "modelPerformance.lm" "modelPerformance"   
#> 
#> attr(,"class")
#> [1] "modelTest.lm" "modelTest"
```

``` r


APAStyler(mt)
#>                 Term                      Est          Type
#>               <char>                   <char>        <char>
#>  1:      (Intercept) 28.65*** [ 25.40, 31.90] Fixed Effects
#>  2:               hp    -0.02 [ -0.06,  0.01] Fixed Effects
#>  3:             cyl6  -5.97** [ -9.33, -2.61] Fixed Effects
#>  4:             cyl8  -8.52** [-13.29, -3.76] Fixed Effects
#>  5: N (Observations)                       32 Overall Model
#>  6:        logLik DF                        5 Overall Model
#>  7:           logLik                   -79.95 Overall Model
#>  8:              AIC                   169.90 Overall Model
#>  9:              BIC                   177.23 Overall Model
#> 10:               F2                     3.06 Overall Model
#> 11:               R2                     0.75 Overall Model
#> 12:           Adj R2                     0.73 Overall Model
#> 13:               hp      f2 = 0.09, p = .130  Effect Sizes
#> 14:              cyl      f2 = 0.62, p = .001  Effect Sizes
```

The model tests can also be used with interactions.

``` r


m2 <- stats::lm(mpg ~ hp * cyl, data = mtcars)

APAStyler(modelTest(m2))
#>                 Term                       Est          Type
#>               <char>                    <char>        <char>
#>  1:      (Intercept)  35.98*** [ 27.99, 43.98] Fixed Effects
#>  2:               hp    -0.11* [ -0.21, -0.02] Fixed Effects
#>  3:             cyl6   -15.31* [-30.59, -0.03] Fixed Effects
#>  4:             cyl8  -17.90** [-28.71, -7.09] Fixed Effects
#>  5:          hp:cyl6      0.11 [ -0.04,  0.25] Fixed Effects
#>  6:          hp:cyl8      0.10 [  0.00,  0.20] Fixed Effects
#>  7: N (Observations)                        32 Overall Model
#>  8:        logLik DF                         7 Overall Model
#>  9:           logLik                    -77.54 Overall Model
#> 10:              AIC                    169.08 Overall Model
#> 11:              BIC                    179.34 Overall Model
#> 12:               F2                      3.72 Overall Model
#> 13:               R2                      0.79 Overall Model
#> 14:           Adj R2                      0.75 Overall Model
#> 15:               hp       f2 = 0.23, p = .021  Effect Sizes
#> 16:              cyl       f2 = 0.47, p = .007  Effect Sizes
#> 17:           hp:cyl       f2 = 0.16, p = .142  Effect Sizes
```

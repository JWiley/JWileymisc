# Compares the effects of various independent variables on dependent variables

Utility to estimate the unadjusted, covariate adjusted, and multivariate
adjusted unique contributions of one or more IVs on one or more DVs

## Usage

``` r
compareIVs(
  dv,
  type,
  iv,
  covariates = character(),
  data,
  multivariate = FALSE,
  ...
)
```

## Arguments

- dv:

  A character string or vector of the depentent variable(s)

- type:

  A character string or vector indicating the type of dependent
  variable(s)

- iv:

  A character string or vector giving the IV(s)

- covariates:

  A character string or vector giving the covariate(s)

- data:

  The data to be used for analysis

- multivariate:

  A logical value whether to have models with all IVs simultaneously.

- ...:

  Additional arguments passed on to the internal function, `.runIt`.

## Value

A list with all the model results.

## Examples

``` r
test1 <- compareIVs(
  dv = c("mpg", "disp"),
  type = c("normal", "normal"),
  iv = c("hp", "qsec"),
  covariates = "am",
  data = mtcars, multivariate = TRUE)
#> Multivariate uses complete cases for all IVs and covariates
#> Warning: executing %dopar% sequentially: no parallel backend registered
#> Multivariate uses complete cases for all IVs and covariates
test1$OverallSummary
#>      dv   iv        Type           R2           D
#> 1   mpg   hp  Unadjusted  0.589185253 0.602437341
#> 2   mpg   hp    Adjusted  0.428543631 0.422235690
#> 3   mpg   hp MultiUnique  0.100202250 0.101303887
#> 4   mpg qsec  Unadjusted  0.147806198 0.175296320
#> 5   mpg qsec    Adjusted  0.326783610 0.327040832
#> 6   mpg qsec MultiUnique -0.001557770 0.006109029
#> 7  disp   hp  Unadjusted  0.613119655 0.625599666
#> 8  disp   hp    Adjusted  0.452667898 0.445145204
#> 9  disp   hp MultiUnique  0.108406045 0.108532739
#> 10 disp qsec  Unadjusted  0.161030314 0.188093852
#> 11 disp qsec    Adjusted  0.342986638 0.342540154
#> 12 disp qsec MultiUnique -0.001275215 0.005927689
rm(test1)
```

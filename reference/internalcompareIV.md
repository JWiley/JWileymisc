# Compares the effects of various independent variables

This is an internal function designed to run many models to compare the
unique predictive effect of different IVs with and without covariates on
an outcome.

## Usage

``` r
internalcompareIV(
  dv,
  type = c("normal", "binary", "count"),
  iv,
  covariates = character(),
  data,
  multivariate = FALSE,
  ...
)
```

## Arguments

- dv:

  A character string of the depentent variable

- type:

  A character string indicating the type of dependent variable

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
test1 <- JWileymisc:::internalcompareIV(
  dv = "mpg", type = "normal",
  iv = "hp",
  covariates = "am",
  data = mtcars, multivariate = FALSE)
test1$Summary
#> NULL
rm(test1)
```

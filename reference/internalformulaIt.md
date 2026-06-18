# Internal function to create a formula

This function is not intended to be called by users. It creates a
formula style character string from its argument. But note that it does
not actually create a formula class object. If you do not want an
argument, use the empty string.

## Usage

``` r
internalformulaIt(dv, iv, covariates)
```

## Arguments

- dv:

  A character string of the dependent variable.

- iv:

  A character string or vector of the independent variables

- covariates:

  A character string or vector of the dependent variables

## Value

A character string

## Examples

``` r
JWileymisc:::internalformulaIt("mpg", "hp", "am")
#> [1] "mpg ~ hp + am"
JWileymisc:::internalformulaIt("mpg", "hp", "")
#> [1] "mpg ~ hp"
JWileymisc:::internalformulaIt("mpg", "", "am")
#> [1] "mpg ~ am"
```

# Modified lm() to use a specified design matrix

This function is a minor modification of the lm() function to allow the
use of a pre-specified design matrix. It is not intended for public use
but only to support `modelTest.lm`.

## Usage

``` r
lm2(
  formula,
  data,
  subset,
  weights,
  na.action,
  model = TRUE,
  x = FALSE,
  y = FALSE,
  qr = TRUE,
  singular.ok = TRUE,
  contrasts = NULL,
  offset,
  designMatrix,
  yObserved,
  ...
)
```

## Arguments

- formula:

  An object of class "formula" although it is only minimally used

- data:

  the dataset

- subset:

  subset

- weights:

  any weights

- na.action:

  Defaults to `na.omit`

- model:

  defaults to `TRUE`

- x:

  defaults to `FALSE`

- y:

  defaults to `FALSE`

- qr:

  defaults to `TRUE`

- singular.ok:

  defaults to `TRUE`

- contrasts:

  defaults to `NULL`

- offset:

  missing by default

- designMatrix:

  a model matrix / design matrix (all numeric, pre coded if applicable
  for discrete variables)

- yObserved:

  the observed y values

- ...:

  additional arguments

## Value

an lm class object

## See also

`lm`

## Examples

``` r
mtcars$cyl <- factor(mtcars$cyl)
m <- lm(mpg ~ hp * cyl, data = mtcars)

x <- model.matrix(m)
y <- mtcars$mpg
m2 <- JWileymisc:::lm2(mpg ~ 1 + cyl + hp:cyl, data = mtcars,
  designMatrix = x[, -2, drop = FALSE],
  yObserved = y)

anova(m, m2)
#> Analysis of Variance Table
#> 
#> Model 1: mpg ~ hp * cyl
#> Model 2: mpg ~ 1 + cyl + hp:cyl
#>   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
#> 1     26 238.47                              
#> 2     27 294.20 -1   -55.739 6.0773 0.02061 *
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

rm(m, m2, x, y)
```

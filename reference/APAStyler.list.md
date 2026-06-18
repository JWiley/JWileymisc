# APAStyler method for lists

This assumes that all the objects in a list have the same class and that
an `APAStyler` method exists for that class.

## Usage

``` r
# S3 method for class 'list'
APAStyler(object, ...)
```

## Arguments

- object:

  A list in this case, where each element is another known class.

- ...:

  Additional arguments.

## Value

Styled results.

## Examples

``` r
if (FALSE) { # \dontrun{
m1 <- lm(mpg ~ qsec * hp, data = mtcars)
m2 <- lm(mpg ~ qsec + hp, data = mtcars)
m3 <- lm(mpg ~ am + vs, data = mtcars)
mt1 <- modelTest(m1)
mt2 <- modelTest(m2)
mt3 <- modelTest(m3)

## styling regression models
APAStyler(list(m1, m2))

## modelTest objects get merged
APAStyler(list(mt1, mt2))

## the models can be named by passing a named list
## including "special" characters using backticks, like spaces
APAStyler(list(Full = mt1, Reduced = mt2))
APAStyler(list(Full = mt1, Reduced = mt2, `Alternate Model` = mt3))

## you can customize the way output is presented
APAStyler(list(mt1, mt2), format = list(
  FixedEffects = "%s, %s\n(%s, %s)",
  EffectSizes = "Cohen's f2 = %s (%s)"))

## clean up
rm(m1, m2, m3, mt1, mt2, mt3)
} # }
```

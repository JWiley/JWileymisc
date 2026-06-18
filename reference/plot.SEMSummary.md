# Plots SEMSummary object

Plots SEMSummary object

## Usage

``` r
# S3 method for class 'SEMSummary'
plot(x, y, ...)
```

## Arguments

- x:

  An object of class SEMSummary.

- y:

  Ignored

- ...:

  Additional arguments passed on to the real workhorse, `corplot`.

## See also

[`corplot`](https://joshuawiley.com/JWileymisc/reference/corplot.md),
[`SEMSummary`](https://joshuawiley.com/JWileymisc/reference/SEMSummary.md)

## Examples

``` r
# default plot
plot(SEMSummary(~ ., data = mtcars))


# same as default
plot(SEMSummary(~ ., data = mtcars), type = "coverage")


# shows p values
plot(SEMSummary(~ ., data = mtcars), type = "p")


# shows correlations
plot(SEMSummary(~ ., data = mtcars), type = "cor")
```

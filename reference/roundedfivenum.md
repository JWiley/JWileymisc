# Calculate a rounded five number summary

Numbers are the minimum, 25th percentile, median, 75th percentile, and
maximum, of the non missing data. Values returned are either the
significant digits or rounded values, whichever ends up resulting in the
fewest total digits.

## Usage

``` r
roundedfivenum(x, round = 2, sig = 3)
```

## Arguments

- x:

  The data to have the summary calculated on

- round:

  The number of digits to try rounding

- sig:

  The number of significant digits to try

## Value

The rounded or significant digit five number summary

## Examples

``` r
JWileymisc:::roundedfivenum(rnorm(1000))
#> [1] -3.12 -0.62  0.01  0.67  3.17
JWileymisc:::roundedfivenum(mtcars$hp)
#> [1]  52.0  96.5 123.0 180.0 335.0
```

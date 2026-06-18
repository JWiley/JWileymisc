# Calculate a Circular Mean

Function to calculate circular mean

## Usage

``` r
meanCircular(x, max, na.rm = TRUE)
```

## Arguments

- x:

  Numeric or integer values

- max:

  The theoretical maximum (e.g., if degrees, 360)

- na.rm:

  A logical value indicating whether to remove missing values. Defaults
  to `TRUE`.

## Value

A numeric value with the circular mean.

## Examples

``` r
meanCircular(c(22:23, 1:2), max = 24)
#> [1] 0
meanCircular(c(12, 24), max = 24)
#> [1] 18
meanCircular(c(6, 7, 23), max = 24)
#> [1] 4.5
meanCircular(c(6, 7, 21), max = 24)
#> [1] 4.693219
meanCircular(c(6, 21), max = 24)
#> [1] 1.5
meanCircular(c(6, 23), max = 24)
#> [1] 2.5
meanCircular(c(.91, .96, .05, .16), max = 1)
#> [1] 0.01771571
meanCircular(c(6, 7, 8, 9), max = 24)
#> [1] 7.5
meanCircular(1:3, max = 24)
#> [1] 2
meanCircular(21:23, max = 24)
#> [1] 22
meanCircular(c(16, 17, 18, 19), max = 24)
#> [1] 17.5
meanCircular(c(355, 5, 15), max = 360)
#> [1] 5
```

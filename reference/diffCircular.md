# Calculate the Circular Difference

Calculate the Circular Difference

## Usage

``` r
diffCircular(x, y, max)
```

## Arguments

- x:

  Numeric or integer values

- y:

  Numeric or integer values

- max:

  the theoretical maximum (e.g., if degrees, 360; if hours, 24; etc.).

## Value

A value with the circular difference. This will always be positive if
defined.

## Examples

``` r
diffCircular(330, 30, max = 360)
#> [1] 60
diffCircular(22, 1, max = 24)
#> [1] 3
diffCircular(c(22, 23, 21, 22), c(1, 1, 23, 14), max = 24)
#> [1] 3 2 2 8
```

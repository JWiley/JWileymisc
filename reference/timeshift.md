# Shift a time variable to have a new center (zero point)

Given a vector, shift the values to have a new center, but keeping the
same minimum and maximum. Designed to work with time values where the
minimum indicates the same time as the maximum (e.g., 24:00:00 is the
same as 00:00:00).

## Usage

``` r
timeshift(x, center = 0, min = 0, max = 1, inverse = FALSE)
```

## Arguments

- x:

  the time scores to shift

- center:

  A value (between the minimum and maximum) to center the time scores.
  Defaults to 0, which has no effect.

- min:

  The theoretical minimum of the time scores. Defaults to 0.

- max:

  the theoretical maximum of the time scores. Defaults to 1.

- inverse:

  A logical value, whether to ‘unshift’ the time scores. Defaults to
  `FALSE`.

## Value

A vector of shifted time scores, recentered as specified.

## Examples

``` r
## example showing centering at 11am (i.e., 11am becomes new 0)
plot((1:24)/24, timeshift((1:24)/24, 11/24))


## example showing the inverse, note that 24/24 becomes 0
plot((1:24)/24, timeshift(timeshift((1:24)/24, 11/24), 11/24, inverse = TRUE))
```

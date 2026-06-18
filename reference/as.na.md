# Coerces vectors to missing

Given a vector, convert it to missing (NA) values, where the class of
the missing matches the input class. Currently supports character,
logical, integer, factor, numeric, times (from chron), Date, POSIXct,
POSIXlt, and zoo (from zoo), and haven labelled from haven.

## Usage

``` r
as.na(x)
```

## Arguments

- x:

  A vector to convert to missing (NA)

## Value

a vector the same length as the input with missing values of the same
class

## Examples

``` r
str(as.na(1L:5L))
#>  int [1:5] NA NA NA NA NA
str(as.na(rnorm(5)))
#>  num [1:5] NA NA NA NA NA
str(as.na(c(TRUE, FALSE)))
#>  logi [1:2] NA NA
str(as.na(as.Date("2017-01-01")))
#>  Date[1:1], format: NA
```

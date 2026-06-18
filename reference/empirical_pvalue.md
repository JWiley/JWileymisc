# Calculates an empirical p-value based on the data

This function takes a vector of statistics and calculates the empirical
p-value, that is, how many fall on the other side of zero. It calculates
a two-tailed p-value.

## Usage

``` r
empirical_pvalue(x, na.rm = TRUE)
```

## Arguments

- x:

  a data vector to operate on

- na.rm:

  Logical whether to remove NA values. Defaults to `TRUE`

## Value

a named vector with the number of values falling at or below zero, above
zero, and the empirical p-value.

## Author

Joshua F. Wiley \<josh@elkhartgroup.com\>

## Examples

``` r

empirical_pvalue(rnorm(100))
#>    <= 0     > 0 p-value 
#>   49.00   51.00    0.98 
```

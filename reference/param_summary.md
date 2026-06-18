# Calculates summaries for a parameter

This function takes a vector of statistics and calculates several
summaries: mean, median, 95 the empirical p-value, that is, how many
fall on the other side of zero.

## Usage

``` r
param_summary(x, trans = function(x) x, ..., na.rm = TRUE)
```

## Arguments

- x:

  a data vector to operate on

- trans:

  A function to transform the data. Used for summaries, but not
  p-values. Defaults to the identity function.

- ...:

  Additional arguments passed to `formatPval` to control p-value
  printing.

- na.rm:

  Logical whether to remove NA values. Defaults to `TRUE`

## Value

A data frame of summary statistics

## Examples

``` r

param_summary(rnorm(100))
#>         Mean   Median       SE     LL2.5  UL97.5 pvalue
#>        <num>    <num>    <num>     <num>   <num> <char>
#> 1: 0.2451365 0.206726 1.104723 -1.783577 2.61219   .900
```

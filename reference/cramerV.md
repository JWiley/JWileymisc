# Calculate Phi or Cramer's V effect size

Simple function to calculate effect sizes for frequency tables.

## Usage

``` r
cramerV(x)
```

## Arguments

- x:

  A frequency table, such as from
  [`xtabs()`](https://rdrr.io/r/stats/xtabs.html).

## Value

A numeric value with Phi for 2 x 2 tables or Cramer's V for tables
larger than 2 x 2.

## Examples

``` r
cramerV(xtabs(~ am + vs, data = mtcars))
#>       Phi 
#> 0.1683451 
cramerV(xtabs(~ cyl + vs, data = mtcars))
#> Cramer's V 
#>  0.8166228 
cramerV(xtabs(~ cyl + am, data = mtcars))
#> Cramer's V 
#>  0.5226355 
```

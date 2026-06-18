# Calculate Standardized Mean Difference (SMD)

Simple function to calculate effect sizes for mean differences.

## Usage

``` r
smd(x, g, index = c("all", "1", "2"))
```

## Arguments

- x:

  A continuous variable

- g:

  A grouping variable, with two levels

- index:

  A character string: “all” uses pooled variance, “1” uses the first
  factor level variance, “2” uses the second factor level variance.

## Value

The standardized mean difference.

## Examples

``` r
smd(mtcars$mpg, mtcars$am)
#>      SMD 
#> 1.477947 
smd(mtcars$mpg, mtcars$am, "all")
#>      SMD 
#> 1.477947 
smd(mtcars$mpg, mtcars$am, "1")
#>      SMD 
#> 1.889672 
smd(mtcars$mpg, mtcars$am, "2")
#>      SMD 
#> 1.174886 

smd(mtcars$hp, mtcars$vs)
#>      SMD 
#> 2.043209 

d <- data.table::as.data.table(mtcars)
d[, smd(mpg, vs)]
#>      SMD 
#> 1.733415 
rm(d)
```

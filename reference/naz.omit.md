# Missing and Zero Character Omit

Given a vector, exclude any missing values, not a number values, non
finite values, and if a character class, any zero length strings.

## Usage

``` r
naz.omit(x)
```

## Arguments

- x:

  A vector to exclude missing, non finite or zero length strings from

## Value

a vector with missing/non finite/zero length strings omitted

## Examples

``` r
## stats na.omit
stats::na.omit(c(1, NA, NaN))
#> [1] 1
#> attr(,"na.action")
#> [1] 2 3
#> attr(,"class")
#> [1] "omit"
stats::na.omit(c("test", "", NA_character_))
#> [1] "test" ""    
#> attr(,"na.action")
#> [1] 3
#> attr(,"class")
#> [1] "omit"

naz.omit(c(1, NA, NaN))
#> [1] 1
naz.omit(c(1L, NA))
#> [1] 1
naz.omit(c(1L, NA, Inf))
#> [1] 1
naz.omit(c("test", "", NA_character_))
#> [1] "test"
```

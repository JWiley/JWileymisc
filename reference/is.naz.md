# Is a variable missing, non finite or zero length character?

Given a vector, return `TRUE` or `FALSE` if each element is either
missing (NA/NaN), non finite (e.g. infinite) or a zero length character
string (only for character vectors).

## Usage

``` r
is.naz(x)
```

## Arguments

- x:

  A vector to identify missing / non finite or zero length strings from

## Value

a logical vector

## Examples

``` r
is.naz(c(1, NA, NaN))
#> [1] FALSE  TRUE  TRUE
is.naz(c(1, NA, NaN, Inf))
#> [1] FALSE  TRUE  TRUE  TRUE
is.naz(c("test", "", NA_character_))
#> [1] FALSE  TRUE  TRUE
```

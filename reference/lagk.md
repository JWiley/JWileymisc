# Create a lagged variable

Given a variable, create a k lagged version, optionally do it by a
grouping factor, such as an ID.

## Usage

``` r
lagk(x, k = 1, by)
```

## Arguments

- x:

  the variable to lag

- k:

  the length to lag it

- by:

  a variable to lag by. Must be sorted.

## Value

a vector of the lagged values

## Examples

``` r
lagk(1:4, 1)
#> [1] NA  1  2  3
```

# Score a set of items to create overall scale score

This function creates a single scale score from a data frame, reversing
as needed.

## Usage

``` r
CheckVals(data, okay, na.rm = TRUE)
```

## Arguments

- data:

  The data to check

- okay:

  A vector of okay or acceptable values

- na.rm:

  Logical whether to remove missing values or not. Defaults to `TRUE`

## Value

`TRUE` if all values are okay, otherwise an error

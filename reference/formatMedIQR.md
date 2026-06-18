# Function to format the median and IQR of a variable

Function to format the median and IQR of a variable

## Usage

``` r
formatMedIQR(x, d = 2, na.rm = TRUE)
```

## Arguments

- x:

  the data to have the median and IQR calculated

- d:

  How many digits to display. Defaults to 2.

- na.rm:

  Logical whether to remove missing values. Defaults to `TRUE`.

## Value

A character string with results

## Examples

``` r
formatMedIQR(mtcars$mpg)
#> [1] "19.20, (15.43, 22.80)"
```

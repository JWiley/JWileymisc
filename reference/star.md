# Function to simplify converting p-values to asterisks

Function to simplify converting p-values to asterisks

## Usage

``` r
star(x, includeMarginal = FALSE)
```

## Arguments

- x:

  p values to convert to stars

- includeMarginal:

  logical value whether to include a symbol for marginally significant
  \>.05 but \< .10 p-values. Defaults to `FALSE`.

## Value

A character string with stars

## Examples

``` r
star(c(.0005, .001, .005, .01, .02, .05, .08, .1, .5, 1))
#>  [1] *** *** **  **  *   *                  
```

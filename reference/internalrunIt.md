# Internal function to run a model using gam()

This function is not intended to be called by users.

## Usage

``` r
internalrunIt(formula, type, data, ...)
```

## Arguments

- formula:

  A character string containing a formula style object.

- type:

  A character string indicating the type of dependent variable.
  Currently “normal”, “binary”, or “count”.

- data:

  A data frame to be used for analysis.

- ...:

  Additional arguments passed to `gam`.

## Value

A summary of the gam model.

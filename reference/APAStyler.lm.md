# APAStyler method for linear models

APAStyler method for linear models

## Usage

``` r
# S3 method for class 'lm'
APAStyler(object, digits = 2, pdigits, file, print = TRUE, ...)
```

## Arguments

- object:

  A `lm` object

- digits:

  The number of digits to round results to. Defaults to 2.

- pdigits:

  The number of digits to use for p values. Defaults to digits + 1 if
  missing.

- file:

  An optional argument indicating whether the output should be written
  to a file.

- print:

  A logical argument, whether or not to print results to screen. This is
  distinct from saving them to a file. Defaults to `TRUE` for back
  compatibility.

- ...:

  Additional argiuments passed on to `write.table`.

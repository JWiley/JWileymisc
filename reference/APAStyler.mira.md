# A generic function for pretty printing in (semi) APA Style

A generic function for pretty printing in (semi) APA Style

## Usage

``` r
# S3 method for class 'mira'
APAStyler(object, lmobject, digits = 2, pdigits, print = TRUE, file, ...)
```

## Arguments

- object:

  `mira` object

- lmobject:

  an lm object the degrees of freedom of which can be used for
  conservative F tests

- digits:

  The number of digits to round results to. Defaults to 2.

- pdigits:

  The number of digits to use for p values. Defaults to digits + 1 if
  missing.

- print:

  A logical argument, whether or not to print results to screen. This is
  distinct from saving them to a file. Defaults to `TRUE` for back
  compatibility.

- file:

  An optional argument indicating whether the output should be written
  to a file.

- ...:

  Additional argiuments passed on to `write.table`.

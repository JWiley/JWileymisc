# Internal Visual Acuity Functions

This function is one of several designed to help convert measures of
visual acuity recorded typically recorded as Snellen fractions (e.g.,
20/20 sees at 20 feet what is "typically" seen at 20 feet. 20/40 sees at
20 feet what is "typically" seen at 40 feet, etc.) into statistically
usable data. It can also parse text data for Counting Fingers (CF) and
Hand Motion (HM) and convert them to approximate logMAR values. This is
an internal function and is not meant to be called directly.

## Usage

``` r
logmar(x, snell.numerator = 20, inverse = FALSE)

CFHM(x, zero)

snellen(snellenvalue, chart.values, chart.nletters)
```

## Arguments

- x:

  Character data of the form: “CF 10”, “HM 12”, “HM”, “CF”, “CF 2”, etc.
  to be converted to logMAR values.

- snell.numerator:

  The numerator of a Snellen fraction. It defaults to 20 (the most
  common one).

- inverse:

  `inverse` = FALSE by default. If TRUE, `logmar` will assume `x` is a
  logMAR value, and calculate the denominator of a Snellen fraction
  using the `snell.numerator` as the numerator.

- zero:

  A “zero” logMAR value to be used for any CF or HM value missing a
  number. May be an actual number or simply `NA`

- snellenvalue:

  The observed snellen values

- chart.values:

  The chart snellen values

- chart.nletters:

  The number of letters per chart line

## Details

This treats CF as approximately 200 letters (per Holladay), so CF at 10
feet has Snellen value "equivalent" of 10/200. HM is approximately 10
times worse, so HM at 10 feet approximately 10/2000. After conversion,
rough equivalents are passed to `logmar` to actually be converted. Other
functions are responsible for suitably parsing the text and passing
numbers to `logmar`. If `inverse = FALSE` (the default), `logmar`
calculates \\-log\_{10}(\frac{snell.numerator}{x})\\. If `TRUE`, then
\\\frac{snell.numerator}{10^{-x}}\\.

The `zero` argument is used to specify a "zero" logMAR value. In
particular, this is used when no distance information is given (e.g.,
only "HM" or "CF" as opposed to "CF 6" or "HM 4"). For the reasioning
and rational behind this, see the "Details" section of
[`VAConverter`](https://joshuawiley.com/JWileymisc/reference/VAConverter.md).

For the `snellen` function, the input should be character data and have
a numerator and denominator separated by '/'. E.g., “20/20”, “20/40 +
3”. This handles both simple and Snellen values that need to be
interpolated given the appropriate chart.

logMAR calculations including interpolating partial lines. Given
“20/25 + 3”, calculate the logMAR of 20/25 and 20/20 (the next step up
since '+'), and go 3/chart.nletters of the way between these two values.
Similarly if “20/20 - 3”, it will go partway between 20/25 and 20/30.
Note that it depends on the lines and letters per line *on the actual
chart used*. The `chart.values` and `chart.nletters` should contain all
the lines and number of letters for the chart that was used.

These functions were written to deal with a very specific style of
recording visual acuity for a study I worked on. It may or may not have
much use elsewhere. `CFHM` was not intended to typically be called by
the user directly. Generally, a higher level function, (e.g.,
`VAConverter`) would be called.

## References

Jack T. Holladay (2004). Visual acuity measurements. *Journal of
Cataract & Refractive Surgery, 30*(2), pp. 287–290.
[doi:10.1016/j.jcrs.2004.01.014](https://doi.org/10.1016/j.jcrs.2004.01.014)

## See also

[`VAConverter`](https://joshuawiley.com/JWileymisc/reference/VAConverter.md)
the overall function typically called

## Examples

``` r
## logMAR value for "perfect" 20/20 vision
JWileymisc:::logmar(x = 20)
#> [1] 0

## Go to and from logMAR value, should return "20"
## there may be slight error due to floating point arithmetic
JWileymisc:::logmar(x = JWileymisc:::logmar(x = 20), inverse = TRUE)
#> [1] 20

## logMAR value for 20/40 vision
JWileymisc:::logmar(40)
#> [1] 0.30103
## logMAR approximations, note "HM" is just the zero value
JWileymisc:::CFHM(c("HM 20", "HM", "CF 20", "CF 12", "CF"), zero = 3)
#> [1] 2.000000 3.000000 1.000000 1.221849 3.000000
## In cases where there is insufficient data, rather than choose
## an arbitrary value, you can may just use NA
JWileymisc:::CFHM(c("HM 20", "HM", "CF 20", "CF 12", "CF"), zero = NA)
#> [1] 2.000000       NA 1.000000 1.221849       NA
```

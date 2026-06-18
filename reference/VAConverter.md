# Visual Acuity Converter

Converter character (string) input of Snellen fractions, Counting
Fingers (CF), and Hand Motion (HM) to logMAR values for use in
statistical models. Can handle linear interpolation if passed an
appropriate chart or if the measures fit with the default chart.

## Usage

``` r
VAConverter(
  OS,
  OD,
  chart.values = NULL,
  chart.nletters = NULL,
  datatype = c("snellen", "decimal", "logMAR"),
  zero = 3
)
```

## Arguments

- OS:

  The values to be converted for the left eye (oculus sinister).

- OD:

  The values to be converted for the right eye (oculus dexter)

- chart.values:

  The Snellen fractions for the chart used (if interpolation is
  necessary and it is different from the default).

- chart.nletters:

  The number of letters on each line of the chart that was used.
  Necessary for proper interpolation.

- datatype:

  The type of data passed to `OS` and `OD`. One of "Snellen" (the
  default), "decimal", or "logMAR". Determines what transformations are
  needed to convert to logMAR values.

- zero:

  The “zero” logMAR value. This is used as the zero point for visual
  acuity. For example, for light perception (LP), no light perception
  (NLP), etc. It defaults to 3 (which is equivalent to a Snellen value
  of 20/20000), but may also be `NA`. See details.

## Value

An object of class
[`VAObject`](https://joshuawiley.com/JWileymisc/reference/VAObject-class.md).
This includes the left and right eye logMAR values in slots `@logMAROS`
and `@logMAROD` as well as additional information. More information can
be found in the class documentation.

## Details

`VAConverter` is primarily designed to take raw character data of
various forms and convert them to logMAR values. Acceptable examples
include: "20/20", "20/80 + 3", "20/20 - 4", "10/20", "CF 10", "HM 2",
"CF 4", "NLP", "LP", "", "CF", "HM", etc. For Snellen values, both parts
should be present, and there should be a space between components; e.g.,
between fraction, +/- and number or between CF and 10. Although I have
attempted to make it as flexible and general as possible, there are
still fairly rigid requirements so that it can parse a variety of text
formats to numerical values. Optionally, it can also handle decimal
values (i.e., the results of actually dividing a Snellen value 20/20 =
1).

`chart.values` and `chart.nletters` must be the same length. These are
used to interpolate values such as "20/20 + 3" which is interpreted as
reading all of the letters on the "20/20" line and "3" of the letters on
the next best line (typically "20/15" but this can be chart dependent).
The functions goes 3/n of the distance between the logMAR values for
each line. This is why it is important to know the values for the chart
*that was actually used*.

If datatype = "logMAR", the values passed to `OS` and `OD` are directly
assigned to the `logMAROS` and `logMAROD` slots of a
`"`[`VAObject`](https://joshuawiley.com/JWileymisc/reference/VAObject-class.md)`"`
and an error is returned if that results in the creation of an invalid
object (e.g., they are not numeric or not of equal length).

The `zero` argument is primarily included to facilitate calculating
averages. For example, in some cases it may be nice to get a sense of an
individual's "overall" or "average" logMAR value. Because on the logMAR
scale, 0 is "20/20", an alternate number needs to be used. 3 was chosen
as a rough default, but it is by no means necessarily the best choice.
If you are not interested in computing an average between the left and
right eyes within individuals, it makes sense to simply use `NA` rather
than a crude "zero" approximation.

## Examples

``` r
## sampdat <- c("HM 12", "20/20 + 3", "20/50", "CF", "HM",
##              "20/70 - 2", "LP", NA, "Prosthetic")
## tmp <- VAConverter(OS = sampdat, OD = rev(sampdat), datatype = "snellen")
```

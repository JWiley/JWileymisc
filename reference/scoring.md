# Score a set of items to create overall scale score - generic

Score a set of items to create overall scale score - generic

## Usage

``` r
score(
  data,
  reverse = NULL,
  limits = NULL,
  mean = TRUE,
  reliability = TRUE,
  na.rm = TRUE,
  ...
)

.scoreCESD(data, okay = c(0, 1, 2, 3), reverse = c(4, 8, 12, 16), ...)

.scoreLOTR(
  data,
  okay = c(1, 2, 3, 4, 5),
  reverse = c(2, 4, 5),
  indices = list(oindex = c(1, 3, 6), pindex = c(2, 4, 5)),
  ...
)

.scoreMastery(data, okay = c(1, 2, 3, 4), reverse = c(1, 6), ...)

.scoreMOSSSS(
  data,
  okay = c(1, 2, 3, 4, 5),
  indices = list(Structural = 1, Tangible = c(2, 5, 12, 15), Affectionate = c(6, 10, 20),
    PositiveInteraction = c(7, 11, 18), EmotionalInformational = c(3, 9, 16, 19, 4, 8,
    13, 17), Functional = 2:20),
  ...
)

.scorePANAS(
  data,
  okay = c(1, 2, 3, 4, 5),
  indices = list(pos = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19), neg = c(2, 4, 6, 7, 8, 11,
    13, 15, 18, 20)),
  ...
)

.scoreRSES(data, okay = c(0, 1, 2, 3), reverse = c(3, 5, 8, 9, 10), ...)

.scoreMOOD(
  data,
  indices = list(vision = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 18, 19), impact =
    c(13, 14, 15, 16, 17, 20, 21)),
  ...
)

scaleScore(
  data,
  type = c("CESD", "LOTR", "Mastery", "RSES", "MOSSSS", "PANAS"),
  ...
)
```

## Arguments

- data:

  A data frame or an object coercable to a data frame.

- reverse:

  A vector the same length as the number of columns in the data

- limits:

  An optional vector indicating the lower and upper possible limits (for
  reversing)

- mean:

  Logical whether to calculate the mean if `TRUE` or sum if `FALSE`

- reliability:

  Logical whether or not to calculate reliability information for the
  scale. Defaults to `TRUE`.

- na.rm:

  Logical whether to remove missing values or not. Defaults to `TRUE`

- ...:

  Additional arguments passed on to lower level functions

- okay:

  A vector of okay or acceptable values

- indices:

  Indicates columns for subscales, where applicable

- type:

  A character string indicating the scale name, the type of scoring to
  use.

## Value

A list containing the results.

- score:

  The calculated scores.

- reliability:

  Results from the omega function.

## See also

[`omega`](https://rdrr.io/pkg/psych/man/omega.html)

# An S4 class to hold visual acuity data

A class to hold Visual Acuity data for the oculus sinister (OS; left
eye) and oculus dexter (OD; right eye)

## Usage

``` r
# S4 method for class 'VAObject'
show(object)

# S4 method for class 'VAObject,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]

# S4 method for class 'VAObject'
print(x, ...)

# S4 method for class 'VAObject'
show(object)

# S4 method for class 'VAObject'
summary(object, weightbest = TRUE, w = c(0.75, 0.25))
```

## Arguments

- object:

  A VAObject class object

- x:

  the object to subset

- i:

  the rows to subset (optional)

- j:

  the columns to subset (optional)

- ...:

  Additional arguments passed to lower functions

- drop:

  should be missing

- weightbest:

  Logical whether to upweight the best seeing eye. Defaults to `TRUE`.

- w:

  A numeric vector of the weights, first for the best seeing then the
  worst seeing eye. Defaults to `c(.75, .25)`.

## Methods (by generic)

- `show(VAObject)`: show method

- `x[i`: extract method

- `print(VAObject)`: print method

- `show(VAObject)`: show method

- `summary(VAObject)`: summary method

## Slots

- `originalOS`:

  the original visual acuity data for the left (ocular sinister) eye

- `originalOD`:

  the original visual acuity data for the right (ocular dexter) eye

- `logMAROS`:

  Logarithm of the minimum angle of resolution data for OS

- `logMAROD`:

  Logarithm of the minimum angle of resolution data for OD

- `chart.values`:

  the snellen values for each line of the chart used to measure visual
  acuity. Used for the linear interpolation in the case of partially
  correct line readings.

- `chart.nletters`:

  the number of letters on each line of the chart used to measure visual
  acuity. Used for the linear interpolation in the case of partially
  correct line readings (+2 is 2/4 of the way to the next line if there
  are four letters, but only 2/6 if there are six, etc.)

- `zero`:

  the logMAR value chosen to represent "zero" visual acuity when
  creating the combined logMAR values for both eyes or taking the
  arithmetic mean.

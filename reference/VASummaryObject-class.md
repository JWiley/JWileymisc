# An S4 class to hold visual acuity summary data

A class designed to hold visual acuity summary data

## Usage

``` r
# S4 method for class 'VASummaryObject'
show(object)

# S4 method for class 'VASummaryObject,missing'
plot(x, y, ...)
```

## Arguments

- object:

  The object to be shown

- x:

  A VASummaryObject

- y:

  Should be missing

- ...:

  Additional, unused arguments

## Methods (by generic)

- `show(VASummaryObject)`: show method

- `plot(x = VASummaryObject, y = missing)`: plot method

## Slots

- `logMAR.combined`:

  Numeric values of the combined logarithm of the minimum angle of
  resolution data for both eyes

- `snellen.combined`:

  the snellen values back transformed from the combined logMAR values

- `mean.logMAR`:

  average of the logarithm of the minimum angle of resolution data

- `mean.snellen`:

  average of the combined Snellen data

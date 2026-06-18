# Plot Residual Diagnostics Default Method

This function creates a number of diagnostic plots from residuals. It is
a default method.

## Usage

``` r
# S3 method for class 'residualDiagnostics'
plot(x, y, plot = TRUE, ask = TRUE, ncol, ...)
```

## Arguments

- x:

  A `residualDiagnostics` class object.

- y:

  Included to match the generic. Not used.

- plot:

  A logical value whether or not to plot the results or simply return
  the graphical objects.

- ask:

  A logical whether to ask before changing plots. Only applies to
  interactive environments.

- ncol:

  The number of columns to use for plots. Missing by default which means
  individual plots are created. If specified, plots are put together in
  a grid.

- ...:

  Included to match the generic. Not used.

## Value

a list including plots of the residuals, residuals versus fitted values

## Examples

``` r
testm <- stats::lm(mpg ~ hp * factor(cyl), data = mtcars)
testm <- stats::lm(mpg ~ factor(cyl), data = mtcars)

md <- residualDiagnostics(testm, ev.perc = .1)

plot(md, plot = FALSE)$ResFittedPlot

plot(md, ncol = 2)


## clean up
rm(testm, md)
```

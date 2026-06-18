# Function to find significant regions from an interaction

This function uses the `contrast` function from rms to find the
threshold for significance from interactions.

## Usage

``` r
intSigRegGraph(
  object,
  predList,
  contrastList,
  xvar,
  varyvar,
  varyvar.levels,
  xlab = xvar,
  ylab = "Predicted Values",
  ratio = 1,
  xlim,
  ylim,
  xbreaks,
  xlabels = xbreaks,
  scale.x = c(m = 0, s = 1),
  scale.y = c(m = 0, s = 1),
  starts = 50
)
```

## Arguments

- object:

  A fitted rms object

- predList:

  TODO

- contrastList:

  TODO

- xvar:

  TODO

- varyvar:

  TODO

- varyvar.levels:

  TODO

- xlab:

  optional

- ylab:

  TODO

- ratio:

  TODO

- xlim:

  TODO

- ylim:

  TODO

- xbreaks:

  TODO

- xlabels:

  optional

- scale.x:

  optional

- scale.y:

  optional

- starts:

  Number of starting values to try between the lower and upper bounds.

## Value

A data table with notes if no convergence or significance thresholds (if
any).

## Examples

``` r
## make me
```

# Internal Function to Calculate Quantiles

Function calculates smoothing spline quantiles or linear quantiles as a
fall back. Not intended for general use. Expected predicted and residual
data. Exported to support related packages.

## Usage

``` r
.quantilePercentiles(
  data,
  Mid = 0.5,
  LL = 0.1,
  UL = 0.9,
  na.rm = TRUE,
  cut = 8L
)
```

## Arguments

- data:

  A dataset of predicted and residual values. Assumed from some sort of
  (probably parametric) model.

- Mid:

  The middle limit for prediction. Defaults to `.5` to give the median.

- LL:

  The lower limit for prediction. Defaults to `.1` to give the 10th
  percentile.

- UL:

  The upper limit for prediction. Defaults to `.9` to give the 90th
  percentile.

- na.rm:

  A logical whether to remove missing values. Defaults to `TRUE`

- cut:

  An integer, how many unique predicted values there have to be at least
  for it to use quantile regression or treat the predicted values as
  discrete. Defaults to 8.

## Value

A data.table with the scores and predicted LL and UL, possibly missing
if quantile regression models do not converge.

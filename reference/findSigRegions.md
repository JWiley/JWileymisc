# Function to find significant regions from an interaction

This function uses the `contrast` function from rms to find the
threshold for significance from interactions.

## Usage

``` r
findSigRegions(
  object,
  l1,
  l2,
  name.vary,
  lower,
  upper,
  alpha = 0.05,
  starts = 50
)
```

## Arguments

- object:

  A fitted rms object

- l1:

  the first set of values to fix for the contrast function

- l2:

  the second set of values to fix for the contrast function

- name.vary:

  the name of the model parameter to vary values for to find the
  threshold. Note that this should not be included in `l1` or `l2`
  arguments.

- lower:

  The lower bound to search for values for the varying value

- upper:

  The upper bound to search for values for the varying value

- alpha:

  The significance threshold, defaults to `.05`

- starts:

  Number of starting values to try between the lower and upper bounds.

## Value

A data table with notes if no convergence or significance thresholds (if
any).

## Examples

``` r
## make me
```

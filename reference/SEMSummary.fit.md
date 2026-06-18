# Summary Statistics for a SEM Analysis

This is a low level fitting function, for SEMSummary.

## Usage

``` r
SEMSummary.fit(
  formula,
  data,
  use = c("fiml", "pairwise.complete.obs", "complete.obs")
)
```

## Arguments

- formula:

  A formula of the variables to be used in the analysis. See the
  ‘details’ section for more information.

- data:

  A data frame, matrix, or list containing the variables used in the
  formula. This is a required argument.

- use:

  A character vector of how to handle missing data. Defaults to “fiml”.

## Value

A list with S3 class “SEMSummary”

- names:

  A character vector containing the variable names.

- n:

  An integer vector of the length of each variable used (this includes
  available and missing data).

- nmissing:

  An integer vector of the number of missing values in each variable.

- mu:

  A vector of the arithmetic means of each variable (on complete data).

- stdev:

  A numeric vector of the standard deviations of each variable (on
  complete data).

- Sigma:

  The numeric covariance matrix for all variables.

- sSigma:

  The numeric correlation matrix for all variables.

- coverage:

  A numeric matrix giving the percentage (technically decimal) of
  information available for each pairwise covariance/correlation.

- pvalue:

  The two-sided p values for the correlation matrix. Pairwise present N
  used to calculate degrees of freedom.

## See also

[`SEMSummary`](https://joshuawiley.com/JWileymisc/reference/SEMSummary.md)

# Estimate the first and second moments

This function relies on the lavaan package to use the Expectation
Maximization (EM) algorithm to estimate the first and second moments
(means and \[co\]variances) when there is missing data.

## Usage

``` r
moments(data, ...)
```

## Arguments

- data:

  A data frame or an object coercable to a data frame. The means and
  covariances of all variables are estimated.

- ...:

  Additional arguments passed on to the `estimate.moments.EM` function
  in lavaan. Note this is not an exported function.

## Value

A list containing the esimates from the EM algorithm.

- mu:

  A named vector of the means.

- sigma:

  The covariance matrix.

## See also

[`SEMSummary`](https://joshuawiley.com/JWileymisc/reference/SEMSummary.md)

## Author

Suggested by Yves Rosseel author of the lavaan package on which this
depends

## Examples

``` r
# sample data
Xmiss <- as.matrix(iris[, -5])
# make 25% missing completely at random
set.seed(10)
Xmiss[sample(length(Xmiss), length(Xmiss) * .25)] <- NA
Xmiss <- as.data.frame(Xmiss)

# true means and covariance
colMeans(iris[, -5])
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>     5.843333     3.057333     3.758000     1.199333 
# covariance with n - 1 divisor
cov(iris[, -5])
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    0.6856935  -0.0424340    1.2743154   0.5162707
#> Sepal.Width    -0.0424340   0.1899794   -0.3296564  -0.1216394
#> Petal.Length    1.2743154  -0.3296564    3.1162779   1.2956094
#> Petal.Width     0.5162707  -0.1216394    1.2956094   0.5810063

# means and covariance matrix using list wise deletion
colMeans(na.omit(Xmiss))
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>     5.900000     3.126667     3.791111     1.235556 
cov(na.omit(Xmiss))
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length   0.77727273 -0.04704545     1.375000   0.5759091
#> Sepal.Width   -0.04704545  0.16654545    -0.366803  -0.1421061
#> Petal.Length   1.37500000 -0.36680303     3.311737   1.4110051
#> Petal.Width    0.57590909 -0.14210606     1.411005   0.6359798

# means and covariance matrix using EM
moments(Xmiss)
#> $sigma
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length   0.66931832 -0.05424143    1.2030205   0.5097302
#> Sepal.Width   -0.05424143  0.19581274   -0.3715246  -0.1330635
#> Petal.Length   1.20302046 -0.37152456    2.9372324   1.2524655
#> Petal.Width    0.50973016 -0.13306354    1.2524655   0.5752271
#> 
#> $mu
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>     5.853181     3.076239     3.695702     1.178956 
#> 
# clean up
rm(Xmiss)
```

# Convert a correlation matrix and standard deviations to a covariance matrix

This is a simple function designed to convert a correlation matrix
(standardized covariance matrix) back to a covariance matrix. It is the
opposite of `cov2cor`.

## Usage

``` r
cor2cov(V, sigma)
```

## Arguments

- V:

  an n x n correlation matrix. Should be numeric, square, and symmetric.

- sigma:

  an n length vector of the standard deviations. The length of the
  vector must match the number of columns in the correlation matrix.

## Value

an n x n covariance matrix

## See also

[`cov2cor`](https://rdrr.io/r/stats/cor.html)

## Examples

``` r
# using a built in dataset
cor2cov(cor(longley), sapply(longley, sd))
#>              GNP.deflator       GNP Unemployed Armed.Forces Population
#> GNP.deflator    116.45762 1063.6041   625.8666     349.0254   73.50300
#> GNP            1063.60412 9879.3537  5612.4370    3088.0428  685.24094
#> Unemployed      625.86663 5612.4370  8732.2343   -1153.7876  446.27415
#> Armed.Forces    349.02537 3088.0428 -1153.7876    4843.0410  176.40981
#> Population       73.50300  685.2409   446.2742     176.4098   48.38735
#> Year             50.92333  470.9779   297.3033     138.2433   32.91740
#> Employed         36.79666  343.3302   164.9103     111.7681   23.46197
#>                   Year  Employed
#> GNP.deflator  50.92333  36.79666
#> GNP          470.97790 343.33021
#> Unemployed   297.30333 164.91027
#> Armed.Forces 138.24333 111.76811
#> Population    32.91740  23.46197
#> Year          22.66667  16.24093
#> Employed      16.24093  12.33392

# should match the above covariance matarix
cov(longley)
#>              GNP.deflator       GNP Unemployed Armed.Forces Population
#> GNP.deflator    116.45763 1063.6041   625.8666     349.0254   73.50300
#> GNP            1063.60412 9879.3537  5612.4370    3088.0428  685.24094
#> Unemployed      625.86663 5612.4370  8732.2343   -1153.7876  446.27415
#> Armed.Forces    349.02537 3088.0428 -1153.7876    4843.0410  176.40981
#> Population       73.50300  685.2409   446.2742     176.4098   48.38735
#> Year             50.92333  470.9779   297.3033     138.2433   32.91740
#> Employed         36.79666  343.3302   164.9103     111.7681   23.46197
#>                   Year  Employed
#> GNP.deflator  50.92333  36.79666
#> GNP          470.97790 343.33021
#> Unemployed   297.30333 164.91027
#> Armed.Forces 138.24333 111.76811
#> Population    32.91740  23.46197
#> Year          22.66667  16.24093
#> Employed      16.24093  12.33392
all.equal(cov(longley), cor2cov(cor(longley), sapply(longley, sd)))
#> [1] TRUE
```

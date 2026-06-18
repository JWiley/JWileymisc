# Winsorize at specified percentiles

Simple function winsorizes data at the specified percentile.

## Usage

``` r
winsorizor(d, percentile, values, na.rm = TRUE)
```

## Arguments

- d:

  A vector, matrix, data frame, or data table to be winsorized

- percentile:

  The percentile bounded by \[0, 1\] to winsorize data at. If a data
  frame or matrix is provided for the data, this should have the same
  length as the number of columns, or it will be repeated for all.

- values:

  If values are specified, use these instead of calculating by
  percentiles. Should be a data frame with columns named “low”, and
  “high”. If a data frame or matrix is provided for the data, there
  should be as many rows for values to winsorize at as there are columns
  in the data.

- na.rm:

  A logical whether to remove NAs.

## Value

winsorized data. Attributes are included to list the exact values (for
each variable, if a data frame or matrix) used to winsorize at the lower
and upper ends.

## Examples

``` r
dev.new(width = 10, height = 5)
par(mfrow = c(1, 2))
hist(as.vector(eurodist), main = "Eurodist")
hist(winsorizor(as.vector(eurodist), .05), 
  main = "Eurodist with lower and upper\n5% winsorized")

library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
dat <- data.table(x = 1:5)
dat[, y := scale(1:5)]
#>        x          y
#>    <int>      <num>
#> 1:     1 -1.2649111
#> 2:     2 -0.6324555
#> 3:     3  0.0000000
#> 4:     4  0.6324555
#> 5:     5  1.2649111
winsorizor(dat$y, .01)
#> Warning: Atomic type with no dimensions, coercing to a numeric vector.
#> To remove this warning, try wrapping the data in as.numeric() or
#> otherwise coercing to a vector prior to passing to winsorizor().
#> [1] -1.2396128 -0.6324555  0.0000000  0.6324555  1.2396128
#> attr(,"winsorizedValues")
#>         low     high percentile
#> 1 -1.239613 1.239613       0.01

## make a copy of the data table
winsorizor(dat, .01)
#>        x          y
#>    <num>      <num>
#> 1:  1.04 -1.2396128
#> 2:  2.00 -0.6324555
#> 3:  3.00  0.0000000
#> 4:  4.00  0.6324555
#> 5:  4.96  1.2396128

winsorizor(mtcars, .01)
#>       mpg cyl    disp     hp  drat      wt    qsec vs am gear carb
#> 1  21.000   6 160.000 110.00 3.900 2.62000 16.4600  0  1    4 4.00
#> 2  21.000   6 160.000 110.00 3.900 2.87500 17.0200  0  1    4 4.00
#> 3  22.800   4 108.000  93.00 3.850 2.32000 18.6100  1  1    4 1.00
#> 4  21.400   6 258.000 110.00 3.080 3.21500 19.4400  1  0    3 1.00
#> 5  18.700   8 360.000 175.00 3.150 3.44000 17.0200  0  0    3 2.00
#> 6  18.100   6 225.000 105.00 2.760 3.46000 20.2200  1  0    3 1.00
#> 7  14.300   8 360.000 245.00 3.210 3.57000 15.8400  0  0    3 4.00
#> 8  24.400   4 146.700  62.00 3.690 3.19000 20.0000  1  0    4 2.00
#> 9  22.800   4 140.800  95.00 3.920 3.15000 22.0692  1  0    4 2.00
#> 10 19.200   6 167.600 123.00 3.920 3.44000 18.3000  1  0    4 4.00
#> 11 17.800   6 167.600 123.00 3.920 3.44000 18.9000  1  0    4 4.00
#> 12 16.400   8 275.800 180.00 3.070 4.07000 17.4000  0  0    3 3.00
#> 13 17.300   8 275.800 180.00 3.070 3.73000 17.6000  0  0    3 3.00
#> 14 15.200   8 275.800 180.00 3.070 3.78000 18.0000  0  0    3 3.00
#> 15 10.400   8 468.280 205.00 2.930 5.25000 17.9800  0  0    3 4.00
#> 16 10.400   8 460.000 215.00 3.000 5.39951 17.8200  0  0    3 4.00
#> 17 14.700   8 440.000 230.00 3.230 5.34500 17.4200  0  0    3 4.00
#> 18 32.400   4  78.700  66.00 4.080 2.20000 19.4700  1  1    4 1.00
#> 19 30.400   4  75.700  55.10 4.775 1.61500 18.5200  1  1    4 2.00
#> 20 33.435   4  72.526  65.00 4.220 1.83500 19.9000  1  1    4 1.00
#> 21 21.500   4 120.100  97.00 3.700 2.46500 20.0100  1  0    3 1.00
#> 22 15.500   8 318.000 150.00 2.760 3.52000 16.8700  0  0    3 2.00
#> 23 15.200   8 304.000 150.00 3.150 3.43500 17.3000  0  0    3 2.00
#> 24 13.300   8 350.000 245.00 3.730 3.84000 15.4100  0  0    3 4.00
#> 25 19.200   8 400.000 175.00 3.080 3.84500 17.0500  0  0    3 2.00
#> 26 27.300   4  79.000  66.00 4.080 1.93500 18.9000  1  1    4 1.00
#> 27 26.000   4 120.300  91.00 4.430 2.14000 16.7000  0  1    5 2.00
#> 28 30.400   4  95.100 113.00 3.770 1.54462 16.9000  1  1    5 2.00
#> 29 15.800   8 351.000 264.00 4.220 3.17000 14.5310  0  1    5 4.00
#> 30 19.700   6 145.000 175.00 3.620 2.77000 15.5000  0  1    5 6.00
#> 31 15.000   8 301.000 312.99 3.540 3.57000 14.6000  0  1    5 7.38
#> 32 21.400   4 121.000 109.00 4.110 2.78000 18.6000  1  1    4 2.00

winsorizor(matrix(1:9, 3), .01)
#>      [,1] [,2] [,3]
#> [1,] 1.02 4.02 7.02
#> [2,] 2.00 5.00 8.00
#> [3,] 2.98 5.98 8.98
#> attr(,"winsorizedValues")
#>    low high percentile
#> 1 1.02 2.98       0.01
#> 2 4.02 5.98       0.01
#> 3 7.02 8.98       0.01

rm(dat) # clean up
```

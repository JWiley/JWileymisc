# Calculate F and p-value from the R2

Calculate F and p-value from the R2

## Usage

``` r
f.r2(r2, numdf, dendf)
```

## Arguments

- r2:

  r squareds

- numdf:

  numerator degrees of freedom

- dendf:

  denominator degrees of freedom

## Value

a vector

## Examples

``` r
JWileymisc:::f.r2(.30, 1, 99)
#>            F        NumDF        DenDF            p 
#> 4.242857e+01 1.000000e+00 9.900000e+01 3.071123e-09 
```

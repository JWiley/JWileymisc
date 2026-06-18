# Format a data frame of summary statistics

This functions nicely formats a data frame of parameter summary
statistics and is designed to be used with the param_summary() function.

## Usage

``` r
param_summary_format(d, digits = getOption("digits"), pretty = FALSE)
```

## Arguments

- d:

  A data frame of the parameter summary statistics

- digits:

  Number of digits to round to for printing

- pretty:

  Logical value whether prettified values should be returned. Defaults
  to `FALSE`.

## Value

A formatted data.table of summary statistics or a formated vector (if
`pretty = TRUE`).

## Examples

``` r
set.seed(1234)
xsum <- do.call(rbind, apply(matrix(rnorm(100*10), ncol = 10),
  2, param_summary))
rownames(xsum) <- letters[1:10]
param_summary_format(xsum)
#>           Mean     Median        SE      LL2.5    UL97.5 pvalue
#>         <char>     <char>    <char>     <char>    <char> <char>
#>  1: -0.1567617 -0.3846280 1.0044053 -1.7219858 2.0969651   .700
#>  2:  0.0412432  0.0328033 1.0321873 -2.0164916 2.0228078   .980
#>  3:  0.1546037  0.2778768 0.9601544 -1.6890724 1.9511068   .740
#>  4: -0.0081051 -0.0431601 1.0503090 -2.3309867 1.9026890   .940
#>  5: -0.0217859 -0.0085302 1.1166766 -1.9308186 2.1481116  1.000
#>  6: -0.1368770 -0.0672109 0.9289288 -2.0963626 1.4303246   .920
#>  7: -0.0878618 -0.0504099 0.9190978 -1.8120791 1.5856680   .960
#>  8: -0.0008372 -0.1044569 0.9849230 -1.7962415 1.7617463   .920
#>  9:  0.0181244 -0.0518836 0.9235805 -1.7336758 1.8020527   .940
#> 10: -0.0677146 -0.0353446 1.0413951 -2.3707526 1.9937100  1.000
param_summary_format(xsum, pretty = TRUE)
#>                                               a 
#>  "-0.1567617 [-1.7219858, 2.0969651], p = .700" 
#>                                               b 
#>   "0.0412432 [-2.0164916, 2.0228078], p = .980" 
#>                                               c 
#>   "0.1546037 [-1.6890724, 1.9511068], p = .740" 
#>                                               d 
#>  "-0.0081051 [-2.3309867, 1.9026890], p = .940" 
#>                                               e 
#> "-0.0217859 [-1.9308186, 2.1481116], p = 1.000" 
#>                                               f 
#>  "-0.1368770 [-2.0963626, 1.4303246], p = .920" 
#>                                               g 
#>  "-0.0878618 [-1.8120791, 1.5856680], p = .960" 
#>                                               h 
#>  "-0.0008372 [-1.7962415, 1.7617463], p = .920" 
#>                                               i 
#>   "0.0181244 [-1.7336758, 1.8020527], p = .940" 
#>                                               j 
#> "-0.0677146 [-2.3707526, 1.9937100], p = 1.000" 

rm(xsum)
```

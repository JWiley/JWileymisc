# Function to simplify formatting p-values for easy viewing / publication

Function to simplify formatting p-values for easy viewing / publication

## Usage

``` r
formatPval(
  x,
  d = 3,
  sd,
  includeP = FALSE,
  includeSign = FALSE,
  dropLeadingZero = TRUE
)
```

## Arguments

- x:

  p values to convert

- d:

  number of digits

- sd:

  number of scientific digits. Defaults to `d` if missing.

- includeP:

  logical value whether to include the character “p” itself. Defaults to
  `FALSE`.

- includeSign:

  logical value whether to include the character “=” or “\<”. Defaults
  to `FALSE` and if `includeP = TRUE` it must be `TRUE`.

- dropLeadingZero:

  logical value whether to drop leading zeros for p-values. Defaults to
  `TRUE`.

## Value

A character string with stars

## Examples

``` r
formatPval(c(.00052456, .000000124, .01035, .030489, .534946))
#> [1] "< .001" "< .001" ".010"   ".030"   ".535"  
formatPval(c(.00052456, .000000124, .01035, .030489, .534946), 3, 3, FALSE, TRUE)
#> [1] "< .001" "< .001" "= .010" "= .030" "= .535"
formatPval(c(.00052456, .000000124, .01035, .030489, .534946), 3, 3, TRUE, TRUE)
#> [1] "p < .001" "p < .001" "p = .010" "p = .030" "p = .535"
formatPval(c(.00052456, .000000124, .01035, .030489, .534946), 5)
#> [1] ".00052"   "< .00001" ".01035"   ".03049"   ".53495"  
formatPval(c(1, .15346, .085463, .05673, .04837, .015353462,
  .0089, .00164, .0006589, .0000000053326), 3, 5)
#>  [1] "1.000"  ".153"   ".085"   ".057"   ".048"   ".015"   ".009"   ".002"  
#>  [9] "< .001" "< .001"
formatPval(c(1, .15346, .085463, .05673, .04837, .015353462,
  .0089, .00164, .0006589, .0000000053326), 3, 5, dropLeadingZero = FALSE)
#>  [1] "1.000"  "0.153"  "0.085"  "0.057"  "0.048"  "0.015"  "0.009"  "0.002" 
#>  [9] "< .001" "< .001"
```

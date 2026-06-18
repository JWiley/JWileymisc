# Return a non-missing correlation matrix

Given a square, symmetric matrix (such as a correlation matrix) this
function tries to drop the fewest possible number of variables to return
a (square, symmetric) matrix with no missing cells.

## Usage

``` r
corOK(x, maxiter = 100)
```

## Arguments

- x:

  a square, symmetric matrix or object coercable to such (such as a data
  frame).

- maxiter:

  a number indicating the maximum number of iterations, currently as a
  sanity check. See details.

## Value

A list with two elements

- x:

  The complete non missing matrix.

- keep.indices:

  A vector of the columns and rows from the original matrix to be kept
  (i.e., that are nonmissing).

## Details

The assumption that x is square and symmetric comes because it is
assumed that the number of missing cells for a given column are
identical to that of the corresponding row. `corOK` finds the column
with the most missing values, and drops that (and its corresponding
row), and continues on in like manner until the matrix has no missing
values. Although this was intended for a correlation matrix, it could be
used on other types of matrices. Note that because `corOK` uses an
iterative method, it can be slow when many columns/rows need to be
removed. For the intended use (correlation matrices) there probably
should not be many missing. As a sanity check and to prevent tediously
long computations, the maximum number of iterations can be set.

## Examples

``` r
cormat <- cor(iris[, -5])
# set missing
cormat[cbind(c(1,2), c(2,1))] <- NA

# print
cormat
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000          NA    0.8717538   0.8179411
#> Sepal.Width            NA   1.0000000   -0.4284401  -0.3661259
#> Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
#> Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000

# return complete
corOK(cormat)
#> $x
#>              Sepal.Width Petal.Length Petal.Width
#> Sepal.Width    1.0000000   -0.4284401  -0.3661259
#> Petal.Length  -0.4284401    1.0000000   0.9628654
#> Petal.Width   -0.3661259    0.9628654   1.0000000
#> 
#> $keep.indices
#> [1] 2 3 4
#> 

# using maximum iterations
corOK(cormat, maxiter=0)
#> Warning: Maximum iterations exceeded.
#> Currently kept indices will be returned.
#> Try increasing maxiter or check why so many correlations are missing.
#> [1] 1 2 3 4

# clean up
rm(cormat)
```

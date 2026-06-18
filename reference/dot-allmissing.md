# Determine which if any variables are all missing in a dataset

Internal function.

## Usage

``` r
.allmissing(data)
```

## Arguments

- data:

  A dataset to check each variable if all missing / non finite / zero
  character vectors

## Value

`FALSE` if no variable(s) all missing, else an informative string
message.

## Examples

``` r
JWileymisc:::.allmissing(mtcars)
#> [1] FALSE
cat(JWileymisc:::.allmissing(data.frame(a = NA, b = 1)), fill = TRUE)
#> the following variable(s) were all missing/not finite/empty strings:
#> a
```

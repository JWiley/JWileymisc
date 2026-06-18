# Create a character vector or file hash of a dataset and each variable

Given a `data.frame` or `data.table`, create a character vector MD5 hash
of the overall dataset and each variable. The goal of this is to create
a secure vector / text file that can be tracked using version control
(e.g., GitHub) without requiring commiting sensitive datasets. The
tracking will make it possible to evaluate whether two datasets are the
same, such as when sending data or when datasets may change over time to
know which variable(s) changed, if any.

## Usage

``` r
hashDataset(x, file)
```

## Arguments

- x:

  A `data.frame` or `data.table` to be hashed.

- file:

  An optional character string. If given, assumed to be the path/name of
  a file to write the character string hash out to, for convenience.
  When non missing, the character vector is returned invisibly and a
  file written. When missing (default), the character vector is returned
  directly.

## Value

A (possibly invisible) character vector. Also (optionally) a text file
written version of the character string.

## Examples

``` r

hashDataset(mtcars)
#>  [1] "dataset: 'mtcars', MD5: a63c70e73b58d0823ab3bcbd3b543d6f"                                     
#>  [2] "'mpg' (numeric), f8e0303e137d946eec8ee06e2f152f51, 95d3087aa4b1ccb7e1ef49ba4b6ed83b (sorted)" 
#>  [3] "'cyl' (numeric), 1b8fcb72575f2a120d77279ecf8851f9, ebfb767c78836025f7917bc845443804 (sorted)" 
#>  [4] "'disp' (numeric), 4743a496928535c54d157d145aa114e1, 46b67296541e9f9c4c4e2b70227f9673 (sorted)"
#>  [5] "'hp' (numeric), c1d443aac7c7e1284e2d0a5a6cbe1a98, cd2e69df37f9f864e98780c5d588c95a (sorted)"  
#>  [6] "'drat' (numeric), 87a99a22f86d30cea74565dbad577790, f8bbcfc7e573cf1c72600683ac354d7a (sorted)"
#>  [7] "'wt' (numeric), 2515679118871778eb21a32575973085, 67b94c293a66443abcb0427684b5ae28 (sorted)"  
#>  [8] "'qsec' (numeric), 8632d4fe83c9a91aa2c3f5069002c870, 6875ea7f4d4a3ec56ff6fc214b69f7ff (sorted)"
#>  [9] "'vs' (numeric), 34b3720888fd730d1dbb5d01c5d57ae3, e5a86583130d2649356bc9dc6060787d (sorted)"  
#> [10] "'am' (numeric), 9bc5bef75a08e8812da6f8542311aaba, 3ec14454a35c411d95eedb78c176c0b4 (sorted)"  
#> [11] "'gear' (numeric), 1415993e090fa6fd2078f6d907deb29f, 1cd9d9b506d3170cfeb79a869b29211f (sorted)"
#> [12] "'carb' (numeric), 5b570f391814e8569d81dd5e2a9f30e0, 2acb8f5f747919d7befb9bba53dd6a68 (sorted)"

## if a file is specified it will write the results to the text file
## nicely formatted, along these lines

cat(hashDataset(cars), sep = "\n")
#> dataset: 'cars', MD5: f98a59010652c8e1ee062ed4c43f648e
#> 'speed' (numeric), 4eb3e01aee9abbc01e91d22b651be559, 4eb3e01aee9abbc01e91d22b651be559 (sorted)
#> 'dist' (numeric), be6c7701fccdd59b5e47c48881a2acae, 5cf4867b5bc8dc320c7aea4513d0f557 (sorted)
```

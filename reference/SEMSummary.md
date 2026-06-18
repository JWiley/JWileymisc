# Summary Statistics for a SEM Analysis

This function is designed to calculate the descriptive statistics and
summaries that are often reported on raw data when the main analyses use
structural equation modelling.

## Usage

``` r
SEMSummary(
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

## Details

This function calculates a variety of relevant statistics on the raw
data used in a SEM analysis. Because it is meant for SEM style data, for
now it expects all variables to be numeric. In the future I may try to
expand it to handle factor variables somehow.

Both the formula and data arguments are required. The formula should be
the right hand side only. The most common way to use it would be with
variable names separated by ‘+s’. For convenience, a ‘.’ is expanded to
mean “all variables in the data set”. For a large number of variables or
when whole datasets are being analyzed, this can be considerably easier
to write. Also it facilitates column indexing by simply passing a subset
of the data (e.g., `data[, 1:10]`) and using the ‘.’ expansion to
analyze the first 10 columns. The examples section demonstrate this use.

Also noteworthy is that `SEMSummary` is not really meant to be used on
its own. It is the computational workhorse, but it is meant to be used
with a styling or printing method to produce simple output. `APAStyler`
has methods for `SEMSummary` output.

There are several new ways to handle missing data now including listwise
deletion, pairwise deletion, and using the EM algorithm, the default.

## See also

[`APAStyler`](https://joshuawiley.com/JWileymisc/reference/APAStyler.md)

## Examples

``` r
## Example using the built in iris dataset
s <- SEMSummary(~ Sepal.Length + Sepal.Width + Petal.Length, data = iris)
s # show output ... not very nice
#> $names
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length"
#> 
#> $n
#> [1] 150
#> 
#> $nmissing
#> Sepal.Length  Sepal.Width Petal.Length 
#>            0            0            0 
#> 
#> $mu
#> Sepal.Length  Sepal.Width Petal.Length 
#>     5.843333     3.057333     3.758000 
#> 
#> $stdev
#> Sepal.Length  Sepal.Width Petal.Length 
#>    0.8280661    0.4358663    1.7652982 
#> 
#> $Sigma
#>              Sepal.Length Sepal.Width Petal.Length
#> Sepal.Length    0.6856935  -0.0424340    1.2743154
#> Sepal.Width    -0.0424340   0.1899794   -0.3296564
#> Petal.Length    1.2743154  -0.3296564    3.1162779
#> 
#> $sSigma
#>              Sepal.Length Sepal.Width Petal.Length
#> Sepal.Length    1.0000000  -0.1175698    0.8717538
#> Sepal.Width    -0.1175698   1.0000000   -0.4284401
#> Petal.Length    0.8717538  -0.4284401    1.0000000
#> 
#> $coverage
#>              Sepal.Length Sepal.Width Petal.Length
#> Sepal.Length            1           1            1
#> Sepal.Width             1           1            1
#> Petal.Length            1           1            1
#> 
#> $pvalue
#>              Sepal.Length  Sepal.Width Petal.Length
#> Sepal.Length           NA 1.518983e-01 0.000000e+00
#> Sepal.Width     0.1518983           NA 4.513314e-08
#> Petal.Length    0.0000000 4.513314e-08           NA
#> 
#> attr(,"class")
#> [1] "SEMSummary"

## Prettier output from SEMSummary
APAStyler(s)
#>                 N   M    SD   1.  2.    3.   
#> 1. Sepal.Length 150 5.84 0.83  -  -0.04  1.27
#> 2. Sepal.Width  150 3.06 0.44      -    -0.33
#> 3. Petal.Length 150 3.76 1.77            -   
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>              Sepal.Length Sepal.Width Petal.Length
#> Sepal.Length 1            1           1           
#> Sepal.Width               1           1           
#> Petal.Length                          1           

#### Subset the dataset and use the . expansion ####

## summary for all variables in mtcars data set
## with 11 variables, this could be a pain to write out
SEMSummary(~ ., data = mtcars)
#> $names
#>  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
#> [11] "carb"
#> 
#> $n
#> [1] 32
#> 
#> $nmissing
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>    0    0    0    0    0    0    0    0    0    0    0 
#> 
#> $mu
#>        mpg        cyl       disp         hp       drat         wt       qsec 
#>  20.090625   6.187500 230.721875 146.687500   3.596563   3.217250  17.848750 
#>         vs         am       gear       carb 
#>   0.437500   0.406250   3.687500   2.812500 
#> 
#> $stdev
#>         mpg         cyl        disp          hp        drat          wt 
#>   6.0269481   1.7859216 123.9386938  68.5628685   0.5346787   0.9784574 
#>        qsec          vs          am        gear        carb 
#>   1.7869432   0.5040161   0.4989909   0.7378041   1.6152000 
#> 
#> $Sigma
#>              mpg         cyl        disp          hp         drat          wt
#> mpg    36.324103  -9.1723790  -633.09721 -320.732056   2.19506351  -5.1166847
#> cyl    -9.172379   3.1895161   199.66028  101.931452  -0.66836694   1.3673710
#> disp -633.097208 199.6602823 15360.79983 6721.158669 -47.06401915 107.6842040
#> hp   -320.732056 101.9314516  6721.15867 4700.866935 -16.45110887  44.1926613
#> drat    2.195064  -0.6683669   -47.06402  -16.451109   0.28588135  -0.3727207
#> wt     -5.116685   1.3673710   107.68420   44.192661  -0.37272073   0.9573790
#> qsec    4.509149  -1.8868548   -96.05168  -86.770081   0.08714073  -0.3054816
#> vs      2.017137  -0.7298387   -44.37762  -24.987903   0.11864919  -0.2736613
#> am      1.803931  -0.4657258   -36.56401   -8.320565   0.19015121  -0.3381048
#> gear    2.135685  -0.6491935   -50.80262   -6.358871   0.27598790  -0.4210806
#> carb   -5.363105   1.5201613    79.06875   83.036290  -0.07840726   0.6757903
#>              qsec           vs           am        gear        carb
#> mpg    4.50914919   2.01713710   1.80393145   2.1356855 -5.36310484
#> cyl   -1.88685484  -0.72983871  -0.46572581  -0.6491935  1.52016129
#> disp -96.05168145 -44.37762097 -36.56401210 -50.8026210 79.06875000
#> hp   -86.77008065 -24.98790323  -8.32056452  -6.3588710 83.03629032
#> drat   0.08714073   0.11864919   0.19015121   0.2759879 -0.07840726
#> wt    -0.30548161  -0.27366129  -0.33810484  -0.4210806  0.67579032
#> qsec   3.19316613   0.67056452  -0.20495968  -0.2804032 -1.89411290
#> vs     0.67056452   0.25403226   0.04233871   0.0766129 -0.46370968
#> am    -0.20495968   0.04233871   0.24899194   0.2923387  0.04637097
#> gear  -0.28040323   0.07661290   0.29233871   0.5443548  0.32661290
#> carb  -1.89411290  -0.46370968   0.04637097   0.3266129  2.60887097
#> 
#> $sSigma
#>             mpg        cyl       disp         hp        drat         wt
#> mpg   1.0000000 -0.8521620 -0.8475514 -0.7761684  0.68117191 -0.8676594
#> cyl  -0.8521620  1.0000000  0.9020329  0.8324475 -0.69993811  0.7824958
#> disp -0.8475514  0.9020329  1.0000000  0.7909486 -0.71021393  0.8879799
#> hp   -0.7761684  0.8324475  0.7909486  1.0000000 -0.44875912  0.6587479
#> drat  0.6811719 -0.6999381 -0.7102139 -0.4487591  1.00000000 -0.7124406
#> wt   -0.8676594  0.7824958  0.8879799  0.6587479 -0.71244065  1.0000000
#> qsec  0.4186840 -0.5912421 -0.4336979 -0.7082234  0.09120476 -0.1747159
#> vs    0.6640389 -0.8108118 -0.7104159 -0.7230967  0.44027846 -0.5549157
#> am    0.5998324 -0.5226070 -0.5912270 -0.2432043  0.71271113 -0.6924953
#> gear  0.4802848 -0.4926866 -0.5555692 -0.1257043  0.69961013 -0.5832870
#> carb -0.5509251  0.5269883  0.3949769  0.7498125 -0.09078980  0.4276059
#>             qsec         vs          am       gear        carb
#> mpg   0.41868403  0.6640389  0.59983243  0.4802848 -0.55092507
#> cyl  -0.59124207 -0.8108118 -0.52260705 -0.4926866  0.52698829
#> disp -0.43369788 -0.7104159 -0.59122704 -0.5555692  0.39497686
#> hp   -0.70822339 -0.7230967 -0.24320426 -0.1257043  0.74981247
#> drat  0.09120476  0.4402785  0.71271113  0.6996101 -0.09078980
#> wt   -0.17471588 -0.5549157 -0.69249526 -0.5832870  0.42760594
#> qsec  1.00000000  0.7445354 -0.22986086 -0.2126822 -0.65624923
#> vs    0.74453544  1.0000000  0.16834512  0.2060233 -0.56960714
#> am   -0.22986086  0.1683451  1.00000000  0.7940588  0.05753435
#> gear -0.21268223  0.2060233  0.79405876  1.0000000  0.27407284
#> carb -0.65624923 -0.5696071  0.05753435  0.2740728  1.00000000
#> 
#> $coverage
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> mpg    1   1    1  1    1  1    1  1  1    1    1
#> cyl    1   1    1  1    1  1    1  1  1    1    1
#> disp   1   1    1  1    1  1    1  1  1    1    1
#> hp     1   1    1  1    1  1    1  1  1    1    1
#> drat   1   1    1  1    1  1    1  1  1    1    1
#> wt     1   1    1  1    1  1    1  1  1    1    1
#> qsec   1   1    1  1    1  1    1  1  1    1    1
#> vs     1   1    1  1    1  1    1  1  1    1    1
#> am     1   1    1  1    1  1    1  1  1    1    1
#> gear   1   1    1  1    1  1    1  1  1    1    1
#> carb   1   1    1  1    1  1    1  1  1    1    1
#> 
#> $pvalue
#>               mpg          cyl         disp           hp         drat
#> mpg            NA 6.112687e-10 9.380327e-10 1.787835e-07 1.776240e-05
#> cyl  6.112687e-10           NA 1.803002e-12 3.477861e-09 8.244636e-06
#> disp 9.380327e-10 1.803002e-12           NA 7.142679e-08 5.282022e-06
#> hp   1.787835e-07 3.477861e-09 7.142679e-08           NA 9.988772e-03
#> drat 1.776240e-05 8.244636e-06 5.282022e-06 9.988772e-03           NA
#> wt   1.293959e-10 1.217567e-07 1.222311e-11 4.145827e-05 4.784260e-06
#> qsec 1.708199e-02 3.660533e-04 1.314404e-02 5.766253e-06 6.195826e-01
#> vs   3.415937e-05 1.843018e-08 5.235012e-06 2.940896e-06 1.167553e-02
#> am   2.850207e-04 2.151207e-03 3.662114e-04 1.798309e-01 4.726790e-06
#> gear 5.400948e-03 4.173297e-03 9.635921e-04 4.930119e-01 8.360110e-06
#> carb 1.084446e-03 1.942340e-03 2.526789e-02 7.827810e-07 6.211834e-01
#>                wt         qsec           vs           am         gear
#> mpg  1.293959e-10 1.708199e-02 3.415937e-05 2.850207e-04 5.400948e-03
#> cyl  1.217567e-07 3.660533e-04 1.843018e-08 2.151207e-03 4.173297e-03
#> disp 1.222311e-11 1.314404e-02 5.235012e-06 3.662114e-04 9.635921e-04
#> hp   4.145827e-05 5.766253e-06 2.940896e-06 1.798309e-01 4.930119e-01
#> drat 4.784260e-06 6.195826e-01 1.167553e-02 4.726790e-06 8.360110e-06
#> wt             NA 3.388683e-01 9.798492e-04 1.125440e-05 4.586601e-04
#> qsec 3.388683e-01           NA 1.029669e-06 2.056621e-01 2.425344e-01
#> vs   9.798492e-04 1.029669e-06           NA 3.570439e-01 2.579439e-01
#> am   1.125440e-05 2.056621e-01 3.570439e-01           NA 5.834043e-08
#> gear 4.586601e-04 2.425344e-01 2.579439e-01 5.834043e-08           NA
#> carb 1.463861e-02 4.536949e-05 6.670496e-04 7.544526e-01 1.290291e-01
#>              carb
#> mpg  1.084446e-03
#> cyl  1.942340e-03
#> disp 2.526789e-02
#> hp   7.827810e-07
#> drat 6.211834e-01
#> wt   1.463861e-02
#> qsec 4.536949e-05
#> vs   6.670496e-04
#> am   7.544526e-01
#> gear 1.290291e-01
#> carb           NA
#> 
#> attr(,"class")
#> [1] "SEMSummary"

## . expansion is also useful when we know column positions
## but not necessarily names
SEMSummary(~ ., data = mtcars[, c(1, 2, 3, 9, 10, 11)])
#> $names
#> [1] "mpg"  "cyl"  "disp" "am"   "gear" "carb"
#> 
#> $n
#> [1] 32
#> 
#> $nmissing
#>  mpg  cyl disp   am gear carb 
#>    0    0    0    0    0    0 
#> 
#> $mu
#>       mpg       cyl      disp        am      gear      carb 
#>  20.09062   6.18750 230.72188   0.40625   3.68750   2.81250 
#> 
#> $stdev
#>         mpg         cyl        disp          am        gear        carb 
#>   6.0269481   1.7859216 123.9386938   0.4989909   0.7378041   1.6152000 
#> 
#> $Sigma
#>              mpg         cyl        disp           am        gear        carb
#> mpg    36.324103  -9.1723790  -633.09721   1.80393145   2.1356855 -5.36310484
#> cyl    -9.172379   3.1895161   199.66028  -0.46572581  -0.6491935  1.52016129
#> disp -633.097208 199.6602823 15360.79983 -36.56401210 -50.8026210 79.06875000
#> am      1.803931  -0.4657258   -36.56401   0.24899194   0.2923387  0.04637097
#> gear    2.135685  -0.6491935   -50.80262   0.29233871   0.5443548  0.32661290
#> carb   -5.363105   1.5201613    79.06875   0.04637097   0.3266129  2.60887097
#> 
#> $sSigma
#>             mpg        cyl       disp          am       gear        carb
#> mpg   1.0000000 -0.8521620 -0.8475514  0.59983243  0.4802848 -0.55092507
#> cyl  -0.8521620  1.0000000  0.9020329 -0.52260705 -0.4926866  0.52698829
#> disp -0.8475514  0.9020329  1.0000000 -0.59122704 -0.5555692  0.39497686
#> am    0.5998324 -0.5226070 -0.5912270  1.00000000  0.7940588  0.05753435
#> gear  0.4802848 -0.4926866 -0.5555692  0.79405876  1.0000000  0.27407284
#> carb -0.5509251  0.5269883  0.3949769  0.05753435  0.2740728  1.00000000
#> 
#> $coverage
#>      mpg cyl disp am gear carb
#> mpg    1   1    1  1    1    1
#> cyl    1   1    1  1    1    1
#> disp   1   1    1  1    1    1
#> am     1   1    1  1    1    1
#> gear   1   1    1  1    1    1
#> carb   1   1    1  1    1    1
#> 
#> $pvalue
#>               mpg          cyl         disp           am         gear
#> mpg            NA 6.112687e-10 9.380327e-10 2.850207e-04 5.400948e-03
#> cyl  6.112687e-10           NA 1.803002e-12 2.151207e-03 4.173297e-03
#> disp 9.380327e-10 1.803002e-12           NA 3.662114e-04 9.635921e-04
#> am   2.850207e-04 2.151207e-03 3.662114e-04           NA 5.834043e-08
#> gear 5.400948e-03 4.173297e-03 9.635921e-04 5.834043e-08           NA
#> carb 1.084446e-03 1.942340e-03 2.526789e-02 7.544526e-01 1.290291e-01
#>             carb
#> mpg  0.001084446
#> cyl  0.001942340
#> disp 0.025267886
#> am   0.754452554
#> gear 0.129029084
#> carb          NA
#> 
#> attr(,"class")
#> [1] "SEMSummary"

## clean up
rm(s)

## sample data
Xmiss <- as.matrix(iris[, -5])
# make q0% missing completely at random
set.seed(10)
Xmiss[sample(length(Xmiss), length(Xmiss) * .10)] <- NA
Xmiss <- as.data.frame(Xmiss)

SEMSummary(~ ., data = Xmiss, use = "fiml")
#> $names
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> 
#> $n
#> [1] 150
#> 
#> $nmissing
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>           15            8           21           16 
#> 
#> $mu
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>     5.859048     3.063777     3.744655     1.199088 
#> 
#> $stdev
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>    0.8315335    0.4326462    1.7393932    0.7630092 
#> 
#> $Sigma
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length   0.69144798 -0.04277193     1.255775   0.5177624
#> Sepal.Width   -0.04277193  0.18718270    -0.325356  -0.1222384
#> Petal.Length   1.25577465 -0.32535602     3.025489   1.2796561
#> Petal.Width    0.51776245 -0.12223835     1.279656   0.5821831
#> 
#> $sSigma
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000  -0.1188902    0.8682288   0.8160580
#> Sepal.Width    -0.1188902   1.0000000   -0.4323428  -0.3702924
#> Petal.Length    0.8682288  -0.4323428    1.0000000   0.9641969
#> Petal.Width     0.8160580  -0.3702924    0.9641969   1.0000000
#> 
#> $coverage
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    0.9000000   0.8466667    0.7733333   0.8000000
#> Sepal.Width     0.8466667   0.9466667    0.8266667   0.8400000
#> Petal.Length    0.7733333   0.8266667    0.8600000   0.7600000
#> Petal.Width     0.8000000   0.8400000    0.7600000   0.8933333
#> 
#> $pvalue
#>              Sepal.Length  Sepal.Width Petal.Length  Petal.Width
#> Sepal.Length           NA 1.830888e-01 0.000000e+00 0.000000e+00
#> Sepal.Width     0.1830888           NA 5.321816e-07 1.973922e-05
#> Petal.Length    0.0000000 5.321816e-07           NA 0.000000e+00
#> Petal.Width     0.0000000 1.973922e-05 0.000000e+00           NA
#> 
#> attr(,"class")
#> [1] "SEMSummary"


## pairwise
APAStyler(SEMSummary(~ ., data = Xmiss, use = "pair"),
  type = "cor")
#>                 N   M    SD   1.  2.    3.    4.   
#> 1. Sepal.Length 135 5.89 0.81  -  -0.18  0.86  0.81
#> 2. Sepal.Width  142 3.07 0.44      -    -0.43 -0.43
#> 3. Petal.Length 129 3.82 1.74            -     0.97
#> 4. Petal.Width  134 1.19 0.77                  -   
#> 
#> Percentage of coverage for each pairwise covariance or correlation
#> 
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length 0.9          0.85        0.77         0.8        
#> Sepal.Width               0.95        0.83         0.84       
#> Petal.Length                          0.86         0.76       
#> Petal.Width                                        0.89       

## same as cor()
cor(Xmiss, use = "pairwise.complete.obs")
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000  -0.1832230    0.8646983   0.8119316
#> Sepal.Width    -0.1832230   1.0000000   -0.4291999  -0.4293565
#> Petal.Length    0.8646983  -0.4291999    1.0000000   0.9650212
#> Petal.Width     0.8119316  -0.4293565    0.9650212   1.0000000

## complete cases only
SEMSummary(~ ., data = Xmiss, use = "comp")
#> $names
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
#> 
#> $n
#> [1] 150
#> 
#> $nmissing
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>           15            8           21           16 
#> 
#> $mu
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>     5.853608     3.037113     3.802062     1.229897 
#> 
#> $stdev
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>    0.7987049    0.4194042    1.7165845    0.7479367 
#> 
#> $Sigma
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length   0.63792955 -0.05919781    1.1747841   0.4824431
#> Sepal.Width   -0.05919781  0.17589991   -0.3682023  -0.1415378
#> Petal.Length   1.17478415 -0.36820232    2.9466624   1.2399377
#> Petal.Width    0.48244308 -0.14153780    1.2399377   0.5594094
#> 
#> $sSigma
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000  -0.1767203    0.8568534   0.8075973
#> Sepal.Width    -0.1767203   1.0000000   -0.5114327  -0.4512059
#> Petal.Length    0.8568534  -0.5114327    1.0000000   0.9657613
#> Petal.Width     0.8075973  -0.4512059    0.9657613   1.0000000
#> 
#> $coverage
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    0.9000000   0.8466667    0.7733333   0.8000000
#> Sepal.Width     0.8466667   0.9466667    0.8266667   0.8400000
#> Petal.Length    0.7733333   0.8266667    0.8600000   0.7600000
#> Petal.Width     0.8000000   0.8400000    0.7600000   0.8933333
#> 
#> $pvalue
#>              Sepal.Length  Sepal.Width Petal.Length  Petal.Width
#> Sepal.Length           NA 4.686510e-02 0.000000e+00 0.000000e+00
#> Sepal.Width     0.0468651           NA 1.278788e-09 1.142829e-07
#> Petal.Length    0.0000000 1.278788e-09           NA 0.000000e+00
#> Petal.Width     0.0000000 1.142829e-07 0.000000e+00           NA
#> 
#> attr(,"class")
#> [1] "SEMSummary"

## clean up
rm(Xmiss)
```

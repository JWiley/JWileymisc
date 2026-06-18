# Test the distribution of a variable against a specific distribution

Function designed to help examine distributions. It also includes an
option for assessing multivariate normality using the (squared)
Mahalanobis distance. A generic function, some methods, and constructor
(`as.testDistribution`) and function to check class
(`is.testDistribution`) also are provided.

Note that for the `use` argument, several options are possible. By
default it is “complete.obs”, which uses only cases with complete data
on all variables. Another option is “pairwise.complete.obs”, which uses
all available data for each variable indivdiually to estimate the means
and variances, and all pairwise complete observation pairs for each
covariance. Because the same cases are not used for all estimates, it is
possible to obtain a covariance matrix that is not positive definite
(e.g., correlations \> +1 or \< -1).

Finally, the last option is “fiml”, which uses full information maximum
likelihood estimates of the means and covariance matrix. Depending on
the number of cases, missing data patterns, and variables, this may be
quite slow and computationally demanding.

The `robust` argument determines whether to use robust estimates or not
when calculating densities, etc. By default it is `FALSE`, but if `TRUE`
and a univariate or multivariate normal distribution is tested, then
robust estimates of the means and covariance matrix (a variance if
univariate) will be used based on `covMcd` from the robustbase package.

## Usage

``` r
testDistribution(x, ...)

as.testDistribution(x)

is.testDistribution(x)

# Default S3 method
testDistribution(
  x,
  distr = c("normal", "beta", "chisq", "f", "gamma", "geometric", "nbinom", "poisson",
    "uniform", "mvnormal"),
  na.rm = TRUE,
  starts,
  extremevalues = c("no", "theoretical", "empirical"),
  ev.perc = 0.001,
  use = c("complete.obs", "pairwise.complete.obs", "fiml"),
  robust = FALSE,
  ...
)
```

## Arguments

- x:

  The data as a single variable or vector to check the distribution
  unless the distribution is “mvnormal” in which case it should be a
  data frame or data table.

- ...:

  Additional arguments. If these include mu and sigma and the
  distribution is multivariate normal, then it will use the passed
  values instead of calculating the mean and covariances of the data.

- distr:

  A character string indicating the distribution to be tested. Currently
  one of: “normal”, “beta”, “chisq” (chi-squared), “f”, “gamma”,
  “geometric”, “nbinom” (negative binomial), “poisson”, “uniform”, or
  “mvnormal” for multivariate normal where Mahalanobis distances are
  calculated and compared against a Chi-squared distribution with
  degrees of freedom equal to the number of variables.

- na.rm:

  A logical value whether to omit missing values. Defaults to `TRUE`.

- starts:

  A named list of the starting values. Not required for all
  distributions. Passed on to `fitdistr` which fits the maximum
  likelihood estimates of the distribution parameters.

- extremevalues:

  A character vector whether to indicate extreme values. Should be “no”
  to do nothing, “empirical” to show extreme values based on the
  observed data percentiles, or “theoretical” to show extreme values
  based on percentiles of the theoretical distribution.

- ev.perc:

  Percentile to use for extreme values. For example if .01, then the
  lowest 1 percent and highest 1 percent will be labelled extreme
  values. Defaults to the lowest and highest 0.1 percent.

- use:

  A character vector indicating how the moments (means and covariance
  matrix) should be estimated in the presence of missing data when
  `distr = mvnormal`. The default is to use complete observations, but
  full information maximum likelihood based on functions in lavaan is
  also available. See details.

- robust:

  A logical whether to use robust estimation or not. Currently only
  applies to normally distributed data (univariate or multivariate).
  Also, when `robust = TRUE`, only complete observations are used (i.e.,
  `use = "complete.obs"`). See details.

## Value

A logical whether or not an object is of class `testDistribution` or an
object of the same class.

A list with information about the distribution (parameter estimates,
name, log likelihood (useful for comparing the fit of different
distributions to the data), and a dataset with the sorted data and
theoretical quantiles.

## See also

[`SEMSummary`](https://joshuawiley.com/JWileymisc/reference/SEMSummary.md)

## Examples

``` r

## example data
set.seed(1234)
d <- data.table::data.table(
  Ynorm = rnorm(200),
  Ybeta = rbeta(200, 1, 4),
  Ychisq = rchisq(200, 8),
  Yf = rf(200, 5, 10),
  Ygamma = rgamma(200, 2, 2),
  Ynbinom = rnbinom(200, mu = 4, size = 9),
  Ypois = rpois(200, 4))

## testing and graphing
testDistribution(d$Ybeta, "beta", starts = list(shape1 = 1, shape2 = 4))
#> $Data
#>                 X           Y OriginalOrder   isEV    YDeviates
#>             <num>       <num>         <int> <fctr>        <num>
#>   1: 0.0009548787 0.002899638            20     No  0.001944759
#>   2: 0.0026222465 0.006542140           122     No  0.003919893
#>   3: 0.0041999262 0.007116305           124     No  0.002916379
#>   4: 0.0057324381 0.007488806           119     No  0.001756368
#>   5: 0.0072360495 0.008381499             7     No  0.001145450
#>  ---                                                           
#> 196: 0.5896978140 0.658553356           192     No  0.068855542
#> 197: 0.6125406443 0.680790719           171     No  0.068250075
#> 198: 0.6411187285 0.689096550           175     No  0.047977822
#> 199: 0.6804840872 0.694824897           181     No  0.014340810
#> 200: 0.7510216428 0.697959189           198     No -0.053062454
#> 
#> $Distribution
#> $Distribution$d
#> function (x, shape1, shape2, ncp = 0, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_dbeta, x, shape1, shape2, log)
#>     else .Call(C_dnbeta, x, shape1, shape2, ncp, log)
#> }
#> <bytecode: 0x55a18cba4308>
#> <environment: namespace:stats>
#> 
#> $Distribution$q
#> function (p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_qbeta, p, shape1, shape2, lower.tail, log.p)
#>     else .Call(C_qnbeta, p, shape1, shape2, ncp, lower.tail, 
#>         log.p)
#> }
#> <bytecode: 0x55a18cba7530>
#> <environment: namespace:stats>
#> 
#> $Distribution$Name
#> [1] "Beta"
#> 
#> $Distribution$fit
#>      shape1       shape2  
#>   1.09047254   4.43181099 
#>  (0.09708525) (0.46533971)
#> 
#> $Distribution$LL
#> 'log Lik.' 131.529 (df=2)
#> 
#> 
#> $EVLimits
#> [1] -Inf  Inf
#> 
#> $NOK
#> [1] 200
#> 
#> $distr
#> [1] "beta"
#> 
#> $na.rm
#> [1] TRUE
#> 
#> $extremevalues
#> [1] "no"
#> 
#> $ev.perc
#> [1] 0.001
#> 
#> $use
#> [1] "complete.obs"
#> 
#> $robust
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "testDistribution"
testDistribution(d$Ychisq, "chisq", starts = list(df = 8))
#> $Data
#>              X         Y OriginalOrder   isEV   YDeviates
#>          <num>     <num>         <int> <fctr>       <num>
#>   1:  1.020806  1.119129            75     No  0.09832363
#>   2:  1.409747  1.386369            54     No -0.02337731
#>   3:  1.647133  1.476380            78     No -0.17075244
#>   4:  1.829535  1.785484           149     No -0.04405142
#>   5:  1.981898  1.948253            56     No -0.03364471
#>  ---                                                     
#> 196: 17.443427 19.520193            34     No  2.07676616
#> 197: 18.145695 19.541868           154     No  1.39617310
#> 198: 19.071980 20.585244            55     No  1.51326441
#> 199: 20.451772 20.984934           170     No  0.53316198
#> 200: 23.332634 24.064860            12     No  0.73222569
#> 
#> $Distribution
#> $Distribution$d
#> function (x, df, ncp = 0, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_dchisq, x, df, log)
#>     else .Call(C_dnchisq, x, df, ncp, log)
#> }
#> <bytecode: 0x55a18af14b40>
#> <environment: namespace:stats>
#> 
#> $Distribution$q
#> function (p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_qchisq, p, df, lower.tail, log.p)
#>     else .Call(C_qnchisq, p, df, ncp, lower.tail, log.p)
#> }
#> <bytecode: 0x55a18af17e10>
#> <environment: namespace:stats>
#> 
#> $Distribution$Name
#> [1] "Chi-squared"
#> 
#> $Distribution$fit
#>       df    
#>   7.7424489 
#>  (0.2605917)
#> 
#> $Distribution$LL
#> 'log Lik.' -544.7285 (df=1)
#> 
#> 
#> $EVLimits
#> [1] -Inf  Inf
#> 
#> $NOK
#> [1] 200
#> 
#> $distr
#> [1] "chisq"
#> 
#> $na.rm
#> [1] TRUE
#> 
#> $extremevalues
#> [1] "no"
#> 
#> $ev.perc
#> [1] 0.001
#> 
#> $use
#> [1] "complete.obs"
#> 
#> $robust
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "testDistribution"

## for chi-square distribution, extreme values only on
## the right tail
testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
  extremevalues = "empirical", ev.perc = .1)
#> $Data
#>              X         Y OriginalOrder   isEV   YDeviates
#>          <num>     <num>         <int> <fctr>       <num>
#>   1:  1.020806  1.119129            75     No  0.09832363
#>   2:  1.409747  1.386369            54     No -0.02337731
#>   3:  1.647133  1.476380            78     No -0.17075244
#>   4:  1.829535  1.785484           149     No -0.04405142
#>   5:  1.981898  1.948253            56     No -0.03364471
#>  ---                                                     
#> 196: 17.443427 19.520193            34    Yes  2.07676616
#> 197: 18.145695 19.541868           154    Yes  1.39617310
#> 198: 19.071980 20.585244            55    Yes  1.51326441
#> 199: 20.451772 20.984934           170    Yes  0.53316198
#> 200: 23.332634 24.064860            12    Yes  0.73222569
#> 
#> $Distribution
#> $Distribution$d
#> function (x, df, ncp = 0, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_dchisq, x, df, log)
#>     else .Call(C_dnchisq, x, df, ncp, log)
#> }
#> <bytecode: 0x55a18af14b40>
#> <environment: namespace:stats>
#> 
#> $Distribution$q
#> function (p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_qchisq, p, df, lower.tail, log.p)
#>     else .Call(C_qnchisq, p, df, ncp, lower.tail, log.p)
#> }
#> <bytecode: 0x55a18af17e10>
#> <environment: namespace:stats>
#> 
#> $Distribution$Name
#> [1] "Chi-squared"
#> 
#> $Distribution$fit
#>       df    
#>   7.7424489 
#>  (0.2605917)
#> 
#> $Distribution$LL
#> 'log Lik.' -544.7285 (df=1)
#> 
#> 
#> $EVLimits
#>               90% 
#>     -Inf 13.60881 
#> 
#> $NOK
#> [1] 200
#> 
#> $distr
#> [1] "chisq"
#> 
#> $na.rm
#> [1] TRUE
#> 
#> $extremevalues
#> [1] "empirical"
#> 
#> $ev.perc
#> [1] 0.1
#> 
#> $use
#> [1] "complete.obs"
#> 
#> $robust
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "testDistribution"
testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
  extremevalues = "theoretical", ev.perc = .1)
#> $Data
#>              X         Y OriginalOrder   isEV   YDeviates
#>          <num>     <num>         <int> <fctr>       <num>
#>   1:  1.020806  1.119129            75     No  0.09832363
#>   2:  1.409747  1.386369            54     No -0.02337731
#>   3:  1.647133  1.476380            78     No -0.17075244
#>   4:  1.829535  1.785484           149     No -0.04405142
#>   5:  1.981898  1.948253            56     No -0.03364471
#>  ---                                                     
#> 196: 17.443427 19.520193            34    Yes  2.07676616
#> 197: 18.145695 19.541868           154    Yes  1.39617310
#> 198: 19.071980 20.585244            55    Yes  1.51326441
#> 199: 20.451772 20.984934           170    Yes  0.53316198
#> 200: 23.332634 24.064860            12    Yes  0.73222569
#> 
#> $Distribution
#> $Distribution$d
#> function (x, df, ncp = 0, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_dchisq, x, df, log)
#>     else .Call(C_dnchisq, x, df, ncp, log)
#> }
#> <bytecode: 0x55a18af14b40>
#> <environment: namespace:stats>
#> 
#> $Distribution$q
#> function (p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_qchisq, p, df, lower.tail, log.p)
#>     else .Call(C_qnchisq, p, df, ncp, lower.tail, log.p)
#> }
#> <bytecode: 0x55a18af17e10>
#> <environment: namespace:stats>
#> 
#> $Distribution$Name
#> [1] "Chi-squared"
#> 
#> $Distribution$fit
#>       df    
#>   7.7424489 
#>  (0.2605917)
#> 
#> $Distribution$LL
#> 'log Lik.' -544.7285 (df=1)
#> 
#> 
#> $EVLimits
#> [1]  0.00000 13.01761
#> 
#> $NOK
#> [1] 200
#> 
#> $distr
#> [1] "chisq"
#> 
#> $na.rm
#> [1] TRUE
#> 
#> $extremevalues
#> [1] "theoretical"
#> 
#> $ev.perc
#> [1] 0.1
#> 
#> $use
#> [1] "complete.obs"
#> 
#> $robust
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "testDistribution"

if (FALSE) { # \dontrun{

testDistribution(d$Yf, "uniform")
testDistribution(d$Ypois, "geometric")

testDistribution(d$Yf, "f", starts = list(df1 = 5, df2 = 10))
testDistribution(d$Ygamma, "gamma")
testDistribution(d$Ynbinom, "poisson")
testDistribution(d$Ynbinom, "nbinom")
testDistribution(d$Ypois, "poisson")

## compare log likelihood of two different distributions
testDistribution(d$Ygamma, "normal")$Distribution$LL
testDistribution(d$Ygamma, "gamma")$Distribution$LL

testDistribution(d$Ynorm, "normal")
testDistribution(c(d$Ynorm, 10, 1000), "normal",
  extremevalues = "theoretical")
testDistribution(c(d$Ynorm, 10, 1000), "normal",
  extremevalues = "theoretical", robust = TRUE)

testDistribution(mtcars, "mvnormal")

## for multivariate normal mahalanobis distance
## which follows a chi-square distribution, extreme values only on
## the right tail
testDistribution(mtcars, "mvnormal", extremevalues = "empirical",
  ev.perc = .1)
testDistribution(mtcars, "mvnormal", extremevalues = "theoretical",
  ev.perc = .1)

rm(d) ## cleanup
} # }
```

# Function to format the reuslts of a hypothesis test as text

Function to format the reuslts of a hypothesis test as text

## Usage

``` r
formatHtest(
  x,
  type = c("t", "F", "chisq", "kw", "mh", "r_pearson", "r_kendall", "r_spearman"),
  ...
)
```

## Arguments

- x:

  A `htest` class object

- type:

  The type of htest. Currently one of: “t”, “F”, “chisq”, “kw”, “mh”,
  “r_pearson”, “r_kendall”, or “r_spearman” for t-tests, F-tests,
  chi-square tests, kruskal-wallis tests, Mantel-Haenszel tests, pearson
  correlations, kendall tau correlation, and spearman rho correlation,
  respectively.

- ...:

  Arguments passed on to p-value formatting

## Value

A character string with results

## Examples

``` r
formatHtest(t.test(extra ~ group, data = sleep), type = "t")
#> [1] "t(df = 17.78) = -1.86, p = .079"
formatHtest(anova(aov(mpg ~ factor(cyl), data = mtcars)), type = "F")
#> [1] "F(2, 29) = 39.70, p < .001"
formatHtest(chisq.test(c(A = 20, B = 15, C = 25)), type = "chisq")
#> [1] "Chi-square(df = 2) = 2.5, p = .287"
formatHtest(kruskal.test(Ozone ~ Month, data = airquality), type = "kw")
#> [1] "Kruskal-Wallis chi-square(df = 4) = 29.27, p < .001"
formatHtest(mantelhaen.test(UCBAdmissions), type = "mh")
#> [1] "Mantel-Haenszel chi-square(df = 1) = 1.43, p = .232, common odds ratio = 0.90, CI = (0.77, 1.06)."
formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "pearson"), type = "r_pearson")
#> [1] "r = -0.78, CI = (-0.89, -0.59), t(df = 30) = -6.74, p < .001"
formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "kendall"), type = "r_kendall")
#> Warning: cannot compute exact p-value with ties
#> [1] "tau = -0.74, z = -5.87, p < .001"
formatHtest(cor.test(~ mpg + hp, data = mtcars, method = "spearman"), type = "r_spearman")
#> Warning: cannot compute exact p-value with ties
#> [1] "rho = -0.89, S = 10337.3, p < .001"
```

# Function makes nice tables

Give a dataset and a list of variables, or just the data in the vars.
For best results, convert categorical variables into factors. Provides a
table of estimated descriptive statistics optionally by group levels.

## Usage

``` r
egltable(
  vars,
  g,
  data,
  idvar,
  strict = TRUE,
  parametric = TRUE,
  paired = FALSE,
  simChisq = FALSE,
  sims = 1000000L
)
```

## Arguments

- vars:

  Either an index (numeric or character) of variables to access from the
  `data` argument, or the data to be described itself.

- g:

  A variable used tou group/separate the data prior to calculating
  descriptive statistics.

- data:

  optional argument of the dataset containing the variables to be
  described.

- idvar:

  A character string indicating the variable name of the ID variable.
  Not currently used, but will eventually support `egltable` supporting
  repeated measures data.

- strict:

  Logical, whether to strictly follow the type of each variable, or to
  assume categorical if the number of unique values is less than or
  equal to 3.

- parametric:

  Logical whether to use parametric tests in the case of multiple groups
  to test for differences. Only applies to continuous variables. If
  `TRUE`, the default, uses one-way ANOVA, and a F test. If `FALSE`,
  uses the Kruskal-Wallis test.

- paired:

  Logical whether the data are paired or not. Defaults to `FALSE`. If
  `TRUE`, the grouping variable, `g`, must have two levels and `idvar`
  must be specified. When used a paired t-test is used for parametric,
  continuous data and a Wilcoxon test for paired non parametric,
  continuous data and a McNemar chi square test is used for categorical
  data.

- simChisq:

  Logical whether to estimate p-values for chi-square test for
  categorical data when there are multiple groups, by simulation.
  Defaults to `FALSE`. Useful when there are small cells as will provide
  a more accurate test in extreme cases, similar to Fisher Exact Test
  but generalizing to large dimension of tables.

- sims:

  Integer for the number of simulations to be used to estimate p-values
  for the chi-square tests for categorical variables when there are
  multiple groups. Defaults to one million (`1e6L`).

## Value

A data frame of the table.

## Examples

``` r
egltable(iris)
#>                 M (SD)/N (%)
#>          <char>       <char>
#> 1: Sepal.Length  5.84 (0.83)
#> 2:  Sepal.Width  3.06 (0.44)
#> 3: Petal.Length  3.76 (1.77)
#> 4:  Petal.Width  1.20 (0.76)
#> 5:      Species             
#> 6:       setosa   50 (33.3%)
#> 7:   versicolor   50 (33.3%)
#> 8:    virginica   50 (33.3%)
egltable(colnames(iris)[1:4], "Species", data = iris)
#>                 setosa M (SD) versicolor M (SD) virginica M (SD)
#>          <char>        <char>            <char>           <char>
#> 1: Sepal.Length   5.01 (0.35)       5.94 (0.52)      6.59 (0.64)
#> 2:  Sepal.Width   3.43 (0.38)       2.77 (0.31)      2.97 (0.32)
#> 3: Petal.Length   1.46 (0.17)       4.26 (0.47)      5.55 (0.55)
#> 4:  Petal.Width   0.25 (0.11)       1.33 (0.20)      2.03 (0.27)
#>                                                 Test
#>                                               <char>
#> 1:  F(2, 147) = 119.26, p < .001, Eta-squared = 0.62
#> 2:   F(2, 147) = 49.16, p < .001, Eta-squared = 0.40
#> 3: F(2, 147) = 1180.16, p < .001, Eta-squared = 0.94
#> 4:  F(2, 147) = 960.01, p < .001, Eta-squared = 0.93
egltable(iris, parametric = FALSE)
#>                 Mdn (IQR)/N (%)
#>          <char>          <char>
#> 1: Sepal.Length     5.80 (1.30)
#> 2:  Sepal.Width     3.00 (0.50)
#> 3: Petal.Length     4.35 (3.50)
#> 4:  Petal.Width     1.30 (1.50)
#> 5:      Species                
#> 6:       setosa      50 (33.3%)
#> 7:   versicolor      50 (33.3%)
#> 8:    virginica      50 (33.3%)
egltable(colnames(iris)[1:4], "Species", iris,
  parametric = FALSE)
#>                 setosa Mdn (IQR) versicolor Mdn (IQR) virginica Mdn (IQR)
#>          <char>           <char>               <char>              <char>
#> 1: Sepal.Length      5.00 (0.40)          5.90 (0.70)         6.50 (0.67)
#> 2:  Sepal.Width      3.40 (0.48)          2.80 (0.48)         3.00 (0.38)
#> 3: Petal.Length      1.50 (0.18)          4.35 (0.60)         5.55 (0.78)
#> 4:  Petal.Width      0.20 (0.10)          1.30 (0.30)         2.00 (0.50)
#>                                        Test
#>                                      <char>
#> 1:  KW chi-square = 96.94, df = 2, p < .001
#> 2:  KW chi-square = 63.57, df = 2, p < .001
#> 3: KW chi-square = 130.41, df = 2, p < .001
#> 4: KW chi-square = 131.19, df = 2, p < .001
egltable(colnames(iris)[1:4], "Species", iris,
  parametric = c(TRUE, TRUE, FALSE, FALSE))
#>                            setosa See Rows versicolor See Rows
#>                     <char>          <char>              <char>
#> 1:    Sepal.Length, M (SD)     5.01 (0.35)         5.94 (0.52)
#> 2:     Sepal.Width, M (SD)     3.43 (0.38)         2.77 (0.31)
#> 3: Petal.Length, Mdn (IQR)     1.50 (0.18)         4.35 (0.60)
#> 4:  Petal.Width, Mdn (IQR)     0.20 (0.10)         1.30 (0.30)
#>    virginica See Rows                                             Test
#>                <char>                                           <char>
#> 1:        6.59 (0.64) F(2, 147) = 119.26, p < .001, Eta-squared = 0.62
#> 2:        2.97 (0.32)  F(2, 147) = 49.16, p < .001, Eta-squared = 0.40
#> 3:        5.55 (0.78)         KW chi-square = 130.41, df = 2, p < .001
#> 4:        2.00 (0.50)         KW chi-square = 131.19, df = 2, p < .001
egltable(colnames(iris)[1:4], "Species", iris,
  parametric = c(TRUE, TRUE, FALSE, FALSE), simChisq=TRUE)
#>                            setosa See Rows versicolor See Rows
#>                     <char>          <char>              <char>
#> 1:    Sepal.Length, M (SD)     5.01 (0.35)         5.94 (0.52)
#> 2:     Sepal.Width, M (SD)     3.43 (0.38)         2.77 (0.31)
#> 3: Petal.Length, Mdn (IQR)     1.50 (0.18)         4.35 (0.60)
#> 4:  Petal.Width, Mdn (IQR)     0.20 (0.10)         1.30 (0.30)
#>    virginica See Rows                                             Test
#>                <char>                                           <char>
#> 1:        6.59 (0.64) F(2, 147) = 119.26, p < .001, Eta-squared = 0.62
#> 2:        2.97 (0.32)  F(2, 147) = 49.16, p < .001, Eta-squared = 0.40
#> 3:        5.55 (0.78)         KW chi-square = 130.41, df = 2, p < .001
#> 4:        2.00 (0.50)         KW chi-square = 131.19, df = 2, p < .001

diris <- data.table::as.data.table(iris)
egltable("Sepal.Length", g = "Species", data = diris)
#>                 setosa M (SD) versicolor M (SD) virginica M (SD)
#>          <char>        <char>            <char>           <char>
#> 1: Sepal.Length   5.01 (0.35)       5.94 (0.52)      6.59 (0.64)
#>                                                Test
#>                                              <char>
#> 1: F(2, 147) = 119.26, p < .001, Eta-squared = 0.62

tmp <- mtcars
tmp$cyl <- factor(tmp$cyl)
tmp$am <- factor(tmp$am, levels = 0:1)

egltable(c("mpg", "hp"), "vs", tmp)
#>                 0 M (SD)      1 M (SD)                                 Test
#>    <char>         <char>        <char>                               <char>
#> 1:    mpg   16.62 (3.86)  24.56 (5.38) t(df=30) = -4.86, p < .001, d = 1.73
#> 2:     hp 189.72 (60.28) 91.36 (24.42)  t(df=30) = 5.73, p < .001, d = 2.04
egltable(c("mpg", "hp"), "am", tmp)
#>                 0 M (SD)       1 M (SD)                                 Test
#>    <char>         <char>         <char>                               <char>
#> 1:    mpg   17.15 (3.83)   24.39 (6.17) t(df=30) = -4.11, p < .001, d = 1.48
#> 2:     hp 160.26 (53.91) 126.85 (84.06)  t(df=30) = 1.37, p = .180, d = 0.49
egltable(c("am", "cyl"), "vs", tmp)
#> Warning: Chi-squared approximation may be incorrect
#>              0 N (%)    1 N (%)
#>    <char>     <char>     <char>
#> 1:     am                      
#> 2:      0 12 (66.7%)  7 (50.0%)
#> 3:      1  6 (33.3%)  7 (50.0%)
#> 4:    cyl                      
#> 5:      4   1 (5.6%) 10 (71.4%)
#> 6:      6  3 (16.7%)  4 (28.6%)
#> 7:      8 14 (77.8%)   0 (0.0%)
#>                                                       Test
#>                                                     <char>
#> 1:         Chi-square = 0.91, df = 1, p = .341, Phi = 0.17
#> 2:                                                        
#> 3:                                                        
#> 4: Chi-square = 21.34, df = 2, p < .001, Cramer's V = 0.82
#> 5:                                                        
#> 6:                                                        
#> 7:                                                        

tests <- with(sleep,
    wilcox.test(extra[group == 1],
           extra[group == 2], paired = TRUE))
str(tests)
#> List of 7
#>  $ statistic  : Named num 0
#>   ..- attr(*, "names")= chr "V"
#>  $ parameter  : NULL
#>  $ p.value    : num 0.00391
#>  $ null.value : Named num 0
#>   ..- attr(*, "names")= chr "location shift"
#>  $ alternative: chr "two.sided"
#>  $ method     : chr "Wilcoxon signed rank exact test"
#>  $ data.name  : chr "extra[group == 1] and extra[group == 2]"
#>  - attr(*, "class")= chr "htest"

## example with paired data
egltable(c("extra"), g = "group", data = sleep, idvar = "ID", paired = TRUE)
#>              1 M (SD)    2 M (SD)                               Test
#>    <char>      <char>      <char>                             <char>
#> 1:  extra 0.75 (1.79) 2.33 (2.00) t(df=9) = 4.06, p = .003, d = 1.28

## what happens when ignoring pairing (p-value off)
# egltable(c("extra"), g = "group", data = sleep, idvar = "ID")

## paired categorical data example
## using data on chick weights to create categorical data
tmp <- subset(ChickWeight, Time %in% c(0, 20))
tmp$WeightTertile <- cut(tmp$weight,
  breaks = quantile(tmp$weight, c(0, 1/3, 2/3, 1), na.rm = TRUE),
  include.lowest = TRUE)

egltable(c("weight", "WeightTertile"), g = "Time",
  data = tmp,
  idvar = "Chick", paired = TRUE)
#>                  0 M (SD)/N (%) 20 M (SD)/N (%)
#>           <char>         <char>          <char>
#> 1:        weight   41.06 (1.13)  209.72 (66.51)
#> 2: WeightTertile                               
#> 3:     [39,41.7]     32 (64.0%)        0 (0.0%)
#> 4:    (41.7,169]     18 (36.0%)      14 (30.4%)
#> 5:     (169,361]       0 (0.0%)      32 (69.6%)
#>                                              Test
#>                                            <char>
#> 1:           t(df=45) = 17.10, p < .001, d = 2.52
#> 2: McNemar's Chi-square = 39.00, df = 3, p < .001
#> 3:                                               
#> 4:                                               
#> 5:                                               

rm(tmp)
```

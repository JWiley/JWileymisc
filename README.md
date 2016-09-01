JWileymisc
========

An `R` package with general utility and convenience functions.
Some of these are general functions that help using and
exploring SEM style data.  Others are more specific.
This package has grown out of my own work, and is often to automate
repetitive tasks.


Installation
------------

To get the latest development version, use:

```
#install.packages("devtools")
library(devtools)

install_github("JWiley/JWileymisc")
```

To get the version on cran:

```
install.packages("JWileymisc")
```


Examples
--------

I do not have any vignettes or demos for this package.
However, the functions are documented and I have included examples in the
function documentation that are relatively basic.  Below are just a
few examples that I tend to use it for:

- check the univariate distribution of a variable:

```
testdistr(mtcars$mpg, "normal")
```

- check the distributions of a variables' residuals

```
testdistr(resid(lm(Petal.Length ~ Species, data = iris)), "normal")
```

- check for multivariate normality

```
mvqq(iris[,-5])
```

- view a heatmap of a correlation matrix

```
plot(SEMSummary(~ Petal.Length + Sepal.Length + Petal.Width + Sepal.Width, data = iris))
```

This package also has some utility functions used for other packages,
such as calculating empirical p-values from bootstrapping or MCMC
samples as from a Bayesian analysis, etc. These are probably less
interesting to most users.

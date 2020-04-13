# JWileymisc 1.1.1

## Bug Fixes
* `modelTest()` now works correctly when there are interaction terms
  with categorical variables and when most "on-the-fly" transformations are
  performed, such as `log()` etc. Does not work when new variables
  that are a composite of multiple variables are created, e.g.,
  `I(hp + wt)`, but a more informative error message is given.
* Added the correct version requirements for `data.table` to the
  DESCRIPTION and dependencies.


# JWileymisc 1.1.0

## Changes
* Polished methods for functions, including `APAStyler()` methods.

# JWileymisc 1.0.1

## Bug Fixes
* `residualDiagnostics.lm()` and so too `modelDiagnostics.lm`
  would return the index of extreme values
  based on the complete data used for modelling, not of the original 
  input dataset. This made it difficult to identify and remove extreme
  values in subsequent model runs. This is now corrected.

# JWileymisc 1.0.0

## Changes

* This version includes a significant re-write of the package that 
  results in many of the previous user-facing functions changing 
  names and arguments. This will likely break old code.
  This re-write was necessary to help standardize functions and
  arguments, and to make functions more robust.
  Many functions are now generics with specific methods written.
  Further, some functions previously bundled into `JWileymisc` have 
  been separated into other packages, including the new 
  `extraoperators` package, covering binary operators, 
  and a package for diagnostics on mixed models. See the new vignettes 
  added to the package for examples of current practice in using
  `JWileymisc`.

## New Features
* `egltable()` has added statistical tests for paired data. For
  continuous, parametric paired data, a pseudo Cohen's d is calculated
  on the change scores.

# JWileymisc 0.3.2

## New Features
* `omegaSEM()` Function that calculates coefficient omega for 
  measuring internal consistency reliability. Works for two 
  level models and returns within and between level omega 
  values.

* `egltable()` Function has added effect sizes when multiple groups
  are compared including Cohen's d for two groups, eta-squared for
  multiple groups, and phi for categorical variables.

## Bug Fixes
* `testdistr()` now only finds extreme values for the right tail of a 
   chi-square distribution.
* `.detailedTestsVGLM()` now identifies levels of the outcome
  correctly.

# JWileymisc 0.3.1

## Changes
* `detailedTests()` is now more generic and dispatches to 
  `.detailedTestsLMER()` or `.detailedTestsVGLM()` to provide 
  detailed tests for both linear mixed effects models and 
  multinomial logistic regression models fit by `vglm()`.
* `ezMULTINOM()` is now deprecated in favor of the new, more 
  generic `detailedTests()`.

## Bug Fixes
* `testdistr()` now creates more appropriate plots for discrete 
   distributions including the Poisson and Negative Binomial.

* `moments()` now updated to accomodate changes in the lavaan 
  package (thanks to Yves Rosseel)
  
* `TukeyHSDgg()` updated to use the emmeans package instead of 
  the now defunct lsmeans package.
  
* `formatLMER()` returned the lower confidence interval twice 
  instead of the lower and upper confidence interval. 
  This is now fixed.

# JWileymisc 0.3.0

## New Features
* `R2LMER()` A simple function to calculate the marginal and
  conditional variance accounted for by a model estimated by 
  `lmer()`.

* `compareLMER()` A function to compare two models estimated by 
   `lmer()` include significance tests and effect sizes 
   for estimates of the variance explained.
   
* `detailedTests()` A function to compute detailed tests on a 
   model estimated from `lmer()` including confidence intervals 
   for parameters, significance tests, where possible, 
   overall model fit, and effect sizes for the model and each variable.
     
* `formatLMER()` A function to nicely format detailed model results,
  possibly from multiple models.  Requires results from 
  `detailedTests()` based on `lmer()` models, at the moment.
  
* `iccMixed()` A function to calculate the intraclass correlation 
	coefficient using mixed effects models.  Works with either 
	normally distributed outcomes or binary outcomes, in which case 
	the latent variable estimate of the ICC is computed.
	
* `nEffective()` Calculates the effective sample size based on 
    the number of independent units, number of observations per 
	unit, and the intraclass correlation coefficient.
	
* `acfByID()` Calculates the lagged autocorrelation of a variable 
    by an ID variable and returns a data.table for further use,
	such as examination, summary, or plotting

* `meanDeviations()` A simple function to calculate means and mean 
	deviations, useful for creating between and within versions of 
	a variable in a data.table

* `as.na()` function added to convert data to missing (NA) while
    preserving the class/type of the data (useful for data.table).
	
* `meanDecompose()` function added to decompose multilevel or 
    repeated measures data into means and residuals.
	
* `timeshift()` function added to center a time variable at a new 
    zero point. Useful when times may start and end off the standard 
	24 hour period (e.g., 11am to 2am, which technically fall on
    different dates).

* `intSigRegGraph()` function added to graph regions of significance
	from interactions with linear models as well as the mostly helper
	function, `findSigRegions()`.
  
* `ezMULTINOM()` new function added to make running multinomial 
	logistic regression easy in R, along with all pairwise contrasts 
	and omnibus tests of statistical significance.

* `testdistr()` function expanded to cover multivariate
	normal data, and the old `mvqq()` function is now deprecated.

* `testdistr()` includes optional robust estimates for
	univariate and multivariate normal data
   
* `formatHtest()` gains support for pearson, kendal, and spearman
	correlations from the `cor.test()` function
	
* `logicals` A series of support functions for findings values in 
    a particular range, such as `%gele%` for values greater than or 
    equal to the min and less than or equal to the max as well as 
	to automatically subset the data when prefixed with an s, 
	`%sgele%` `%sin%` etc.

## Bug Fixes

* `winsorizor()` now properly handles atomic data. Fixes
	an issue where variables in a data table would be
	atomic after calling the `scale()` function and
	`winsorizor()` would fail.
	
* `egltable()` now works with data.tables

# JWileymisc 0.2.1

## New Features
* `testdistr()` function to plot data against different theoretical
	distributions using `ggplot2`. A sort of generalized `qqnorm()`
	allowing other distributions besides the normal distribution.

* `winsorizor()` Function moved from `pscore` package. Sets any values
	beyond specific quantiles of the empirical data to the specified
	quantiles. Can work on vectors, data frames, or matrices.

# JWileymisc 0.2.0

Initial release to CRAN.

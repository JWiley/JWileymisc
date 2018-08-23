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

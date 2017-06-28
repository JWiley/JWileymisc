# JWileymisc 0.3.0

## New Features

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

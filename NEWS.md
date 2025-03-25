# JWileymisc 1.4.2

## Changes
* `corplot()` and functions that build on it (like `plot()` methods for `SEMSummary()` 
   now use a 45 degree angle for column labels to improve readability with long variable 
   names. As always this is a `ggplot2` theme option and can be overwritten by the user.
* `residualDiagnostics` gains an argument `quantiles`. It defaults to `TRUE` providing
   the same behaviour as before. It can optionally be set to `FALSE` to remove the quantile 
   estimations for residuals.
   This is useful when the smooths fail or to save computational time.
* `.quantilePercentiles` tries harder to detect degenerate or failing cases of 
   quantile/percentile estimation. This has implications for `residualDiagnostics`.
* `plot.residualDiagnostics()` no longer calculates a loess smooth line for the average
   of the residuals. Instead this is conducted as part of the `residualDiagnostics()`
   function when `quantiles = TRUE`. Also the mid line is now the median from a quantile 
   regression, first trying smooths and falling back to linear if that fails.
*  many of these changes also impact `modelDiagnostics()` and `plot.modelDiagnostics()`
   as they are based on residual diagnostics.

## New Features
* `geom_tufterange()` simple geom to add a left and bottom line that shows the observed range of non missing data.
* `theme_tufte()` simple mostly empty theme built off the minimal theme.

# JWileymisc 1.4.1

## Bug Fixes
* Updated code in preparation for is.atomic(NULL) no longer working.
* Updated to address aes_string() being deprecated

# JWileymisc 1.4.0

## New Features
* `is.naz()` is a new function that is the testing counter part to `naz.omit()`.
   Notably, both `is.naz()` and `naz.omit()` both also identify and exclude 
   non finite values now. This is new behavior.

## Changes
* `egltable()` has more error checks including for variables
   that are all missing values. Frequency (percent) results are 
   now shown more clearly with a percent sign (%). This corresponds 
   to changes in the backend of `egltable()` to facilitate more 
   tests and descriptives to be calculated.

# JWileymisc 1.3.0

## New Features
* `diffCircular()` calculates the circular difference 
	between two vectors.
* `saveRDSfst()` saves RDS files using `fst` for multithreaded compression.
* `readRDSfst()` reads RDS files using `fst` for multithreaded decompression.

## Bug Fixes
* `egltable()` now correctly handles categorical variables by 
   a grouping variable, when the categorical variable is not 
   a factor class. Fixes a bug that could occur with 
   cells with zero frequencies when the variables were not 
   factors.
* `winsorizor()` would not properly check if a vector of percentiles 
   was passed. This is fixed and a new test added to prevent regression.
   
## Changes
* Now using testthat 3e and preferably for light/dark package website.

# JWileymisc 1.2.0

## Bug Fixes
* `meanCircular()` would be off by pi in some circumstances. This has
  been corrected and more test cases added.
* `SEMSummary()` pairwise correlations were based on the standardized
  pairwise covariance matrix, which used the same standard deviation
  for a variable regardless of the pair. This has now been fixed.
* Formula has been specified in calls to `stat_smooth()` to reduce 
  messages about this.
  
## Changes
* `corplot()` uses a more color blind friendly palette and defaults to
  showing correlations and p-values.
* Model diagnostics are smarter about linear regressions with essentially 
  discrete model predictions and no longer try to run quantile regression 
  to examine homogeneity of variance in these cases.
* Switched to using `ggpubr` instead of `cowplot` for themes and 
  arranging multiple plots.
* Bumped minimum version of `R` required.
* Further unit testing.

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

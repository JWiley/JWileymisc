# JWileymisc 0.3.0

## New Features

* `intSigRegGraph()` function added to graph regions of significance
  from interactions with linear models as well as the mostly helper
  function, `findSigRegions()`.

* `testdistr()` function expanded to cover multivariate
  normal data, and the old `mvqq()` function is now deprecated.

* `testdistr()` includes optional robust estimates for
   univariate and multivariate normal data

## Bug Fixes

* `winsorizor()` now properly handles atomic data. Fixes
  an issue where variables in a data table would be
  atomic after calling the `scale()` function and
  `winsorizor()` would fail.

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

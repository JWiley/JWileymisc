#' Calculates an empirical p-value based on the data
#'
#' This function takes a vector of statistics and calculates
#' the empirical p-value, that is, how many fall on the other
#' side of zero.  It calculates a two-tailed p-value.
#'
#' @param x a data vector to operate on
#' @param na.rm Logical whether to remove NA values. Defaults to \code{TRUE}
#' @return a named vector with the number of values falling at
#'   or below zero, above zero, and the empirical p-value.
#' @author Joshua F. Wiley <josh@@elkhartgroup.com>
#' @export
#' @keywords utilities
#' @examples
#'
#' empirical_pvalue(rnorm(100))
empirical_pvalue <- function(x, na.rm = TRUE) {
  x <- as.integer(x <= 0)
  tmp <- table(factor(x, levels = 1:0, labels = c("<= 0", "> 0")))
  m <- mean(x, na.rm = na.rm)
  pval2 <- 2 * min(m, 1 - m)
  out <- c(as.vector(tmp), pval2)
  names(out) <- c(names(tmp), "p-value")

  out
}

#' Change directory
#'
#' The function takes a path and changes the current working directory
#' to the path. If the directory specified in the path does not
#' currently exist, it will be created.
#'
#' The function has been designed to be platform independent,
#' although it has had limited testing. Path creation is done using
#' \code{file.path}, the existence of the directory is checked using
#' \code{file.exists} and the directory created with \code{dir.create}.
#' Only the first argument, is required.  The other optional arguments
#' are handy when one wants to create many similar directories with a common base.
#'
#' @param base a character string with the base path to the directory. This is required.
#' @param pre an optional character string with the prefix to add to
#'   the base path. Non character strings will be coerced to character class.
#' @param num an optional character string, prefixed by \code{pre}.
#'   Non character strings will be coerced to character class.
#' @return NULL, changes the current working directory
#' @keywords utilities
#' @export
#' @examples
#' \dontrun{
#' # an example just using the base
#' cd("~/testdir")
#'
#' # an example using the optional arguments
#' base <- "~/testdir"
#' pre <- "test_"
#'
#' cd(base, pre, 1)
#' cd(base, pre, 2)
#' }
cd <- function(base, pre, num) {
  stopifnot(is.character(base))
  if (!missing(pre) & !missing(num)) {
    pre <- as.character(pre)
    num <- as.character(num)
    newdir <- file.path(base, paste0(pre, num))
  } else {
    newdir <- file.path(base)
  }
  if (!file.exists(newdir)) {
    dir.create(newdir)
  }
  setwd(newdir)
  return(invisible(NULL))
}

#' Convert a correlation matrix and standard deviations to a covariance matrix
#'
#' This is a simple function designed to convert a correlation matrix
#' (standardized covariance matrix) back to a covariance matrix.
#' It is the opposite of \code{cov2cor}.
#'
#' @param V an n x n correlation matrix.  Should be numeric, square, and symmetric.
#' @param sigma an n length vector of the standard deviations. The length of the
#'   vector must match the number of columns in the correlation matrix.
#' @return an n x n covariance matrix
#' @seealso \code{\link{cov2cor}}
#' @export
#' @examples
#' # using a built in dataset
#' cor2cov(cor(longley), sapply(longley, sd))
#'
#' # should match the above covariance matarix
#' cov(longley)
#' all.equal(cov(longley), cor2cov(cor(longley), sapply(longley, sd)))
cor2cov <- function(V, sigma) {
  p <- (d <- dim(V))[1L]
  if (!is.numeric(V) || length(d) != 2L || p != d[2L])
      stop("'V' is not a square numeric matrix")
  if (length(sigma) != p)
      stop("'sigma' is not a vector comformable as the standard deviations of 'V'")
  if (any(diag(V) != 1))
      warning("diag(.) contained non 1 entries.  Did you pass a correlation matrix?")
  sigma * V * rep(sigma, each = p)
}

#' Return a non-missing correlation matrix
#'
#' Given a square, symmetric matrix (such as a correlation matrix)
#' this function tries to drop the fewest possible number of variables
#' to return a (square, symmetric) matrix with no missing cells.
#'
#' The assumption that x is square and symmetric comes because it is
#' assumed that the number of missing cells for a given column are identical
#' to that of the corresponding row.  \code{corOK} finds the column with the
#' most missing values, and drops that (and its corresponding row), and continues
#' on in like manner until the matrix has no missing values.  Although this was
#' intended for a correlation matrix, it could be used on other types of matrices.
#' Note that because \code{corOK} uses an iterative method, it can be slow when many
#' columns/rows need to be removed. For the intended use (correlation matrices) there
#' probably should not be many missing.  As a sanity check and to prevent tediously long
#' computations, the maximum number of iterations can be set.
#'
#' @param x a square, symmetric matrix or object coercable to such (such as a data frame).
#' @param maxiter a number indicating the maximum number of iterations,
#'   currently as a sanity check. See details.
#' @return A list with two elements
#'   \item{x}{The complete non missing matrix.}
#'   \item{keep.indices}{A vector of the columns and rows from the
#'     original matrix to be kept (i.e., that are nonmissing).}
#' @keywords utils
#' @export
#' @examples
#' cormat <- cor(iris[, -5])
#' # set missing
#' cormat[cbind(c(1,2), c(2,1))] <- NA
#'
#' # print
#' cormat
#'
#' # return complete
#' corOK(cormat)
#'
#' # using maximum iterations
#' corOK(cormat, maxiter=0)
#'
#' # clean up
#' rm(cormat)
corOK <- function(x, maxiter = 100) {
  n <- as.character(1:ncol(x))
  i <- 0L
  keep <- 1:ncol(x)

  lmat <- is.na(x)

  while (any(lmat[keep, keep]) && i < maxiter) {
    nmiss <- colSums(lmat[keep, keep])
    index <- which.max(nmiss)
    keep <- keep[!keep %in% as.integer(n[keep][index])]
    i <- i + 1L
  }

  if (i == maxiter) {
    warning("Maximum iterations exceeded.\n",
      "Currently kept indices will be returned.\n",
      "Try increasing maxiter or check why so many correlations are missing.")
    return(keep)
  }

  list(x = x[keep, keep], keep.indices = keep)
}


#' Coerces vectors to missing
#'
#' Given a vector, convert it to missing (NA) values,
#' where the class of the missing matches the input class.
#' Currently supports character, logical, integer, factor, numeric,
#' times (from \pkg{chron}), Date, POSIXct, POSIXlt, and
#' zoo (from \pkg{zoo}).
#'
#' @param x A vector to convert to missing (NA)
#' @return a vector the same length as the input with missing values of the same class
#' @keywords utils
#' @export
#' @examples
#' str(as.na(1L:5L))
#' str(as.na(rnorm(5)))
#' str(as.na(c(TRUE, FALSE)))
#' str(as.na(as.Date("2017-01-01")))
as.na <- function(x) {
  n <- length(x)
  if (inherits(x, "character")) {
    use <- NA_character_
  } else if (inherits(x, "logical")) {
    use <- NA
  } else if (inherits(x, "integer")) {
    use <- NA_integer_
  } else if (inherits(x, "factor")) {
    use <- factor(NA, levels = levels(x))
  } else if (inherits(x, "numeric")) {
    use <- NA_real_
  } else if (inherits(x, "times")) { ## from chron package
    use <- structure(rep(NA, n), format = "h:m:s", class = "times")
    return(use) ## force return as rep() does not play nicely with chron times
  } else if (inherits(x, "Date")) {
    use <- structure(NA_real_, class = "Date")
  } else if (inherits(x, "POSIXct")) {
    use <- structure(NA_real_, class = c("POSIXct", "POSIXt"))
  } else if (inherits(x, "POSIXlt")) {
    use <- structure(list(sec = NA_real_, min = NA_integer_, hour = NA_integer_,
                          mday = NA_integer_, mon = NA_integer_, year = NA_integer_,
                          wday = NA_integer_, yday = NA_integer_, isdst = -1L, zone = "",
                          gmtoff = NA_integer_), .Names = c("sec", "min", "hour", "mday",
                            "mon", "year", "wday", "yday", "isdst", "zone", "gmtoff"),
                     class = c("POSIXlt", "POSIXt"),
                     tzone = c("", "AEST", "AEDT"))
  } else if (inherits(x, "zoo")) { ## from zoo package
    use <- structure(rep(NA, n), index = rep(NA_integer_, n), class = "zoo")
    return(use) ## force return as rep() does not play nicely with zoo
  } else {
    stop(sprintf("Unknown class of type %s", class(x)))
  }
  rep(use, n)
}

#' Is a variable missing, non finite or zero length character?
#'
#' Given a vector, return \code{TRUE} or \code{FALSE}
#' if each element is either missing (NA/NaN), non finite (e.g. infinite)
#' or a zero length character string (only for character vectors).
#'
#' @param x A vector to identify missing / non finite or zero length strings from
#' @return a logical vector
#' @keywords utils
#' @export
#' @examples
#' is.naz(c(1, NA, NaN))
#' is.naz(c(1, NA, NaN, Inf))
#' is.naz(c("test", "", NA_character_))
is.naz <- function(x) {
  if (is.character(x)) {
    ok <- (is.na(x) | is.nan(x) | (!nzchar(x)))
  } else if (is.numeric(x)) {
    ok <- (is.na(x) | is.nan(x) | (!is.finite(x)))
  } else {
    ok <- (is.na(x) | is.nan(x))
  }
  ok
}


#' Missing and Zero Character Omit
#'
#' Given a vector, exclude any missing values,
#' not a number values, non finite values,
#' and if a character class, any zero length strings.
#'
#' @param x A vector to exclude missing, non finite or zero length strings from
#' @return a vector with missing/non finite/zero length strings omitted
#' @keywords utils
#' @importFrom stats na.omit
#' @export
#' @examples
#' ## stats na.omit
#' stats::na.omit(c(1, NA, NaN))
#' stats::na.omit(c("test", "", NA_character_))
#'
#' naz.omit(c(1, NA, NaN))
#' naz.omit(c(1L, NA))
#' naz.omit(c(1L, NA, Inf))
#' naz.omit(c("test", "", NA_character_))
naz.omit <- function(x) {
  ok <- !is.naz(x)
  x[ok]
}

#' Determine which if any variables are all missing in a dataset
#'
#' Internal function.
#'
#' @param data A dataset to check each variable if all missing / non finite / zero character vectors
#' @return \code{FALSE} if no variable(s) all missing, else an informative string message.
#' @keywords internal
#' @examples
#' JWileymisc:::.allmissing(mtcars)
#' cat(JWileymisc:::.allmissing(data.frame(a = NA, b = 1)), fill = TRUE)
.allmissing <- function(data) {
  k <- ncol(data)
  x <- rep(NA, k)

  for (i in seq_len(k)) {
    x[i] <- all(is.naz(data[[i]]))
  }

  if (isTRUE(any(x))) {
    out <- sprintf(
      "the following variable(s) were all missing/not finite/empty strings:\n%s",
      paste(names(data)[x], collapse = "\n"))
  } else {
    out <- FALSE
  }
  return(out)
}


#' Create a lagged variable
#'
#' Given a variable, create a k lagged version,
#' optionally do it by a grouping factor, such as an ID.
#'
#' @param x the variable to lag
#' @param k the length to lag it
#' @param by a variable to lag by. Must be sorted.
#' @return a vector of the lagged values
#' @keywords utils
#' @export
#' @examples
#' lagk(1:4, 1)
lagk <- function(x, k = 1, by) {
  m <- missing(by)
  pad <- rep(NA, k)
  if (missing(by)) {
    n <- length(x)
    if (k >= n) {
      out <- rep(NA, n)
    } else {
      out <- c(pad, x[-((n - k + 1):n)])
    }
  } else {
    if (!is.factor(by)) {
      by <- factor(by, levels = unique(by))
    }
    stopifnot(!is.unsorted(by))
    out <- unlist(tapply(x, by, function(xs) {
      n <- length(xs)
      if (k >= n) {
        tmpout <- rep(NA, n)
      } else {
        tmpout <- c(pad, xs[-((n - k + 1):n)])
      }
      return(tmpout)
    }))
  }
  return(out)
}


#' Shift a time variable to have a new center (zero point)
#'
#' Given a vector, shift the values to have a new center, but keeping the same
#' minimum and maximum.  Designed to work with time values where
#' the minimum indicates the same time as the maximum (e.g.,
#' 24:00:00 is the same as 00:00:00).
#'
#' @param x the time scores to shift
#' @param center A value (between the minimum and maximum) to center
#'   the time scores. Defaults to 0, which has no effect.
#' @param min The theoretical minimum of the time scores.
#'   Defaults to 0.
#' @param max the theoretical maximum of the time scores.
#'   Defaults to 1.
#' @param inverse A logical value, whether to \sQuote{unshift}
#'   the time scores.  Defaults to \code{FALSE}.
#' @return A vector of shifted time scores, recentered as specified.
#' @export
#' @examples
#' ## example showing centering at 11am (i.e., 11am becomes new 0)
#' plot((1:24)/24, timeshift((1:24)/24, 11/24))
#'
#' ## example showing the inverse, note that 24/24 becomes 0
#' plot((1:24)/24, timeshift(timeshift((1:24)/24, 11/24), 11/24, inverse = TRUE))
timeshift <- function(x, center = 0, min = 0, max = 1, inverse = FALSE) {
  stopifnot(center >= min && center <= max)
  stopifnot(all(x >= min))
  stopifnot(all(x <= max))

  if (isTRUE(inverse)) { ## need to fix this
    ifelse(x >= (max - center),
           (x + center) - max,
           x + center)
  } else if (isFALSE(inverse)) {
    ifelse(x < center,
           (max - (center - x)),
           x - center)
  } else {
    stop("invalid inverse option, must be TRUE/FALSE")
  }
}

#' Create a character vector or file hash of a dataset and each variable
#'
#' Given a \code{data.frame} or \code{data.table}, create a character vector
#' MD5 hash of the overall dataset and each variable. The goal of this is to create
#' a secure vector / text file that can be tracked using version control
#' (e.g., GitHub) without requiring commiting sensitive datasets.
#' The tracking will make it possible to evaluate whether two datasets are the
#' same, such as when sending data or when datasets may change over time
#' to know which variable(s) changed, if any.
#'
#' @param x A \code{data.frame} or \code{data.table} to be hashed.
#' @param file An optional character string. If given, assumed to be the path/name of a
#'   file to write the character string hash out to, for convenience. When
#'   non missing, the character vector is returned invisibly and a file written.
#'   When missing (default), the character vector is returned directly.
#' @return A (possibly invisible) character vector. Also (optionally) a text file
#'   written version of the character string.
#' @export
#' @importFrom digest digest
#' @importFrom data.table is.data.table
#' @examples
#'
#' hashDataset(mtcars)
#'
#' ## if a file is specified it will write the results to the text file
#' ## nicely formatted, along these lines
#'
#' cat(hashDataset(cars), sep = "\n")
#' 
hashDataset <- function(x, file) {
  stopifnot(isTRUE(is.data.frame(x)) || isTRUE(is.data.table(x)))

  textcall <- match.call()
  textcall <- deparse(textcall$x)

  hash <- c(
    sprintf("dataset: '%s', MD5: %s", textcall, digest(x, "md5")),
    unlist(lapply(names(x), function(y) {
      sprintf("'%s' (%s), %s, %s (sorted)",
              y, paste(class(x[[y]]), collapse = "; "),
              digest(x[[y]], "md5"), digest(sort(x[[y]]), "md5"))

    })))

  if (!missing(file)) {
    write(x = hash,
          file = file,
          sep = "\n")
    return(invisible(hash))
  } else {
    return(hash)
  }
}


#' @name compressed RDS
#' @rdname compressedrds
#'
#' @title  Save and read RDS functions for using multithreaded \dQuote{ZSTD} or \dQuote{LZ4} compression
#'
#' @details By default, \code{saveRDS()} does not have multithreaded compression
#' built in. These functions use \dQuote{ZSTD} or \dQuote{LZ4} compression
#' via the \code{fst} package for multithreaded compression and decompression
#' with good performance.
#' To save them, objects are serialized, compressed, and then saved using \code{saveRDS()}.
#' To read them, objects are read in using \code{readRDS()}, decompressed, and then unserialized.
#' Hashing is used to verify the results. \code{saveRDS()} is performed using \code{version = 3},
#' so it will not work on older versions of \code{R}.
#'
#' @param object An R object to be saved.
#' @param filename A character string giving the filename of the object on disk (to save to or read from)
#' @param compression A numeric value between 0 and 100 indicating the amount of compression.
#'   Defaults to 100, the highest level of compression. 0 gives the lowest compression.
#' @param algorithm A character string of the type of compression to use.
#'   Defaults to \dQuote{ZSTD} which is better compression but slower.
#'   The only other option is \dQuote{LZ4} which is faster but may provide less compression.
#' @return \code{saveRDSfst()} is called for its side effect of saving a file to disk.
#'   The original \code{R} object if using \code{readRDSfst()}.
NULL

#' @rdname compressedrds
#' @importFrom fst compress_fst
#' @export
#' @examples
#'
#' saveRDSfst(mtcars, filename = file.path(tempdir(), "mtcars.RDS"))
saveRDSfst <- function(object, filename = "", compression = 100, algorithm = "ZSTD") {
  if (isFALSE(is.character(filename))) {
    stop("'filename' must be a character string")
  }
  if (isFALSE(identical(length(filename), 1L))) {
    stop("'filename' must have length one, only a single filename can be specified")
  }
  if (isFALSE(nzchar(filename))) {
    stop("'filename' must be a non zero character string")
  }

  if (isFALSE(is.character(algorithm))) {
    stop("'algorithm' must be a character string of either 'ZSTD' (default) or 'LZ4' (faster)")
  }
  if (isFALSE(identical(length(algorithm), 1L))) {
    stop("'algorithm' must have length one, only a single algorithm can be specified")
  }
  if (isFALSE(algorithm %in% c("ZSTD", "LZ4"))) {
    stop("'algorithm' must be a character string of either 'ZSTD' (default) or 'LZ4' (faster)")
  }

  if (isFALSE(is.numeric(compression) || is.integer(compression))) {
    badclass <- paste(class(compression), collapse = " ")
    stop(sprintf("'compression' must be numeric or integer class but was %s", badclass))
  }
  if (isFALSE(identical(length(compression), 1L))) {
    stop("'compression' must have length one, only a single compression value can be specified")
  }

  compression <- as.numeric(compression)
  if (isFALSE(compression %gele% c(0, 100))) {
    stop("'compression' must be between 0 and 100")
  }

  base::saveRDS(
    compress_fst(
      serialize(object, NULL),
      compression = as.numeric(compression), hash = TRUE,
      compressor = algorithm),
    file = filename, version = 3)
}

#' @rdname compressedrds
#' @importFrom fst decompress_fst
#' @export
#' @examples
#'
#' saveRDSfst(mtcars, filename = file.path(tempdir(), "mtcars.RDS"))
#' readRDSfst(file.path(tempdir(), "mtcars.RDS"))
readRDSfst <- function(filename) {
  if (isFALSE(is.character(filename))) {
    stop("'filename' must be a character string")
  }
  if (isFALSE(identical(length(filename), 1L))) {
    stop("'filename' must have length one, only a single filename can be specified")
  }
  if (isFALSE(nzchar(filename))) {
    stop("'filename' must be a non zero character string")
  }

  unserialize(decompress_fst(base::readRDS(filename)))
}

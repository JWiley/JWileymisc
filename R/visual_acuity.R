##' An S4 class to hold visual acuity data
##'
##' A class to hold Visual Acuity data for the oculus sinister (OS; left eye)
##' and oculus dexter (OD; right eye)
##'
##' @slot originalOS the original visual acuity data for the left (ocular sinister) eye
##' @slot originalOD the original visual acuity data for the right (ocular dexter) eye
##' @slot logMAROS Logarithm of the minimum angle of resolution data for OS
##' @slot logMAROD Logarithm of the minimum angle of resolution data for OD
##' @slot chart.values the snellen values for each line of the chart used
##'   to measure visual acuity.  Used for the linear interpolation in the
##'   case of partially correct line readings.
##' @slot chart.nletters the number of letters on each line of the chart
##'   used to measure visual acuity.  Used for the linear interpolation
##'   in the case of partially correct line readings (+2 is 2/4 of the
##'   way to the next line if there are four letters, but only 2/6 if
##'   there are six, etc.)
##' @slot zero the logMAR value chosen to represent "zero" visual acuity
##'   when creating the combined logMAR values for both eyes or taking
##'   the arithmetic mean.
##' @import methods
setClass("VAObject",
         representation(
           originalOS = "ANY",
           originalOD = "ANY",
           logMAROS = "numeric",
           logMAROD = "numeric",
           chart.values = "character",
           chart.nletters = "integer",
           zero = "numeric"),
         prototype(
           originalOS = character(0),
           originalOD = character(0),
           logMAROS = numeric(0),
           logMAROD = numeric(0),
           chart.values = paste("20/",
                                c(200, 100, 70, 50, 40, 30, 25, 20, 15, 10), sep = ''),
           chart.nletters = 1:10,
           zero = as.numeric(NA)),
         validity = function(object) {
           noOS <- length(object@originalOS); noOD <- length(object@originalOD)
           coOS <- class(object@originalOS); coOD <- class(object@originalOD)
           nlOS <- length(object@logMAROS); nlOD <- length(object@logMAROD)
           nCV <- length(object@chart.values); nCN <- length(object@chart.nletters)

           results <- c(
             if (identical(noOS, noOD)) TRUE else paste("Lengths of 'originalOS' (",
                                                        noOS, ") and of 'originalOD' (", noOD, ") should be equal.", sep = ""),
             if (identical(coOS, coOD)) TRUE else paste("Class of 'originalOS' (",
                                                        coOS, ") and of 'originalOD' (", coOD, ") should be identical.", sep = ""),
             if (identical(nlOS, nlOD)) TRUE else paste("Lengths of 'logMAROS' (",
                                                        nlOS, ") and of 'logMAROD' (", nlOD, ") should be equal.", sep = ""),
             if (identical(nCV, nCN)) TRUE else paste("Lengths of 'originalOS' (",
                                                      noOS, ") and of 'originalOD' (", noOD, ") should be equal.", sep = "")
           )

           if (isTRUE(all(as.logical(results)))) TRUE else print(results)
         })


## ValidVASummaryObject <- function(object) {
##   noOS <- length(object@originalOS)
##   nlogMARc <- length(object@logMAR.combined)
##   nSnellenc <- length(object@snellen.combined)
##   nmlogMAR <- length(object@mean.logMAR)
##   nmSnellen <- length(object@mean.snellen)
##   results <- c(
##     if (identical(noOS, nlogMARc)) TRUE else paste(
##       "Lengths of 'originalOS' (", noOS, ") and of 'logMAR.combined' (",
##       nlogMARc, ") should be equal.", sep = ""),
##     if (identical(noOS, nlogMARc)) TRUE else paste(
##       "Lengths of 'originalOS' (", noOS, ") and of 'snellen.combined' (",
##       nSnellenc, ") should be equal.", sep = ""),
##     if (identical(nmSnellen, 2L)) TRUE else paste(
##       "Length of 'mean.logMAR' (", nmlogMAR, ") should be equal to 2",
##       sep = ""),
##     if (identical(nmSnellen, 2L)) TRUE else paste(
##       "Length of 'mean.snellen' (", nmSnellen, ") should be equal to 2",
##       sep = "")
##   )

##   if (isTRUE(all(as.logical(results)))) TRUE else print(results)
## }

##' An S4 class to hold visual acuity summary data
##'
##' A class designed to hold visual acuity summary data
##'
##' @slot logMAR.combined Numeric values of the combined logarithm of
##'   the minimum angle of resolution data for both eyes
##' @slot snellen.combined the snellen values back transformed from the
##'   combined logMAR values
##' @slot mean.logMAR average of the logarithm of the minimum angle of resolution data
##' @slot mean.snellen average of the combined Snellen data
##' @import methods
setClass("VASummaryObject",
  representation("VAObject",
    logMAR.combined = "numeric",
    snellen.combined = "character",
    mean.logMAR = "numeric",
    mean.snellen = "character"),
  prototype(
    logMAR.combined = numeric(0),
    snellen.combined = character(0),
    mean.logMAR = numeric(0),
    mean.snellen = character(0)),
  validity = NULL)


##' @describeIn VAObject extract method
##'
##' @param x the object to subset
##' @param i the rows to subset (optional)
##' @param j the columns to subset (optional)
##' @param drop should be missing
##' @param ... Additional arguments passed to lower functions
##' @export
##' @aliases [,VAObject-method
setMethod(f = "[",
  signature = "VAObject",
  definition = function (x, i, j, ..., drop = TRUE) {
    newx <- cbind(logMAROS = x@logMAROS, logMAROD = x@logMAROD)

    callNextMethod(x = newx, i = i, j = j, ... = ..., drop = drop)
  })

##' @describeIn VAObject print method
##'
##' @param object A VAObject class object
setMethod(f = "print",
  signature(x = "VAObject"),
  definition = function(x, ...) {
    cat("Chart values and number of letters for interpolation \n\n")
    print(cbind(Values = x@chart.values,
      Nletters = x@chart.nletters), ...)
    cat("\n Zero value used for missing values \n")
    print(x@zero, ...)
    cat("\n Left and Right Eye logMAR Values \n")
    print(x[], ...)
  })


##' @describeIn VAObject show method
setMethod(f = "show",
  signature = "VAObject",
  definition = function(object) {
    print(object[])
  })

##' @describeIn VAObject summary method
##'
##' @param weightbest Logical whether to upweight the best seeing eye.
##'   Defaults to \code{TRUE}.
##' @param w A numeric vector of the weights, first for the best seeing
##'   then the worst seeing eye. Defaults to \code{c(.75, .25)}.
setMethod(f = "summary",
  signature = signature(object = "VAObject"),
  definition = function(object, weightbest = TRUE, w = c(.75, .25)) {
    output <- new("VASummaryObject", object)

    if (weightbest) {
      ## Thanks to Dr. David Winsemius for pmin/pmax
      output@logMAR.combined <-
        w[1] * pmin(object@logMAROS, object@logMAROD, na.rm = TRUE) +
        w[2] * pmax(object@logMAROS, object@logMAROD, na.rm = TRUE)
    } else {
      if (!identical(w[1], w[2])) {
        warning("Unequal weights applied to the left and right eyes")
      }
      output@logMAR.combined <-
        w[1] * object@logMAROS + w[2] * object@logMAROD
    }

    output@mean.logMAR <- mean(output@logMAR.combined, na.rm = TRUE)
    snell.denom <- logmar(x = output@logMAR.combined, 20, inverse = TRUE)
    output@snellen.combined <- paste("20/", round(snell.denom), sep = '')
    output@snellen.combined[is.na(snell.denom)] <- NA
    output@mean.snellen <- paste("20/",
      round(logmar(output@mean.logMAR, 20, inverse = TRUE)), sep = '')
    return(output)
  })


##' @describeIn VASummaryObject show method
##'
##' @param object The object to be shown
setMethod(f = "show",
  signature = "VASummaryObject",
  definition = function(object) {
    cat("Summary Information for logMAR Values", fill = TRUE)
    cat("------------------------------------- \n", fill = TRUE)
    print(summary(data.frame(
      "LeftEye" = object@logMAROS,
      "RightEye" = object@logMAROD,
      "Combined" = object@logMAR.combined)))
    cat("\n Mean Combined Snellen value:", object@mean.snellen, fill = TRUE)
  })

##' @describeIn VASummaryObject plot method
##'
##' @param x A VASummaryObject
##' @param y Should be missing
##' @param ... Additional, unused arguments
##' @importFrom graphics par layout
setMethod(f = "plot",
  signature = signature(x = "VASummaryObject", y = "missing"),
  definition = function(x, y, ...) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    layout(matrix(c(1, 2), 1, 2, byrow = TRUE))
    plot(x = x@logMAROS, y = x@logMAR.combined,
      xlab = "Left Eye logMAR", ylab = "Combined logMAR")
    plot(x = x@logMAROD, y = x@logMAR.combined,
      xlab = "Right Eye logMAR", ylab = "Combined logMAR")
  })


##' Internal Visual Acuity Functions
##'
##' This function is one of several designed to help convert measures of
##' visual acuity recorded typically recorded as Snellen fractions
##' (e.g., 20/20 sees at 20 feet what is "typically" seen at 20 feet.
##' 20/40 sees at 20 feet what is "typically" seen at 40 feet, etc.)
##' into statistically usable data.  It can also parse text data
##' for Counting Fingers (CF) and Hand Motion (HM)  and convert them to
##' approximate logMAR values.
##' This is an internal function and is not  meant to be
##' called directly.
##'
##' This treats CF as approximately 200 letters (per Holladay), so
##' CF at 10 feet has Snellen value "equivalent" of 10/200. HM is
##' approximately 10 times worse, so HM at 10 feet approximately
##' 10/2000.  After conversion, rough equivalents are passed to
##' \code{logmar} to actually be converted.
##' Other functions are responsible for suitably parsing the text and
##' passing numbers to \code{logmar}. If \code{inverse = FALSE} (the default),
##' \code{logmar} calculates \eqn{-log_{10}(\frac{snell.numerator}{x})}{-log10(snell.numerator/x)}.
##' If \code{TRUE}, then \eqn{\frac{snell.numerator}{10^{-x}}}{snell.numerator/(10 ^ -x)}.
##'
##' The \code{zero} argument is used to specify a "zero" logMAR value.
##' In particular, this is used when no distance information is given
##' (e.g., only "HM" or "CF" as opposed to "CF 6" or "HM 4").
##' For the reasioning and rational behind this, see the "Details"
##' section of \code{\link{VAConverter}}.
##'
##' For the \code{snellen} function, the input should be character data
##' and have a numerator and denominator separated by '/'.  E.g.,
##' \dQuote{20/20}, \dQuote{20/40 + 3}.  This handles both simple
##' and Snellen values that need to be interpolated given the
##' appropriate chart.
##'
##' logMAR calculations including interpolating partial lines.
##' Given \dQuote{20/25 + 3}, calculate the logMAR of 20/25 and
##' 20/20 (the next step up since '+'), and go 3/chart.nletters
##' of the way between these two values.  Similarly if
##' \dQuote{20/20 - 3}, it will go partway between 20/25 and 20/30.
##' Note that it depends on the lines and letters per line
##' \emph{on the actual chart used}. The \code{chart.values} and
##' \code{chart.nletters} should contain all the lines and number
##' of letters for the chart that was used.
##'
##' These functions were written to deal
##' with a very specific style of recording visual acuity for a study I
##' worked on.  It may or may not have much use elsewhere.  \code{CFHM}
##' was not intended to typically be called by the user directly.
##' Generally, a higher level function, (e.g., \code{VAConverter}) would
##' be called.
##'
##' @param x Character data of the form: \dQuote{CF 10}, \dQuote{HM 12},
##'   \dQuote{HM}, \dQuote{CF}, \dQuote{CF 2}, etc. to be converted to
##'   logMAR values.
##' @param snell.numerator The numerator of a Snellen fraction.
##'   It defaults to 20 (the most common one).
##' @param inverse \code{inverse} = FALSE by default.  If TRUE,
##' \code{logmar} will assume \code{x} is a logMAR value, and
##' calculate the denominator of a Snellen fraction using the
##' \code{snell.numerator} as the numerator.
##' @param zero A \dQuote{zero} logMAR value to be used for any CF or HM
##'   value missing a number.  May be an actual number or simply \code{NA}
##' @param snellenvalue The observed snellen values
##' @param chart.values The chart snellen values
##' @param chart.nletters The number of letters per chart line
##' @references Jack T. Holladay (2004).  Visual acuity measurements.
##'   \emph{Journal of Cataract & Refractive Surgery, 30}(2),
##'   pp. 287--290. DOI: 10.1016/j.jcrs.2004.01.014.
##'   \url{http://dx.doi.org/10.1016/j.jcrs.2004.01.014}
##' @seealso \code{\link{VAConverter}} the overall function typically
##'   called
##' @keywords internal
##' @name vainternal
NULL

##' @rdname vainternal
##' @examples
##' ## logMAR value for "perfect" 20/20 vision
##' JWileymisc:::logmar(x = 20)
##'
##' ## Go to and from logMAR value, should return "20"
##' ## there may be slight error due to floating point arithmetic
##' JWileymisc:::logmar(x = JWileymisc:::logmar(x = 20), inverse = TRUE)
##'
##' ## logMAR value for 20/40 vision
##' JWileymisc:::logmar(40)
logmar <- function(x, snell.numerator = 20, inverse = FALSE) {
  if (inverse) {
    decimal <- 10 ^ (-x)
    snell.denominator <- snell.numerator/decimal
    return(snell.denominator)
  }

  decimal <- snell.numerator/x
  logMAR <- ( -log10(decimal) )
  return(logMAR)
}

##' @rdname vainternal
##' @examples
##' ## logMAR approximations, note "HM" is just the zero value
##' JWileymisc:::CFHM(c("HM 20", "HM", "CF 20", "CF 12", "CF"), zero = 3)
##' ## In cases where there is insufficient data, rather than choose
##' ## an arbitrary value, you can may just use NA
##' JWileymisc:::CFHM(c("HM 20", "HM", "CF 20", "CF 12", "CF"), zero = NA)
CFHM <- function(x, zero) {
  output <- vector("numeric", length(x))
  index <- grepl("[0-9]", x)

  dat <- do.call("rbind", strsplit(x = x[index], split = "[[:space:]]+"))
  dat[, 1] <- ifelse(dat[, 1] == "CF", 200, ifelse(dat[, 1] == "HM", 2000,
    "Invalid/unsupported measure of visual acuity"))
  dat <- matrix(as.numeric(dat), nrow = dim(dat)[1],
    ncol = dim(dat)[2], dimnames = list(NULL, c("denom", "numer")))

  output[index] <- logmar(dat[, "denom"], dat[, "numer"])
  output[!index] <- zero
  return(output)
}

##' @rdname vainternal
snellen <- function(snellenvalue, chart.values, chart.nletters) {
  output <- vector("numeric", length(snellenvalue))
  index <- grepl("\\+|\\-", snellenvalue)

  if (any(index)) {
    ## logMAR calculations including interpolating partial lines
    isv <- strsplit(x = snellenvalue[index], split = "/|[[:space:]]+")
    isv <- as.data.frame(do.call("rbind", isv), stringsAsFactors = FALSE)
    isv[,-3] <- as.data.frame(lapply(isv[,-3], as.numeric))
    colnames(isv) <- c("numer", "denom", "sign", "nletters")

    ## This block of code converts the various arguments to numeric
    ## from possibly character data. It also reorders the chart so that higher (+) line
    ## numbers indicate better vision (i.e., 20/400 would come before 20/20, not after)
    new.chart <- matrix(
      data = as.numeric(unlist(strsplit(x = chart.values, split = "/"))),
      ncol = 2, byrow = TRUE)
    new.chart <- cbind(new.chart, chart.nletters)
    colnames(new.chart) <- c("numer", "denom", "nletters")
    reordered <- order(new.chart[ , "numer"] / new.chart[ , "denom"])
    new.chart <- new.chart[reordered, ]
    location <- match(paste(isv[, "numer"], isv[, "denom"], sep = "/"),
      chart.values[reordered])

    ## Depending whether the sign is positive or negative, it either goes up
    ## or down one line for interpolation
    location2 <- ifelse(isv[, "sign"] == "+", location + 1, location - 1)
    new.nletters <- new.chart[ifelse(isv[, "sign"] == "+", location2,
      location), "nletters"]

    # If the given Snellen value is "20/30 + 3", xnum = 20, xdenom = 30
    # and ynum/denom are the values for the line being interpolated between.
    # This just standardizes the variable names so one equation can do the calculations
    xnum <- new.chart[location, "numer"]
    xdenom <- new.chart[location, "denom"]
    ynum <- new.chart[location2, "numer"]
    ydenom <- new.chart[location2, "denom"]

    names(new.nletters) <- names(xnum) <- names(xdenom) <- names(ynum) <- names(ydenom) <- NULL

    ## Go chart.nletters/new.nletters of the distance between the logMARs
    ## i.e., a linear interpolation
    output[index] <-
        logmar(x = xdenom, snell.numerator = xnum) -
        ( (logmar(x = xdenom, snell.numerator = xnum) -
           logmar(x = ydenom, snell.numerator = ynum)) *
         (isv[, "nletters"] / new.nletters)
        )
  }

  if (any(!index)) {
    ## Simple logMAR calculations for snellen values
    sv <- do.call("rbind", strsplit(x = snellenvalue[!index], split = "/"))
    sv <- matrix(as.numeric(sv), nrow = dim(sv)[1], ncol = dim(sv)[2],
      dimnames = list(NULL, c("numer", "denom")))
    output[!index] <- logmar(x = sv[, "denom"],
      snell.numerator = sv[, "numer"])
  }

  return(output)
}

##' Visual Acuity Converter
##'
##' Converter character (string) input of Snellen fractions,
##' Counting Fingers (CF), and Hand Motion (HM) to logMAR
##' values for use in statistical models.  Can handle linear
##' interpolation if passed an appropriate chart or if the
##' measures fit with the default chart.
##'
##'   \code{VAConverter} is primarily designed to take raw character
##'   data of various forms and convert them to logMAR values.
##'   Acceptable examples include: "20/20", "20/80 + 3", "20/20 - 4",
##'   "10/20", "CF 10", "HM 2", "CF 4", "NLP", "LP", "", "CF", "HM",
##'   etc.  For Snellen values, both parts should be present, and
##'   there should be a space between components; e.g., between
##'   fraction, +/- and number or between CF and 10.  Although I have
##'   attempted to make it as flexible and general as possible, there
##'   are still fairly rigid requirements so that it can parse a
##'   variety of text formats to numerical values.  Optionally, it can
##'   also handle decimal values (i.e., the results of actually
##'   dividing a Snellen value 20/20 = 1).
##'
##' \code{chart.values} and \code{chart.nletters} must be the same
##' length.  These are used to interpolate values such as "20/20 + 3"
##' which is interpreted as reading all of the letters on the "20/20"
##' line and "3" of the letters on the next best line (typically
##' "20/15" but this can be chart dependent).  The functions goes 3/n
##' of the distance between the logMAR values for each line.  This is
##' why it is important to know the values for the chart \emph{that
##' was actually used}.
##'
##' If datatype = "logMAR", the values passed to \code{OS} and
##' \code{OD} are directly assigned to the \code{logMAROS} and
##' \code{logMAROD} slots of a \code{"\linkS4class{VAObject}"} and an
##' error is returned if that results in the creation of an invalid
##' object (e.g., they are not numeric or not of equal length).
##'
##' The \code{zero} argument is primarily included to facilitate
##' calculating averages.  For example, in some cases it may be nice
##' to get a sense of an individual's "overall" or "average" logMAR
##' value.  Because on the logMAR scale, 0 is "20/20", an alternate
##' number needs to be used.  3 was chosen as a rough default, but it
##' is by no means necessarily the best choice.  If you are not
##' interested in computing an average between the left and right eyes
##' within individuals, it makes sense to simply use \code{NA} rather
##' than a crude "zero" approximation.
##'
##' @param OS The values to be converted for the left eye
##'   (oculus sinister).
##' @param OD The values to be converted for the right eye
##'   (oculus dexter)
##' @param chart.values The Snellen fractions for the chart used (if
##'   interpolation is necessary and it is different from the
##'   default).
##' @param chart.nletters The number of letters on each line of the
##'   chart that was used.  Necessary for proper interpolation.
##' @param datatype The type of data passed to \code{OS} and
##'   \code{OD}.  One of "Snellen" (the default), "decimal", or
##'   "logMAR".  Determines what transformations are needed to convert
##'   to logMAR values.
##' @param zero The \dQuote{zero} logMAR value.  This is used as the
##'   zero point for visual acuity.  For example, for light perception
##'   (LP), no light perception (NLP), etc.  It defaults to 3 (which
##'   is equivalent to a Snellen value of 20/20000), but may also be
##'   \code{NA}.  See details.
##' @return An object of class \code{\linkS4class{VAObject}}.  This
##'   includes the left and right eye logMAR values in slots
##'   \code{@logMAROS} and \code{@logMAROD} as well as additional
##'   information.  More information can be found in the class
##'   documentation.
##' @export
##' @examples
##' ## sampdat <- c("HM 12", "20/20 + 3", "20/50", "CF", "HM",
##' ##              "20/70 - 2", "LP", NA, "Prosthetic")
##' ## tmp <- VAConverter(OS = sampdat, OD = rev(sampdat), datatype = "snellen")
VAConverter <- function(OS, OD, chart.values = NULL, chart.nletters = NULL,
  datatype = c("snellen", "decimal", "logMAR"), zero = 3) {

  datatype <- match.arg(datatype)

  dat <- new("VAObject", originalOS = OS, originalOD = OD,
    zero = as.numeric(zero))

  if (!missing(chart.values) && !missing(chart.nletters)) {
    dat@chart.values <- chart.values
    dat@chart.nletters <- chart.nletters
  }

  switch(datatype,
    snellen = {},
    decimal = {dat@logMAROS <- (-log10(dat@originalOS))
      dat@logMAROD <- (-log10(dat@originalOD))
      return(dat)},
    logMAR = {dat@logMAROS <- dat@originalOS
      dat@logMAROD <- dat@originalOD
      return(dat)})

  tmpOS <- dat@originalOS
  tmpOD <- dat@originalOD
  tmpOS[nchar(tmpOS) < 2] <- NA
  tmpOD[nchar(tmpOD) < 2] <- NA
  pattern <- c("/", "CF|HM", "LP|NLP|Prosthetic")
  indexOS <- sapply(pattern, grep, x = tmpOS, ignore.case = TRUE)
  indexOD <- sapply(pattern, grep, x = tmpOD, ignore.case = TRUE)

  ## To do: ignore 0 length grep results better, this is a terrible kludge
  if (length(indexOS[[1]]) > 0) {
  tmpOS[indexOS[[1]]] <- snellen(snellenvalue = tmpOS[indexOS[[1]]],
    chart.values = dat@chart.values,
    chart.nletters = dat@chart.nletters)
  }
  if (length(indexOS[[2]]) > 0) {
  tmpOS[indexOS[[2]]] <- CFHM(x = tmpOS[indexOS[[2]]], zero = zero)
  }
  if (length(indexOS[[3]]) > 0) {
  tmpOS[indexOS[[3]]] <- zero
  }

  if (length(indexOD[[1]]) > 0) {
  tmpOD[indexOD[[1]]] <- snellen(snellenvalue = tmpOD[indexOD[[1]]],
    chart.values = dat@chart.values,
    chart.nletters = dat@chart.nletters)
  }
  if (length(indexOD[[2]]) > 0) {
  tmpOD[indexOD[[2]]] <- CFHM(x = tmpOD[indexOD[[2]]], zero = zero)
  }
  if (length(indexOD[[3]]) > 0) {
  tmpOD[indexOD[[3]]] <- zero
  }

  dat@logMAROS <- as.numeric(tmpOS)
  dat@logMAROD <- as.numeric(tmpOD)

  ## if (isTRUE(ValidVAObject(dat))) return(dat) else stop("Malformed VAObject")
  return(dat)
}

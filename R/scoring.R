##' Score a set of items to create overall scale score
##'
##' This function creates a single scale score from a data frame, reversing as needed.
##'
##' @param data The data to check
##' @param okay A vector of okay or acceptable values
##' @param na.rm Logical whether to remove missing values or not. Defaults to \code{TRUE}
##' @return \code{TRUE} if all values are okay, otherwise an error
##' @importFrom extraoperators %snin%
##' @keywords internal
CheckVals <- function(data, okay, na.rm = TRUE) {
  cx <- class(data)
  if (!is.vector(data)) {
    if (is.list(data)) {
      data <- unlist(data)
    } else if (is.matrix(data) || is.array(data)) {
      data <- as.vector(data)
    }
  }
  if (!is.vector(data)) stop(cx, " is not a valid class for this function")

  if (na.rm) okay <- c(okay, NA)

  nm <- data %snin% okay
  if (length(nm) > 0)
    stop("\nThe following values are not valid: \n[", paste(nm, collapse = ", "), "]")

  return(TRUE)
}

##' Score a set of items to create overall scale score - generic
##'
##' @param data A data frame or an object coercable to a data frame.
##' @param reverse A vector the same length as the number of columns in the data
##' @param limits An optional vector indicating the lower and upper possible limits (for reversing)
##' @param mean Logical whether to calculate the mean if \code{TRUE} or sum if \code{FALSE}
##' @param reliability Logical whether or not to calculate reliability information for the scale.
##'   Defaults to \code{TRUE}.
##' @param okay A vector of okay or acceptable values
##' @param indices Indicates columns for subscales, where applicable
##' @param type A character string indicating the scale name, the type of scoring to use.
##' @param na.rm Logical whether to remove missing values or not. Defaults to \code{TRUE}
##' @param ... Additional arguments passed on to lower level functions
##' @seealso \code{\link{omega}}
##' @keywords internal
##' @importFrom psych omega
##' @importFrom data.table is.data.table
##' @name scoring
NULL

##' @rdname scoring
##' @return A list containing the results.
##'   \item{score}{The calculated scores.}
##'   \item{reliability}{Results from the omega function.}
score <- function(data, reverse = NULL, limits = NULL, mean = TRUE,
  reliability = TRUE, na.rm = TRUE, ...) {

  if (!mean && na.rm) {
    na.rm <- FALSE
    warning("Summing is not meaningful for missing values. ",
            "'na.rm' set to FALSE.", call. = FALSE)
  }

  ## Setup and appropriateness tests
  if (any(limits < 0) && !is.null(rev)) {
    stop("Cannot reverse score scale that can take on negative values.")
  }
  stopifnot(is.data.table(data) | is.data.frame(data) | is.matrix(data))
  if (is.data.table(data) || is.matrix(data)) {
    data <- as.data.frame(data)
  }
  stopifnot(unlist(lapply(data, is.numeric)))

  ## Reverse specified variables or columns
  if (!is.null(reverse)) {
    stopifnot(length(reverse) <= ncol(data))
    data[, reverse] <- (data[, reverse] * (-1)) + sum(limits)
  }

  ## Find the mean or sum
  if (mean) {
    value <- rowMeans(data, na.rm = na.rm)
  } else {
    value <- rowSums(data, na.rm = na.rm)
  }

  if (reliability) {
    reliability <- omega(m = data, plot = FALSE, ...)
  } else {
    reliability <- NULL
  }
  output <- list(score = value, reliability = reliability)

  return(output)
}

##' @rdname scoring
.scoreCESD <- function(data, okay = c(0, 1, 2, 3), reverse = c(4, 8, 12, 16), ...) {
  stopifnot(identical(ncol(data), 20L))

  QA <- sapply(data, function(X) CheckVals(X, okay = okay))
  stopifnot(QA)

  results <- score(data = data, reverse = reverse, limits = range(okay),
    mean = TRUE, ...)
  sumScore <- results$score * 20L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 20 & sumScore[use] <= max(okay) * 20)
  results$score <- data.frame(meanCESD = results$score, sumCESD = sumScore)
  ## summary(results[["reliability"]])
  return(results)
}

##' @rdname scoring
.scoreLOTR <- function(data, okay = c(1, 2, 3, 4, 5), reverse = c(2, 4, 5),
                       indices = list(
                         oindex = c(1, 3, 6),
                         pindex = c(2, 4, 5)), ...) {
  oindex <- indices$oindex
  pindex <- indices$pindex
  stopifnot(identical(ncol(data), 6L))

  QA <- sapply(data, function(X) CheckVals(X, okay = okay))
  stopifnot(QA)

  results <- score(data = data, reverse = reverse, limits = range(okay),
    mean = TRUE, ...)
  sumScore <- results$score * 6L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 6 & sumScore[use] <= max(okay) * 6)
  results$score <- data.frame(meanLOTR = results$score, sumLOTR = sumScore)

  oresults <- score(data = data[, oindex], limits = range(okay),
    mean = TRUE, ...)
  sumScore <- oresults$score * 3L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 3 & sumScore[use] <= max(okay) * 3)
  oresults$score <- data.frame(meanOptimism = oresults$score, sumOptimism = sumScore)

  presults <- score(data = data[, pindex], limits = range(okay),
    mean = TRUE, ...)
  sumScore <- presults$score * 3L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 3 & sumScore[use] <= max(okay) * 3)
  presults$score <- data.frame(meanPessimism = presults$score, sumPessimism = sumScore)
  results <- list(score = cbind(results$score, oresults$score, presults$score),
                  reliabilityLOTR = results$reliability,
                  reliabilityOptimism = oresults$reliability,
                  reliabilityPessimism = presults$reliability)
  return(results)
}

##' @rdname scoring
.scoreMastery <- function(data, okay = c(1, 2, 3, 4), reverse = c(1, 6), ...) {
  stopifnot(identical(ncol(data), 7L))

  QA <- sapply(data, function(X) CheckVals(X, okay = okay))
  stopifnot(QA)

  results <- score(data = data, reverse = reverse, limits = range(okay),
    mean = TRUE, ...)
  sumScore <- results$score * 7L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 7 & sumScore[use] <= max(okay) * 7)
  results$score <- data.frame(meanMastery = results$score, sumMastery = sumScore)
  ## summary(results[["reliability"]])
  return(results)
}

##' @rdname scoring
.scoreMOSSSS <- function(data, okay = c(1, 2, 3, 4, 5), indices = list(
    Structural = 1,
    Tangible = c(2, 5, 12, 15),
    Affectionate = c(6, 10, 20),
    PositiveInteraction = c(7, 11, 18),
    EmotionalInformational = c(3, 9, 16, 19, 4, 8, 13, 17),
    ## Composite of all subscales but Structural, additionally includes
    ## item 14 (for some reason left out of other subscales)
    Functional = 2:20), ...) {

  stopifnot(identical(ncol(data), 20L))

  QA <- sapply(data[,-1], function(X) CheckVals(X, okay = okay))
  stopifnot(QA)

  SSS <- lapply(indices, FUN = function(i) {
    score(data = data[, i, drop=FALSE], limits = range(okay),
      mean = TRUE, reliability = length(i) > 1, ...)
  })

  scores <- as.data.frame(lapply(SSS, `[[`, "score"))
  colnames(scores) <- paste0("mean", names(indices))
  reliabilities <- lapply(SSS, `[[`, "reliability")
  names(reliabilities) <- paste0("reliability", names(indices))
  results <- c(score = list(scores), reliabilities)
  return(results)
}

##' @rdname scoring
.scorePANAS <- function(data, okay = c(1, 2, 3, 4, 5),
                        indices = list(
                          pos = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19),
                          neg = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)), ...) {
  pos <- indices$pos
  neg <- indices$neg

  stopifnot(identical(ncol(data), 20L))

  QA <- sapply(data, function(X) CheckVals(X, okay = okay))
  stopifnot(QA)

  PAresults <- score(data = data[, pos], limits = range(okay),
    mean = TRUE, ...)
  sumScore <- PAresults$score * 10L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 10 & sumScore[use] <= max(okay) * 10)
  PAresults$score <- data.frame(meanPA = PAresults$score, sumPA = sumScore)

  NAresults <- score(data = data[, neg], limits = range(okay),
    mean = TRUE, ...)
  sumScore <- NAresults$score * 10L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 10 & sumScore[use] <= max(okay) * 10)
  NAresults$score <- data.frame(meanNA = NAresults$score, sumNA = sumScore)
  results <- list(score = cbind(PAresults$score, NAresults$score),
    PAreliability = PAresults$reliability,
    NAreliability = NAresults$reliability)
  return(results)
}

##' @rdname scoring
.scoreRSES <- function(data, okay = c(0, 1, 2, 3),
  reverse = c(3, 5, 8, 9, 10), ...) {
  stopifnot(identical(ncol(data), 10L))

  QA <- sapply(data, function(X) CheckVals(X, okay = okay))
  stopifnot(QA)

  results <- score(data = data, reverse = reverse, limits = range(okay),
    mean = TRUE, ...)
  sumScore <- results$score * 10L
  use <- !is.na(sumScore)
  stopifnot(sumScore[use] >= min(okay) * 10 & sumScore[use] <= max(okay) * 10)
  results$score <- data.frame(meanRSES = results$score, sumRSES = sumScore)
  return(results)
}

##' @rdname scoring
.scoreMOOD <- function(data, indices = list(
  vision = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 18, 19),
  impact = c(13, 14, 15, 16, 17, 20, 21)), ...) {
  ## Assumes that vision and impact are in the following order by default
  ## "1a" "1b" "1c" "1d" "1e" "1f" "1g" "1h" "2a" "2b" "2c" "2d" "3" "4"
  ## "2e" "2f" "2g" "2h" "2i" "5" "6"

  vision <- indices$vision
  impact <- indices$impact
  stopifnot(identical(ncol(data), 21L))

  stopifnot(identical(length(vision), 14L))
  stopifnot(identical(length(impact), 7L))

  Total <- data[, c(vision, impact)]
  ## Transform to 100 point scale
  Total[, c(vision[-14], impact[-7])] <-
    Total[, c(vision[-14], impact[-7])] * 25
  ## reverse so higher is worse
  Total[, vision[14]] <- 100 - Total[, vision[14]]
  ## Transform to 100 point scale
  Total[, impact[7]] <- Total[, impact[7]] * 100/3

  d <- !is.na(Total)

  Total[, vision][rowSums(d[, vision]) < 7, ] <- NA
  Total[, impact][rowSums(d[, impact]) < 4, ] <- NA

  Total[, vision] <- t(apply(Total[, vision], 1, function(x) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)}))
  Total[, impact] <- t(apply(Total[, impact], 1, function(x) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)}))

  dat <- list(Vision = Total[, vision],
    Impact = Total[, impact], Total = Total)

  lapply(dat, score, reliability = TRUE, na.rm = FALSE, max = 1)
}

##' @rdname scoring
##' @export
scaleScore <- function(data, type = c("CESD", "LOTR", "Mastery", "RSES", "MOSSSS", "PANAS"), ...) {
  type <- match.arg(type)

  stopifnot(is.data.table(data) | is.data.frame(data) | is.matrix(data))

  if (is.data.table(data) || is.matrix(data)) {
    data <- as.data.frame(data)
  }

  fun <- switch(type,
    CESD = .scoreCESD,
    LOTR = .scoreLOTR,
    Mastery = .scoreMastery,
    RSES = .scoreRSES,
    MOSSSS = .scoreMOSSSS,
    PANAS = .scorePANAS)
  fun(data, ...)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoring.R
\name{CheckVals}
\alias{CheckVals}
\title{Score a set of items to create overall scale score}
\usage{
CheckVals(data, okay, na.rm = TRUE)
}
\arguments{
\item{data}{The data to check}

\item{okay}{A vector of okay or acceptable values}

\item{na.rm}{Logical whether to remove missing values or not. Defaults to \code{TRUE}}
}
\value{
\code{TRUE} if all values are okay, otherwise an error
}
\description{
This function creates a single scale score from a data frame, reversing as needed.
}
\keyword{internal}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{as.na}
\alias{as.na}
\title{Coerces vectors to missing}
\usage{
as.na(x)
}
\arguments{
\item{x}{A vector to convert to missing (NA)}
}
\value{
a vector the same length as the input with missing values of the same class
}
\description{
Given a vector, convert it to missing (NA) values,
where the class of the missing matches the input class.
Currently supports character, logical, integer, factor, numeric,
times (from \pkg{chron}), Date, POSIXct, POSIXlt, and
zoo (from \pkg{zoo}), and haven labelled from \pkg{haven}.
}
\examples{
str(as.na(1L:5L))
str(as.na(rnorm(5)))
str(as.na(c(TRUE, FALSE)))
str(as.na(as.Date("2017-01-01")))
}
\keyword{utils}

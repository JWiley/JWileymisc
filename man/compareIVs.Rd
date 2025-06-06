% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{compareIVs}
\alias{compareIVs}
\title{Compares the effects of various independent variables on dependent variables}
\usage{
compareIVs(
  dv,
  type,
  iv,
  covariates = character(),
  data,
  multivariate = FALSE,
  ...
)
}
\arguments{
\item{dv}{A character string or vector of the depentent variable(s)}

\item{type}{A character string or vector indicating the type of 
dependent variable(s)}

\item{iv}{A character string or vector giving the IV(s)}

\item{covariates}{A character string or vector giving the covariate(s)}

\item{data}{The data to be used for analysis}

\item{multivariate}{A logical value whether to have models with all 
IVs simultaneously.}

\item{\ldots}{Additional arguments passed on to the internal function, 
\code{.runIt}.}
}
\value{
A list with all the model results.
}
\description{
Utility to estimate the unadjusted, covariate adjusted, and 
multivariate adjusted unique contributions of one or more IVs 
on one or more DVs
}
\examples{
test1 <- compareIVs(
  dv = c("mpg", "disp"),
  type = c("normal", "normal"),
  iv = c("hp", "qsec"),
  covariates = "am",
  data = mtcars, multivariate = TRUE)
test1$OverallSummary
rm(test1)
}

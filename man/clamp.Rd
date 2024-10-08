% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{clamp}
\alias{clamp}
\title{Clamp a value to a range}
\usage{
clamp(x, lower = -Inf, upper = Inf)
}
\arguments{
\item{x}{(\code{numeric()}) A numeric value to be clamped.}

\item{lower}{('numeric(1)') The lower bound of the range.}

\item{upper}{(\code{numeric(1)}) The upper bound of the range.}
}
\value{
A numeric value clamped to the range [\code{lower}, \code{upper}].
}
\description{
Clamp a value to a range
}
\keyword{internal}

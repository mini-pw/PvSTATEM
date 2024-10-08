% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes-plate.R
\name{is_valid_data_type}
\alias{is_valid_data_type}
\title{Check validity of given data type}
\usage{
is_valid_data_type(data_type)
}
\arguments{
\item{data_type}{A string representing the data type.}
}
\value{
\code{TRUE} if the data type is valid, \code{FALSE} otherwise.
}
\description{
Check if the data type is valid. The data type is valid if it is one of the
elements of the \code{VALID_DATA_TYPES} vector. The valid data types are:
\cr \code{c(Median, Net MFI, Count, Avg Net MFI, Mean, Peak)}.
}

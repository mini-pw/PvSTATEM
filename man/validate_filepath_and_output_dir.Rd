% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{validate_filepath_and_output_dir}
\alias{validate_filepath_and_output_dir}
\title{Validate filepath and output_dir}
\usage{
validate_filepath_and_output_dir(
  filename,
  output_dir,
  plate_name,
  suffix,
  extension,
  verbose = TRUE
)
}
\arguments{
\item{filename}{(\code{character(1)}) The path to the file.}

\item{output_dir}{(\code{character(1)}) The directory where the file should be saved.}

\item{plate_name}{(\code{character(1)}) The name of the plate.}

\item{suffix}{(\code{character(1)}) The suffix to be added to the filename if it is not provided, e.g. \code{RAU}.}

\item{extension}{(\code{character(1)}) The extension to be added to the filename if it does not have one.
Passed without a dot, e.g. \code{csv}.}

\item{verbose}{(\code{logical(1)}) A logical value indicating whether the function should print additional information.}
}
\value{
An absolute output path.
}
\description{
This function validates the filepath and output_dir arguments.
}
\keyword{internal}

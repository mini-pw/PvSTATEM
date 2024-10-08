% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process-plate.R
\name{process_plate}
\alias{process_plate}
\title{Process a plate and save computed dilutions to a CSV}
\usage{
process_plate(
  plate,
  output_path = NULL,
  data_type = "Median",
  adjust_blanks = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{plate}{(\code{Plate()}) a plate object}

\item{output_path}{(\code{character(1)}) path to save the computed dilutions.
If not provided the file will be saved in the working directory with the name \verb{dilutions_\{plate_name\}.csv}.
Where the \code{{plate_name}} is the name of the plate.}

\item{data_type}{(\code{character(1)}) type of data to use for the computation. Median is the default}

\item{adjust_blanks}{(\code{logical(1)}) adjust blanks before computing dilutions. Default is \code{FALSE}}

\item{verbose}{(\code{logical(1)}) print additional information. Default is \code{TRUE}}

\item{...}{Additional arguments to be passed to the fit model function (\code{create_standard_curve_model_analyte})}
}
\description{
The behavior can be summarized as follows:
\enumerate{
\item Adjust blanks if not already done.
\item Fit a model to each analyte using standard curve samples.
\item Compute dilutions for each analyte using the corresponding model.
\item Aggregate computed dilutions into a single data frame.
\item Save the computed dilutions to a CSV file.
}
}
\examples{

plate_file <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM")
layout_file <- system.file("extdata", "CovidOISExPONTENT_layout.csv", package = "PvSTATEM")

plate <- read_luminex_data(plate_file, layout_file)

tmp_dir <- tempdir(check = TRUE)
temporary_filepath <- file.path(tmp_dir, "output.csv")
process_plate(plate, output_path = temporary_filepath)
# create and save dataframe with computed dilutions

}

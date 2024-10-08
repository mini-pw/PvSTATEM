% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes-model.R
\name{create_standard_curve_model_analyte}
\alias{create_standard_curve_model_analyte}
\title{Create a standard curve model for a certain analyte}
\usage{
create_standard_curve_model_analyte(
  plate,
  analyte_name,
  data_type = "Median",
  source_mfi_range_from_all_analytes = FALSE,
  ...
)
}
\arguments{
\item{plate}{(\code{Plate()})
Object of the Plate class}

\item{analyte_name}{(\code{character(1)})
Name of the analyte for which we want to create the model}

\item{data_type}{(\code{character(1)})
Data type of the value we want to use to fit the model - the same datatype as in the plate file. By default, it equals to \code{Median}}

\item{source_mfi_range_from_all_analytes}{(\code{logical(1)})
If \code{TRUE}, the MFI range is calculated from all analytes; if \code{FALSE}, the MFI range is calculated only for the current analyte
Defaults to \code{FALSE}}

\item{...}{Additional arguments passed to the model}
}
\value{
(\code{Model()}) Standard Curve model
}
\description{
Create a standard curve model for a certain analyte
}

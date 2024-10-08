% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-plate.R
\name{plot_plate}
\alias{plot_plate}
\title{Plot a 96-well plate with colored wells}
\usage{
plot_plate(
  colors,
  plot_numbers = FALSE,
  numbers = NULL,
  plot_title = "Plate",
  plot_legend = FALSE,
  legend_mapping = NULL
)
}
\arguments{
\item{colors}{A vector with 96 colors that will be used to color the wells, order is from left to right and top to bottom}

\item{plot_numbers}{Logical value indicating if the well numbers should be plotted, default is \code{FALSE}}

\item{numbers}{An optional vector with 96 numbers, plotted on the wells. Order is from left to right and top to bottom, must have the same length as colors. It could be used, for instance, to plot the bead count of each well. Must be provided in case the \code{plot_numbers} parameters is set to \code{TRUE}}

\item{plot_title}{The title of the plot (default is "Plate")}

\item{plot_legend}{Logical value indicating if the legend should be plotted, default is \code{FALSE}}

\item{legend_mapping}{A named vector with the colors mapping used to create the legend}
}
\value{
A ggplot object
}
\description{
It is a generic function to plot a 96-well plate with colored wells
used by other functions in this package, mainly to plot layout and counts.
The function uses a background image of a 96-well plate and
plots the colors in the wells using ggplot2.
This function is not intended to be used directly by the user.
Rather, it is used by other functions, specified in this file.
}
\keyword{internal}

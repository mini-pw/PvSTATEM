#' @title Plot Levey-Jennings chart
#'
#' @description
#' The function plots a Levey-Jennings chart for the given analyte
#' in the list of plates. The Levey-Jennings chart is a graphical
#' representation of the data that enables the detection of outliers
#' and trends. It is a quality control tool that is widely used
#' in the laboratories across the world.
#'
#'
#'
#' @param list_of_plates A list of plate objects for which to plot the
#' Levey-Jennings chart
#' @param analyte_name (`character(1)`) the analyte for which to plot the
#' Levey-Jennings chart
#' @param sd_lines (`numeric`) the vector of coefficients for the
#' standard deviation lines to plot, for example, c(1.96, 2.58)
#' will plot four horizontal lines: mean +/- 1.96*sd, mean +/- 2.58*sd
#' default is c(1.96) which will plot two lines mean +/- 1.96*sd
#' @param data_type (`character(1)`) the type of data used plot. The default is "Median"
#'
#' @return A ggplot object with the Levey-Jennings chart
#'
#' @export
plot_levey_jennings <- function(list_of_plates,
                                analyte_name,
                                dilution = "1/400",
                                sd_lines = c(1.96),
                                data_type = "Median") {
  stopifnot(is.list(list_of_plates))
  stopifnot(length(list_of_plates) > 0)
  stopifnot(all(sapply(list_of_plates, inherits, "Plate")))

  stopifnot(is.character(analyte_name))
  stopifnot(all(sapply(list_of_plates, function(plate) analyte_name %in% plate$analyte_names)))

  stopifnot(is.character(dilution))
  stopifnot(all(sapply(list_of_plates, function(plate) dilution %in% plate$get_dilution("STANDARD CURVE"))))

  stopifnot(is.numeric(sd_lines))

  date_of_experiment <- c()
  mfi_values <- c()
  for (plate in list_of_plates) {
    dilutions <- plate$get_dilution("STANDARD CURVE")
    data <- plate$get_data(analyte_name, "STANDARD CURVE", data_type)

    date_of_experiment <- c(date_of_experiment, plate$plate_datetime)
    mfi_values <- c(mfi_values, data[dilutions == dilution])
  }

  mean <- mean(mfi_values)
  sd <- sd(mfi_values)
}
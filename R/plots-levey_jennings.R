#' @title Plot Levey-Jennings chart
#'
#' @description
#' The function plots a Levey-Jennings chart for the given analyte in the list of plates.
#' The Levey-Jennings chart is a graphical representation of the data that enables the detection of outliers and trends.
#' It is a quality control tool that is widely used in the laboratories across the world.
#'
#'
#'
#' @param list_of_plates  A list of plate objects for which to plot the Levey-Jennings chart
#' @param analyte_name (`character(1)`) the analyte for which to plot the Levey-Jennings chart
#'
#' @return A ggplot object with the Levey-Jennings chart
#'
#' @export
plot_levey_jennings <- function(list_of_plates, analyte_name) {
  stopifnot(is.list(list_of_plates))
  stopifnot(length(list_of_plates) > 0)
  stopifnot(all(sapply(list_of_plates, inherits, "Plate")))

  stopifnot(is.character(analyte_name))
  stopifnot(all(sapply(list_of_plates, function(plate) analyte_name %in% plate$analyte_names)))


}
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
#' @param dilution (`character(1)`) the dilution for which to plot the
#' Levey-Jennings chart. The default is "1/400"
#' @param sd_lines (`numeric`) the vector of coefficients for the
#' standard deviation lines to plot, for example, c(1.96, 2.58)
#' will plot four horizontal lines: mean +/- 1.96*sd, mean +/- 2.58*sd
#' default is c(1.96) which will plot two lines mean +/- 1.96*sd
#' @param data_type (`character(1)`) the type of data used plot. The default is "Median"
#'
#' @importFrom stats setNames
#'
#' @return A ggplot object with the Levey-Jennings chart
#'
#' @examples
#' # creating temporary directory for the example
#' output_dir <- tempdir(check = TRUE)
#'
#' dir_with_luminex_files <- system.file("extdata", "multiplate_reallife_reduced",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' list_of_plates <- process_dir(dir_with_luminex_files,
#'   return_plates = TRUE, format = "xPONENT", output_dir = output_dir
#' )
#' list_of_plates <- rep(list_of_plates, 10) # since we have only 3 plates i will repeat them 10 times
#'
#' plot_levey_jennings(list_of_plates, "ME", dilution = "1/400", sd_lines = c(0.5, 1, 1.96, 2.58))
#'
#' @export
plot_levey_jennings <- function(list_of_plates,
                                analyte_name,
                                dilution = "1/400",
                                sd_lines = c(1.96),
                                data_type = "Median") {
  if (!is.list(list_of_plates)) {
    stop("The list_of_plates is not a list.")
  }
  if (length(list_of_plates) == 0) {
    stop("The list_of_plates is empty.")
  }
  if (length(list_of_plates) <= 10) {
    warning("The number of plates is less than 10. For the Levey-Jennings chart it is recommended to have at least 10 plates.")
  }
  if (!all(sapply(list_of_plates, inherits, "Plate"))) {
    stop("The list_of_plates contains objects that are not of class Plate.")
  }
  if (!is.character(analyte_name)) {
    stop("The analyte_name is not a string.")
  }
  if (!all(sapply(list_of_plates, function(plate) analyte_name %in% plate$analyte_names))) {
    plate_where_analyte_is_missing <- which(sapply(list_of_plates, function(plate) !(analyte_name %in% plate$analyte_names)))
    stop("The analyte_name is not present in plates ", paste(plate_where_analyte_is_missing, collapse = ", "))
  }
  if (!is.character(dilution)) {
    stop("The dilution is not a string.")
  }
  if (!all(sapply(list_of_plates, function(plate) dilution %in% plate$get_dilution("STANDARD CURVE")))) {
    plate_where_dilution_is_missing <- which(sapply(list_of_plates, function(plate) !(dilution %in% plate$get_dilution("STANDARD CURVE"))))
    stop("The dilution is not present in plates ", paste(plate_where_dilution_is_missing, collapse = ", "))
  }
  if (!is.numeric(sd_lines)) {
    stop("The sd_lines is not a numeric vector.")
  }
  if (length(sd_lines) > 6) {
    stop("It is impossible to have more than 6 pairs of standard deviation lines.")
  }

  date_of_experiment <- c()
  mfi_values <- c()
  for (plate in list_of_plates) {
    dilutions <- plate$get_dilution("STANDARD CURVE")
    plate_data <- plate$get_data(analyte_name, "STANDARD CURVE", data_type)

    date_of_experiment <- c(date_of_experiment, plate$plate_datetime)
    mfi_values <- c(mfi_values, plate_data[dilutions == dilution])
  }
  counter <- seq(1, length(mfi_values))

  mean <- mean(mfi_values)
  sd <- sd(mfi_values)

  plot_data <- data.frame(date = date_of_experiment, mfi = mfi_values, counter = counter)
  p <- ggplot2::ggplot(data = plot_data, aes(x = counter, y = .data$mfi)) +
    ggplot2::geom_point(size = 3, colour = "blue") +
    ggplot2::geom_line(size = 1.3, colour = "blue") +
    ggplot2::geom_hline(yintercept = mean, color = "black", size = 1) +
    ggplot2::labs(
      title = paste("Levey-Jennings chart for", analyte_name, "at", dilution, "dilution"),
      x = "Control measurement number",
      y = "MFI"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = 9, vjust = 1),
      axis.text.y = element_text(size = 9),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_blank(),
      panel.grid.minor = element_line(color = scales::alpha("grey", .5), size = 0.1) # Make the minor grid lines less visible
    ) +
    ggplot2::scale_x_continuous(breaks = plot_data$counter, labels = plot_data$counter) # Add custom x-axis labels

  line_types <- c("dashed", "dotted", "dotdash", "longdash", "twodash", "1F")
  line_labels <- c()
  line_level <- c()
  counter <- 1
  # Add standard deviation lines
  for (sd_line in sd_lines) {
    line_labels <- c(line_labels, paste0("Mean +/- ", sd_line, " SD"))
    line_level <- c(line_level, mean + sd_line * sd)
    p <- p + ggplot2::geom_hline(yintercept = mean - sd_line * sd, linetype = line_types[counter])
    counter <- counter + 1
  }

  sd_lines_df <- data.frame(yintercept = line_level, label = line_labels)
  p <- p + ggplot2::geom_hline(
    data = sd_lines_df,
    aes(yintercept = .data$yintercept, linetype = .data$label),
    color = "black"
  )
  p <- p + ggplot2::scale_linetype_manual(
    values = setNames(line_types[seq_along(line_labels)], line_labels)
  )

  return(p)
}

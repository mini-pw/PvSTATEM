#' @title Standard curves
#'
#' @description
#' Plot standard curve samples of a plate of a given analyte.
#'
#' @param plate A plate object
#'
#' @param analyte_name Name of the analyte of which standard curve we want to plot.
#' @param data_type Data type of the value we want to plot - the same datatype as in the plate file. By default equals to `Net MFI`
#' @param decreasing_dilution_order If `TRUE` the dilutions are plotted in decreasing order, `TRUE` by default
#' @param log_scale Which elements on the plot should be displayed in log scale. By default `"dilutions"`. If `NULL` or `c()` no log scale is used, if `"all"` or `c("dilutions", "MFI")` all elements are displayed in log scale.
#' @param plot_line If `TRUE` a line is plotted, `TRUE` by default
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @return ggplot object with the plot
#'
#' @import ggplot2
#'
#' @export
plot_standard_curve_analyte <- function(plate, analyte_name,
                                        data_type = "Median", decreasing_dilution_order = TRUE,
                                        log_scale = c("dilutions"), plot_line = TRUE, verbose = TRUE) {
  AVAILABLE_LOG_SCALE_VALUES <- c("all", "dilutions", "MFI")

  if (!inherits(plate, "Plate")) {
    stop("plate object should be a Plate")
  }
  if (!is.null(log_scale) && !all(log_scale %in% AVAILABLE_LOG_SCALE_VALUES)) {
    stop("log_scale should be a character vector containing elements from set: ", paste(AVAILABLE_LOG_SCALE_VALUES, collapse = ", ", "\nInstead passed: ", log_scale))
  }
  if (!(analyte_name %in% plate$analyte_names)) {
    stop(analyte_name, " not found in the plate object")
  }

  plot_name <- paste0("Sample values of standard curve for analyte: ", analyte_name)
  plot_data <- data.frame(
    MFI = plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type),
    plate = plate$plate_name,
    dilution_values = plate$get_dilution_values("STANDARD CURVE"),
    dilutions = plate$get_dilution("STANDARD CURVE")
  )

  # Scale x and y if needed
  x_log_scale <- "dilutions" %in% log_scale || "all" %in% log_scale
  y_log_scale <- "MFI" %in% log_scale || "all" %in% log_scale
  x_trans <- ifelse(x_log_scale, "log10", "identity")
  x_cords_trans <- ifelse(decreasing_dilution_order, "reverse", "identity")
  y_trans <- ifelse(y_log_scale, "log10", "identity")

  xlab <- ifelse(x_log_scale, "dilution (log scale)", "dilution")
  x_ticks <- c(plot_data$dilution_values, max(plot_data$dilution_values) + 1)
  x_labels <- c(plot_data$dilutions, "")
  ylab <- ifelse(y_log_scale, paste(data_type, "(log scale)"), data_type)

  # Automatically position the legend
  legend_position <- c(0.8, 0.2)
  if (decreasing_dilution_order) {
    if (x_log_scale && !y_log_scale) {
      legend_position <- c(0.8, 0.8)
    } else {
      legend_position <- c(0.2, 0.2)
    }
  } else {
    if (x_log_scale && !y_log_scale) {
      legend_position <- c(0.2, 0.8)
    } else {
      legend_position <- c(0.8, 0.2)
    }
  }

  options(scipen = 30)
  p <- ggplot2::ggplot(plot_data, aes(x = dilution_values, y = MFI))
  if (plot_line) {
    p <- p + geom_line(linewidth = 1.2, color = "blue")
  }
  p <- p + geom_point(size = 3, color = "blue") +
    labs(title = plot_name, x = xlab, y = ylab) +
    scale_x_continuous(
      breaks = x_ticks, labels = x_labels,
      trans = x_trans
    ) +
    scale_y_continuous(trans = y_trans) +
    coord_trans(x = x_cords_trans) +
    theme_minimal() +
    theme(
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      legend.position.inside = legend_position,
      legend.background = element_rect(fill = "white", color = "black")
    )

  p
}


#' Plot standard curve of a certain analyte with fitted model
#'
#' @param plate Plate object
#' @param analyte_name Name of the analyte for which we want to plot the standard curve - the same for which the model was fitted
#' @param model nplr object with the model
#' @param data_type Data type of the value we want to plot - the same datatype as in the plate file. By default equals to `Median`
#' @param decreasing_dilution_order If `TRUE` the dilutions are plotted in decreasing order, `TRUE` by default.
#' @param log_scale Which elements on the plot should be displayed in log scale. By default `"all"`. If `NULL` or `c()` no log scale is used, if `"all"` or `c("dilutions", "MFI")` all elements are displayed in log scale.
#' @param plot_asymptote If `TRUE` the asymptotes are plotted, `TRUE` by default
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @return a ggplot object with the plot
#'
#' @description
#' Function plots the values of standard curve samples and the fitted model.
#'
#'
#' @import ggplot2
#'
#' @export
plot_standard_curve_analyte_with_model <- function(plate, analyte_name, model, data_type = "Median", decreasing_dilution_order = TRUE, log_scale = c("all"), plot_asymptote = TRUE, verbose = TRUE) {
  p <- plot_standard_curve_analyte(
    plate,
    analyte_name = analyte_name, data_type = data_type,
    decreasing_dilution_order = decreasing_dilution_order,
    log_scale = log_scale, verbose = verbose, plot_line = FALSE
  )

  plot_name <- paste0("Fitted standard curve for analyte: ", analyte_name)
  p$labels$title <- plot_name

  estimates <- model$get_plot_data()
  p <- p + geom_line(
    aes(x = dilution, y = MFI),
    color = "red", data = estimates, linewidth = 1
  )
  if (plot_asymptote) {
    p <- p + geom_hline(
      yintercept = model$top_asymptote, linetype = "dashed", color = "gray"
    ) +
      geom_hline(
        yintercept = model$bottom_asymptote, linetype = "dashed", color = "gray"
      )
  }
  return(p)
}

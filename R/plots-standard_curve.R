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
#' @param plot_blank_mean If `TRUE` the mean of the blank samples is plotted, `TRUE` by default
#' @param plot_dilution_bounds If `TRUE` the dilution bounds are plotted, `TRUE` by default
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @return ggplot object with the plot
#'
#' @import ggplot2
#'
#' @export
plot_standard_curve_analyte <- function(plate,
                                        analyte_name,
                                        data_type = "Median",
                                        decreasing_dilution_order = TRUE,
                                        log_scale = c("dilutions"),
                                        plot_line = TRUE,
                                        plot_blank_mean = TRUE,
                                        plot_dilution_bounds = TRUE,
                                        verbose = TRUE) {
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
  blank_mean <- mean(plate$get_data(analyte_name, "BLANK", data_type = data_type))


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
  p <- ggplot2::ggplot(plot_data, aes(x = dilution_values, y = MFI)) +
    ggplot2::geom_point(aes(color = "Standard curve samples"), size = 3)
  if (plot_line) {
    p <- p + ggplot2::geom_line(aes(color = "Standard curve samples"), linewidth = 1.2)
  }
  if (plot_blank_mean) {
    p <- p + ggplot2::geom_hline(
      aes(yintercept = blank_mean, color = "Blank mean"),
      linetype = "solid"
    )
  }
  if (plot_dilution_bounds) {
    p <- p + ggplot2::geom_vline(
      ggplot2::aes(color = "Min-max dilution bounds", xintercept = min(dilution_values)),
      linetype = "dashed"
    ) + ggplot2::geom_vline(
      ggplot2::aes(color = "Min-max dilution bounds", xintercept = max(dilution_values)),
      linetype = "dashed"
    )
  }
  p <- p + ggplot2::labs(title = plot_name, x = xlab, y = ylab) +
    ggplot2::scale_x_continuous(
      breaks = x_ticks, labels = x_labels,
      trans = x_trans
    ) +
    ggplot2::scale_y_continuous(trans = y_trans) +
    ggplot2::coord_trans(x = x_cords_trans) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      legend.position.inside = legend_position,
      legend.background = element_rect(fill = "white", color = "black")
    ) +
    ggplot2::scale_color_manual(
      values = c("Standard curve samples" = "blue", "Blank mean" = "red", "Min-max dilution bounds" = "gray")
    ) +
    ggplot2::guides(color = guide_legend(title = "Plot object"))

  p
}


#' Plot standard curve of a certain analyte with fitted model
#'
#' @param plate Plate object
#' @param model fitted `Model` object, which predictions we want to plot
#' @param data_type Data type of the value we want to plot - the same datatype as in the plate file. By default equals to `Median`
#' @param decreasing_dilution_order If `TRUE` the dilutions are plotted in decreasing order, `TRUE` by default.
#' @param log_scale Which elements on the plot should be displayed in log scale. By default `"all"`. If `NULL` or `c()` no log scale is used, if `"all"` or `c("dilutions", "MFI")` all elements are displayed in log scale.
#' @param plot_asymptote If `TRUE` the asymptotes are plotted, `TRUE` by default
#' @param plot_test_predictions If `TRUE` the predictions for the test samples are plotted, `TRUE` by default
#' The predictions are obtained through extrapolation of the model
#' @param plot_blank_mean If `TRUE` the mean of the blank samples is plotted, `TRUE` by default
#' @param plot_dilution_bounds If `TRUE` the dilution bounds are plotted, `TRUE` by default
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
plot_standard_curve_analyte_with_model <- function(plate,
                                                   model,
                                                   data_type = "Median",
                                                   decreasing_dilution_order = TRUE,
                                                   log_scale = c("all"),
                                                   plot_asymptote = TRUE,
                                                   plot_test_predictions = TRUE,
                                                   plot_blank_mean = TRUE,
                                                   plot_dilution_bounds = TRUE,
                                                   verbose = TRUE) {
  analyte_name <- model$analyte
  if (!inherits(model, "Model")) {
    stop("model object should be a Model")
  }
  if (is.null(analyte_name)) {
    stop("analyte name should be provided in the model object. Try to fit the model first.")
  }

  p <- plot_standard_curve_analyte(
    plate,
    analyte_name = analyte_name, data_type = data_type,
    decreasing_dilution_order = decreasing_dilution_order,
    log_scale = log_scale, verbose = verbose, plot_line = FALSE,
    plot_blank_mean = plot_blank_mean, plot_dilution_bounds = plot_dilution_bounds
  )

  plot_name <- paste0("Fitted standard curve for analyte: ", analyte_name)
  p$labels$title <- plot_name

  test_samples_mfi <- plate$get_data(analyte_name, "TEST", data_type = data_type)
  test_sample_estimates <- predict(model, test_samples_mfi)

  p <- p + ggplot2::geom_line(
    ggplot2::aes(x = dilution, y = MFI, color = "Fitted model predictions"),
    data = model$get_plot_data(), linewidth = 1
  )
  if (plot_test_predictions) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = dilution, y = MFI, color = "Test sample predictions"),
      data = test_sample_estimates, shape = 4,
      size = 3
    )
  }

  if (plot_asymptote) {
    p <- p + ggplot2::geom_hline(
      ggplot2::aes(yintercept = model$top_asymptote, color = "Asymptotes"),
      linetype = "dashed"
    ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = model$bottom_asymptote, color = "Asymptotes"),
        linetype = "dashed"
      )
  }
  p <- p + ggplot2::scale_color_manual(
    values = c(
      "Standard curve samples" = "blue", "Blank mean" = "red", "Min-max dilution bounds" = "gray",
      "Fitted model" = "green", "Asymptotes" = "gray", "Test sample predictions" = "dark green"
    )
  )
  return(p)
}

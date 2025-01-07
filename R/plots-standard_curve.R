#' @title Standard curves
#'
#' @description
#' Plot standard curve samples of a plate of a given analyte.
#'
#' @param plate A plate object
#' @param analyte_name Name of the analyte of which standard curve we want to plot.
#' @param data_type Data type of the value we want to plot - the same datatype as in the plate file. By default equals to `Net MFI`
#' @param decreasing_rau_order If `TRUE` the RAU values are plotted in decreasing order, `TRUE` by default
#' @param log_scale Which elements on the plot should be displayed in log scale. By default `"RAU"`. If `NULL` or `c()` no log scale is used, if `"all"` or `c("RAU", "MFI")` all elements are displayed in log scale.
#' @param plot_line If `TRUE` a line is plotted, `TRUE` by default
#' @param plot_blank_mean If `TRUE` the mean of the blank samples is plotted, `TRUE` by default
#' @param plot_rau_bounds If `TRUE` the RAU values bounds are plotted, `TRUE` by default
#' @param plot_legend If `TRUE` the legend is plotted, `TRUE` by default
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @return ggplot object with the plot
#'
#' @examples
#' path <- system.file("extdata", "CovidOISExPONTENT.csv",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' plate <- read_luminex_data(path, layout_filepath = layout_path, verbose = FALSE)
#' plot_standard_curve_analyte(plate, "Spike_6P", plot_legend = FALSE, data_type = "Median")
#'
#' @import ggplot2
#'
#' @export
plot_standard_curve_analyte <- function(plate,
                                        analyte_name,
                                        data_type = "Median",
                                        decreasing_rau_order = TRUE,
                                        log_scale = c("all"),
                                        plot_line = TRUE,
                                        plot_blank_mean = TRUE,
                                        plot_rau_bounds = TRUE,
                                        plot_legend = TRUE,
                                        verbose = TRUE) {
  AVAILABLE_LOG_SCALE_VALUES <- c("all", "RAU", "MFI")

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
    RAU = dilution_to_rau(plate$get_dilution_values("STANDARD CURVE"))
  )
  blank_mean <- mean(plate$get_data(analyte_name, "BLANK", data_type = data_type))


  # Scale x and y if needed
  x_log_scale <- "RAU" %in% log_scale || "all" %in% log_scale
  y_log_scale <- "MFI" %in% log_scale || "all" %in% log_scale
  x_trans <- ifelse(x_log_scale, "log10", "identity")
  x_cords_trans <- ifelse(decreasing_rau_order, "reverse", "identity")
  y_trans <- ifelse(y_log_scale, "log10", "identity")

  xlab <- ifelse(x_log_scale, "RAU (log scale)", "RAU")
  x_ticks <- c(plot_data$RAU, max(plot_data$RAU) + 1)
  x_labels <- c(sprintf("%0.2f", plot_data$RAU), "")
  ylab <- ifelse(y_log_scale, paste("MFI ", data_type, "(log scale)"), paste("MFI ", data_type))

  # Automatically position the legend
  legend_position <- c(0.8, 0.2)
  if (decreasing_rau_order) {
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
  p <- ggplot2::ggplot(plot_data, aes(x = .data$RAU, y = .data$MFI)) +
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
  if (plot_rau_bounds) {
    p <- p + ggplot2::geom_vline(
      ggplot2::aes(color = "Min-max RAU bounds", xintercept = min(.data$RAU)),
      linetype = "dashed"
    ) + ggplot2::geom_vline(
      ggplot2::aes(color = "Min-max RAU bounds", xintercept = max(.data$RAU)),
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
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_blank()
    ) +
    ggplot2::scale_color_manual(
      values = c("Standard curve samples" = "blue", "Blank mean" = "red", "Min-max RAU bounds" = "gray")
    )

  if (!plot_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}


#' Plot standard curve of a certain analyte with fitted model
#'
#' @description
#' Function plots the values of standard curve samples and the fitted model.
#'
#' @param plate Plate object
#' @param model fitted `Model` object, which predictions we want to plot
#' @param data_type Data type of the value we want to plot - the same
#' datatype as in the plate file. By default equals to `Median`
#' @param decreasing_rau_order If `TRUE` the RAU values are plotted in
#' decreasing order, `TRUE` by default.
#' @param log_scale Which elements on the plot should be displayed in log scale.
#' By default `"all"`. If `NULL` or `c()` no log scale is used,
#' if `"all"` or `c("RAU", "MFI")` all elements are displayed in log scale.
#' @param plot_asymptote If `TRUE` the asymptotes are plotted, `TRUE` by default
#' @param plot_test_predictions If `TRUE` the predictions for the test samples are plotted, `TRUE` by default.
#' The predictions are obtained through extrapolation of the model
#' @param plot_blank_mean If `TRUE` the mean of the blank samples is plotted, `TRUE` by default
#' @param plot_rau_bounds If `TRUE` the RAU bounds are plotted, `TRUE` by default
#' @param plot_legend If `TRUE` the legend is plotted, `TRUE` by default
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#' @param ... Additional arguments passed to the `predict` function
#'
#' @return a ggplot object with the plot
#
#' @examples
#' path <- system.file("extdata", "CovidOISExPONTENT.csv",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' layout_path <- system.file("extdata", "CovidOISExPONTENT_layout.xlsx",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' plate <- read_luminex_data(path, layout_filepath = layout_path, verbose = FALSE)
#' model <- create_standard_curve_model_analyte(plate, analyte_name = "Spike_B16172")
#' plot_standard_curve_analyte_with_model(plate, model, decreasing_rau_order = FALSE)
#'
#' @import ggplot2
#'
#' @export
plot_standard_curve_analyte_with_model <- function(plate,
                                                   model,
                                                   data_type = "Median",
                                                   decreasing_rau_order = TRUE,
                                                   log_scale = c("all"),
                                                   plot_asymptote = TRUE,
                                                   plot_test_predictions = TRUE,
                                                   plot_blank_mean = TRUE,
                                                   plot_rau_bounds = TRUE,
                                                   plot_legend = TRUE,
                                                   verbose = TRUE,
                                                   ...) {
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
    decreasing_rau_order = decreasing_rau_order,
    log_scale = log_scale, verbose = verbose, plot_line = FALSE,
    plot_blank_mean = plot_blank_mean, plot_rau_bounds = plot_rau_bounds,
    plot_legend = plot_legend
  )

  plot_name <- paste0("Fitted standard curve for analyte: ", analyte_name)
  p$labels$title <- plot_name

  test_samples_mfi <- plate$get_data(analyte_name, "TEST", data_type = data_type)
  test_sample_estimates <- predict(model, test_samples_mfi, ...)


  if (plot_test_predictions) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = .data$RAU, y = .data$MFI, color = "Test sample predictions"),
      data = test_sample_estimates, shape = 4,
      size = 2.2,
      stroke = 1.3,
      alpha = 0.8
    )
  }

  p <- p + ggplot2::geom_line(
    ggplot2::aes(x = .data$RAU, y = .data$MFI, color = "Fitted model predictions"),
    data = model$get_plot_data(), linewidth = 1
  )

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
      "Standard curve samples" = "blue", "Blank mean" = "red", "Min-max RAU bounds" = "gray",
      "Fitted model" = "green", "Asymptotes" = "gray", "Test sample predictions" = "dark green"
    )
  )
  return(p)
}


#' @title Standard curve thumbnail for report
#'
#' @description
#' Function generates a thumbnail of the standard curve for a given analyte.
#' The thumbnail is used in the plate report. It doesn't have any additional
#' parameters, because it is used only internally.
#'
#' @param plate Plate object
#' @param analyte_name Name of the analyte of which standard curve we want to plot.
#' @param data_type Data type of the value we want to plot - the same
#' types as in the plate file. By default equals to `median`
#'
#' @return ggplot object with the plot
#'
#' @keywords internal
plot_standard_curve_thumbnail <- function(plate,
                                          analyte_name,
                                          data_type = "Median") {
  if (!inherits(plate, "Plate")) {
    stop("plate object should be a Plate")
  }
  if (!(analyte_name %in% plate$analyte_names)) {
    stop(analyte_name, " not found in the plate object")
  }
  plot_data <- data.frame(
    MFI = plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type),
    plate = plate$plate_name,
    RAU = dilution_to_rau(plate$get_dilution_values("STANDARD CURVE"))
  )
  blank_mean <- mean(plate$get_data(analyte_name, "BLANK", data_type = data_type))
  x_ticks <- c(plot_data$RAU, max(plot_data$RAU) + 1)
  x_labels <- c(sprintf("%0.2f", plot_data$RAU), "")



  p <- ggplot2::ggplot(plot_data, aes(x = .data$RAU, y = .data$MFI)) +
    ggplot2::geom_point(aes(color = "Standard curve samples"), size = 9) +
    ggplot2::geom_hline(
      aes(yintercept = blank_mean, color = "Blank mean"),
      linetype = "solid", linewidth = 1.8
    ) +
    ggplot2::labs(title = analyte_name, x = "", y = "") +
    ggplot2::scale_x_continuous(
      breaks = x_ticks, labels = x_labels,
      trans = "log10"
    ) +
    # ggplot2::scale_y_continuous(trans = "log10") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line = element_line(colour = "black", size = 2),
      axis.text.x = element_text(size = 0),
      axis.text.y = element_text(size = 0),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 50),
      panel.grid.minor = element_blank(),
    ) +
    ggplot2::coord_trans(x = "reverse") +
    ggplot2::scale_color_manual(
      values = c("Standard curve samples" = "blue", "Blank mean" = "red", "Min-max RAU bounds" = "gray")
    )
  p
}

#' @title Standard curve stacked plot for levey-jennings report
#'
#' @description
#' Function generates a plot of stacked on top of each other standard curves
#' for a given analyte form a list of plates. The plot is created with the
#' levey-jennings report in mind, but it can be run by itself.
#'
#' @param list_of_plates list of Plate objects
#' @param analyte_name Name of the analyte of which standard curves we want to plot.
#' @param data_type Data type of the value we want to plot - the same
#' datatype as in the plate file. By default equals to `Median`
#' @param monochromatic If `TRUE` the color of standard curves changes
#' from white (the oldest) to blue (the newest) it helps to observe drift in
#' calibration of the device; otherwise, more varied colours are used, `TRUE` by default
#' @param legend_type default value is `NULL`, then legend type is determined
#' based on monochromatic value. If monochromatic is equal to `TRUE` then legend
#' type is set to `date`, if it is `FALSE` then legend
#' type is set to `plate_name`. User can override this behavior by
#' setting explicitly `legend_type` to `date` or `plate_name`.
#' @param decreasing_dilution_order If `TRUE` the dilution values are
#' plotted in decreasing order, `TRUE` by default
#' @param log_scale Which elements on the plot should be displayed in log scale.
#' By default `"all"`. If `NULL` or `c()` no log scale is used,
#' if `"all"` or `c("dilutions", "MFI")` all elements are displayed in log scale.
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @return ggplot object with the plot
#'
#' @examples
#'
#' # creating temporary directory for the example
#' output_dir <- tempdir(check = TRUE)
#'
#' dir_with_luminex_files <- system.file("extdata", "multiplate_reallife_reduced",
#'   package = "PvSTATEM", mustWork = TRUE
#' )
#' list_of_plates <- process_dir(dir_with_luminex_files,
#'   return_plates = TRUE, format = "xPONENT", output_dir = output_dir
#' )
#' plot_standard_curve_stacked(list_of_plates, "ME", data_type = "Median", monochromatic = FALSE)
#'
#' @export
plot_standard_curve_stacked <- function(list_of_plates,
                                        analyte_name,
                                        data_type = "Median",
                                        decreasing_dilution_order = TRUE,
                                        monochromatic = TRUE,
                                        legend_type = NULL,
                                        log_scale = c("all"),
                                        verbose = TRUE) {
  AVAILABLE_LOG_SCALE_VALUES <- c("all", "dilutions", "MFI")

  if (!is.null(log_scale) && !all(log_scale %in% AVAILABLE_LOG_SCALE_VALUES)) {
    stop("log_scale should be a character vector containing elements from set: ", paste(AVAILABLE_LOG_SCALE_VALUES, collapse = ", ", "\nInstead passed: ", log_scale))
  }
  if (!is.list(list_of_plates)) {
    stop("list_of_plates should be a list of Plate objects, create it using `process_dir` function")
  }
  if (length(list_of_plates) == 0) {
    stop("list_of_plates should contain at least one Plate object")
  }
  for (plate in list_of_plates) {
    if (!inherits(plate, "Plate")) {
      stop("list_of_plates should contain only a Plate objects, create it using `process_dir` function")
    }
    if (!(analyte_name %in% plate$analyte_names)) {
      stop(analyte_name, " not found in one of the plate on list_of_plates")
    }
  }
  if (!is.null(legend_type) && !legend_type %in% c("date", "plate_name")) {
    stop("legend_type should be either 'date' or 'plate_name' or NULL")
  }

  plot_name <- paste0("Standard curves of: ", analyte_name)

  # Scale x and y if needed
  x_log_scale <- "dilutions" %in% log_scale || "all" %in% log_scale
  y_log_scale <- "MFI" %in% log_scale || "all" %in% log_scale
  x_trans <- ifelse(x_log_scale, "log10", "identity")
  x_cords_trans <- ifelse(decreasing_dilution_order, "reverse", "identity")
  y_trans <- ifelse(y_log_scale, "log10", "identity")

  xlab <- ifelse(x_log_scale, "Dilutions (log scale)", "Dilutions")
  x_ticks <- list_of_plates[[1]]$get_dilution_values("STANDARD CURVE")
  x_labels <- list_of_plates[[1]]$get_dilution("STANDARD CURVE")

  # Add the BLANK to the plot
  x_ticks <- c(x_ticks, min(x_ticks) / 2)
  x_labels <- c(x_labels, "B")
  ylab <- ifelse(y_log_scale, paste("MFI ", data_type, "(log scale)"), paste("MFI ", data_type))

  if (is.null(legend_type)) {
    legend_type <- ifelse(monochromatic, "date", "plate_name")
  }

  options(scipen = 30)
  p <- ggplot2::ggplot()
  p <- p + ggplot2::labs(title = plot_name, x = xlab, y = ylab) +
    ggplot2::scale_x_continuous(
      labels = x_labels,
      breaks = x_ticks,
      trans = x_trans
    ) +
    ggplot2::scale_y_continuous(trans = y_trans) +
    ggplot2::coord_trans(x = x_cords_trans) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 9),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_blank(),
      panel.grid.minor = element_line(color = scales::alpha("grey", .5), size = 0.1) # Make the minor grid lines less visible
    )

  number_of_colors <- length(list_of_plates)
  counter <- 1
  if (monochromatic) {
    # I don't want white and next one to be colors since on white background it's not visible
    number_of_colors <- number_of_colors + 2
    counter <- counter + 2 # skip white and closest one to white

    palette <- grDevices::colorRampPalette(c("white", "blue"))
    colors <- palette(number_of_colors)
  } else {
    colors <- scales::hue_pal()(number_of_colors)
  }
  custom_colors <- list()
  past_labels <- c()

  for  (plate in list_of_plates) {
    blank_mean <- mean(plate$get_data(analyte_name, "BLANK", data_type = data_type))

    legend_label <- ifelse(legend_type == "plate_name", plate$plate_name, format(plate$plate_datetime, format = "%Y-%m-%d %H:%M"))
    past_labels <- c(past_labels, legend_label)
    if (legend_label %in% names(custom_colors)) {
      repetitions <- sum(past_labels == legend_label)
      legend_label <- paste0(legend_label, " (", repetitions, ")")
    }

    plot_data <- data.frame(
      MFI = c(plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type), blank_mean),
      dilutions_value = c(plate$get_dilution_values("STANDARD CURVE"), min(plate$get_dilution_values("STANDARD CURVE")) / 2),
      label = legend_label
    )

    current_color <- colors[counter]
    custom_colors[[legend_label]] <- current_color

    # Add standard curve samples to the plot
    p <- p +
      ggplot2::geom_line(data = plot_data, aes(x = .data$dilutions_value, y = .data$MFI), color = "black", linewidth = 1.5) +
      ggplot2::geom_line(data = plot_data, aes(x = .data$dilutions_value, y = .data$MFI), color = current_color, linewidth = 1.1) +
      ggplot2::geom_point(data = plot_data, aes(x = .data$dilutions_value, y = .data$MFI, color = .data$label), size = 3)

    counter <- counter + 1
  }

  p <- p + ggplot2::scale_color_manual(
    values = custom_colors,
  )

  p
}

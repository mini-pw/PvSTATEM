#' @title Standard curves
#'

#' Plot standard curve samples of a plate
#'
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
                                        log_scale = c("all"), plot_line = TRUE, verbose = TRUE) {
  AVAILABLE_LOG_SCALE_VALUES <- c("all", "dilutions", "MFI")

  if (!inherits(plate, "Plate")) {
    stop("plate object should be a Plate")
  }
  if (!is.null(log_scale) && !all(log_scale %in% AVAILABLE_LOG_SCALE_VALUES)) {
    stop("log_scale should be a character vector containing elements from set: ", paste(AVAILABLE_LOG_SCALE_VALUES, collapse = ", "))
  }
  if (!(analyte_name %in% plate$analyte_names)) {
    stop(analyte_name, " not found in the plate object")
  }

  plot_name <- paste0("Sample values of standard curve for analyte: ", analyte_name)
  mfi <- plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type)
  plot_data <- data.frame(
    dilution = plate$get_dilution_values("STANDARD CURVE"),
    MFI = mfi,
    plate = plate$plate_name
  )

  # Scale x and y if needed
  x_log_scale <- "dilution" %in% log_scale || "all" %in% log_scale
  if (x_log_scale) {
    plot_data$dilution <- log(plot_data$dilution)
  }
  xlab <- ifelse(x_log_scale, "log(dilution)", "dilution")
  y_log_scale <- "MFI" %in% log_scale || "all" %in% log_scale
  if (y_log_scale) {
    plot_data$MFI <- log(plot_data$MFI)
  }
  ylab <- ifelse(y_log_scale, paste0("log(", data_type, ")"), data_type)

  x_ticks <- c(plot_data$dilution, max(plot_data$dilution) + 1)
  x_labels <- c(plate$get_dilution("STANDARD CURVE"), "")

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

  p <- ggplot2::ggplot(plot_data, aes(x = dilution, y = mfi, color = plate))

  if (plot_line) {
    p <- p + geom_line(linewidth = 1.2)
  }

  p <- p + geom_point(size = 3) +
    labs(title = plot_name, x = xlab, y = ylab) +
    scale_x_continuous(
      breaks = x_ticks, labels = x_labels,
      trans = ifelse(decreasing_dilution_order, "reverse", "identity")
    ) +
    scale_y_continuous() +
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

#' Create model using nplr logistic regresion model
#'
#' @param standard_curve_df Data frame with the standard curve data. Should contain columns: `dilution` and `MFI`
#' @param model_name Name of the model
#' @param npars Number of parameters to fit the model, by default 5 - the maximum value. `npars` also accepts a number of parameters to fit the model - an integer between 2 and 5, accepts also value `"all"`, which chooses best models from those with different number of parameters.
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @description
#' This function uses the `nplr` package to fit the model. The model is fitted using the formula:
#'
#' \deqn{y = B + \frac{T - B}{(1 + 10^{b \cdot (x_{mid} - x)})^s},}{y = B + (T - B) / (1 + 10^(b * (x_mid - x)))^s,}
#'
#' where:
#' - \eqn{y} is the predicted value, MFI in our case,
#' - \eqn{x} is the independent variable, dilution in our case,
#' - \eqn{B} is the bottom plateau - the right horizontal asymptote,
#' - \eqn{T} is the top plateau - the left horizontal asymptote,
#' - \eqn{b} is the slope of the curve at the inflection point,
#' - \eqn{x_{mid}}{x_mid} is the x-coordinate at the inflection point,
#' - \eqn{s} is the asymmetric coefficient.
#'
#' This equation is refereed as the Richards' equation. More information about the model can be found in the `nplr` package documentation.
#'
#' By default, `nplr` model transforms the x values using the `log10` function.
#'
#' @import nplr
#' @import dplyr
#'
logistic_regresion_model <- function(
    standard_curve_df,
    model_name = "MFI model",
    npars = 5, verbose = TRUE) {
  stopifnot(is.data.frame(standard_curve_df))
  stopifnot(all(!is.na(standard_curve_df)))
  stopifnot(is.character(model_name))

  data <- standard_curve_df %>%
    select(dilution, MFI)

  number_of_samples <- dim(data)[1]
  if (number_of_samples < 5) {
    verbose_cat(
      "(", color_codes$red_start, "WARNING", color_codes$red_end, ")\n",
      "Using less than 5 samples to fit logistic model. For now using the basic nplr method to fit the logistic model - should be modified in the future",
      verbose = verbose
    )
    npars <- min(npars, number_of_samples)
  }

  model <- nplr::nplr(
    x = data$dilution, y = data$MFI, npars = npars, silent = !verbose
  )
  model
}


#' Create standard curve model for a certain analyte
#'
#' @param plate Plate object
#' @param analyte_name Name of the analyte for which we want to create the model
#' @param data_type Data type of the value we want to use to fit the model - the same datatype as in the plate file. By default equals to `Median`
#' @param ... Additional arguments passed to the model
#'
#' @return Standard Curve model
#'
#' @export
create_standard_curve_model_analyte <- function(plate, analyte_name, data_type = "Median", ...) {
  # Create a dataframe with the data
  dilutions_numeric <- plate$get_dilution_values("STANDARD CURVE")
  curve_values <- plate$get_data(analyte_name, "STANDARD CURVE", data_type = data_type)
  fit_data <- data.frame("MFI" = curve_values, "dilution" = dilutions_numeric)

  # Create the model
  logistic_regresion_model(fit_data, analyte_name, ...)
}


#' Predict dilutions using fitted model
#'
#' @param plate Plate object
#' @param analyte_name Name of the analyte for which we want to predict the dilutions
#' @param model nplr object with the model
#' @param data_type Data type using which the model was fitted - the same datatype as in the plate file. By default equals to `Median`
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @return data frame with columns: `Location`, `Sample`, `MFI`, `dilution`
#'
#' @description
#' Function predicts the dilutions of the samples, based on the MFI values and the fitted model.
#' @export
predict_dilutions <- function(plate, analyte_name, model, data_type = "Median", verbose = TRUE) {
  concentrations_df <- data.frame(
    Location = plate$sample_locations,
    Sample = plate$sample_names,
    MFI = plate$get_data(analyte_name, "ALL", data_type = data_type),
    dilution = plate$get_dilution_values("ALL")
  )

  if (inherits(model, "nplr")) {
    estimates <- nplr::getEstimates(model, concentrations_df$MFI, B = max(concentrations_df$MFI), conf.level = .95)
    concentrations_df$dilution <- estimates$x
  } else {
    stop("For now model should be an instance of nplr, other options not implemented yet")
  }

  concentrations_df
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
  p <- plot_standard_curve_analyte(plate, analyte_name = analyte_name, data_type = data_type, decreasing_dilution_order = decreasing_dilution_order, log_scale = log_scale, verbose = verbose, plot_line = FALSE)

  plot_name <- paste0("Fitted standard curve for analyte: ", analyte_name)
  p$labels$title <- plot_name

  top_asymptote <- nplr::getPar(model)$params$top
  bottom_asymptote <- nplr::getPar(model)$params$bottom
  bottom_asymptote <- max(bottom_asymptote, 1)

  print(top_asymptote)
  print(bottom_asymptote)


  y <- seq(bottom_asymptote, top_asymptote, length.out = 1000)
  estimates <- nplr::getEstimates(model, y, B = 1e4, conf.level = .95)
  x <- estimates$x

  x_log_scale <- "dilution" %in% log_scale || "all" %in% log_scale
  if (x_log_scale) {
    x <- log(x)
  }
  y_log_scale <- "MFI" %in% log_scale || "all" %in% log_scale
  if (y_log_scale) {
    y <- log(y)
  }

  p <- p + geom_line(
    aes(x = x, y = y),
    color = "red", data = estimates, linewidth = 1
  )
  if (plot_asymptote) {
    p <- p + geom_hline(
      yintercept = top_asymptote, linetype = "dashed", color = "gray"
    ) +
      geom_hline(
        yintercept = bottom_asymptote, linetype = "dashed", color = "gray"
      )
  }

  print(p)
}

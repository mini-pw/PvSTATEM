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



#' Create model for standard curve of a certain antibody
#'
#' @param plate Plate object
#' @param antibody_name Name of the antibody for which we want to create the model
#' @param data_type Data type of the value we want to use to fit the model - the same datatype as in the plate file. By default equals to `Median`
#' @param npars Number of parameters to fit the model, by default 5 - the maximum value. `npars` also accepts a number of parameters to fit the model - an integer between 2 and 5, accepts also value `"all"`, which chooses best models from those with different number of parameters.
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#'
#' @return nplr object with the model
#'
#' @description
#' function for now uses the `nplr` package to fit the model. The model is fitted using the formula:
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
#' This equation is referred to as the Richards' equation. More information about the model can be found in the `nplr` package documentation.
#'
#' By default, `nplr` model transforms the x values using the `log10` function.
#'
#' @import nplr
#'
#' @export
create_standard_curve_model_antibody <- function(plate, antibody_name, data_type = "Median", npars = 5, verbose = TRUE) {
  # get standard curve values of certain antibody

  standard_curves <- plate$standard_curve
  dilutions <- sapply(standard_curves, function(sample) sample$sample_type$character_dilution_factor)
  dilutions_numeric <- sapply(standard_curves, function(sample) sample$sample_type$dilution_factor)
  curve_values <- sapply(standard_curves, function(sample) sample$data[data_type, antibody_name])

  # fit the model

  fit.data <- data.frame("MFI" = curve_values, "dilutions" = dilutions_numeric)

  # try catch this later
  if (length(standard_curves) >= 5) {
    model <- nplr::nplr(x = dilutions_numeric, y = curve_values, npars = npars, silent = !verbose)

    # sample_concentrations$dilution <- nplr::getEstimates(model, sample_concentrations$MFI, B = 1e4, conf.level = .95)$y
  } else {
    verbose_cat(
      "(",
      color_codes$red_start,
      "WARNING",
      color_codes$red_end,
      ")",
      "\n Using less than 5 samples to fit logistic model. For now using the basic nplr method to fit the logistic model - should be modified in the future",
      verbose = verbose
    )
    npars <- min(npars, length(standard_curves))
    model <- nplr::nplr(x = dilutions_numeric, y = curve_values, npars = npars, silent = !verbose)
  }
  return(model)
}

#' predict dilutions using fitted model
#'
#' @param plate Plate object
#' @param antibody_name Name of the antibody for which we want to predict the dilutions
#' @param model nplr object with the model
#' @param data_type Data type using which the model was fitted - the same datatype as in the plate file. By default equals to `Median`
#' @param verbose If `TRUE` prints messages, `TRUE` by default
#'
#' @return data frame with columns: `Location`, `Sample`, `MFI`, `dilution`
#'
#' @description
#' Function predicts the dilutions of the samples, based on the MFI values and the fitted model.
#' @export
predict_dilutions <- function(plate, antibody_name, model, data_type = "Median", verbose = TRUE) {
  sample_concentrations <- data.frame(matrix(nrow = plate$number_of_samples, ncol = 4))
  colnames(sample_concentrations) <- c("Location", "Sample", "MFI", "dilution")

  sample_concentrations$Sample <- plate$sample_names

  sample_concentrations$Location <- sapply(seq_len(plate$number_of_samples), function(i) plate$samples[[i]]$sample_location$location_name)

  sample_concentrations$MFI <- sapply(seq_len(plate$number_of_samples), function(i) plate$samples[[i]]$data[data_type, antibody_name])


  if (inherits(model, "nplr")) {
    sample_concentrations$dilution <- nplr::getEstimates(model, sample_concentrations$MFI, B = max(sample_concentrations$MFI), conf.level = .95)$x
  } else {
    stop("For now model should be an instance of nplr, other options not implemented yet")
  }

  sample_concentrations
}

#' Plot standard curve of a certain antibody with fitted model
#'
#' @param plate Plate object
#' @param antibody_name Name of the antibody for which we want to plot the standard curve - the same for which the model was fitted
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
plot_standard_curve_antibody_with_model <- function(plate, antibody_name, model, data_type = "Median", decreasing_dilution_order = TRUE, log_scale = c("all"), plot_asymptote = TRUE, verbose = TRUE) {
  p <- plot_standard_curve_analyte(plate, antibody_name = antibody_name, data_type = data_type, decreasing_dilution_order = decreasing_dilution_order, log_scale = log_scale, verbose = verbose, plot_line = FALSE)

  plot_name <- paste0("Fitted standard curve for analyte: ", antibody_name)
  p$labels$title <- plot_name

  curve_values <- sapply(plate$standard_curve, function(sample) sample$data[data_type, antibody_name])


  top_asymptote <- nplr::getPar(model)$params$top
  bottom_asymptote <- nplr::getPar(model)$params$bottom

  y <- seq(bottom_asymptote, top_asymptote, length.out = 1000)


  estimates <- nplr::getEstimates(model, y, B = 1e4, conf.level = .95)
  x <- estimates$x


  log_if_needed_mfi <- function(x) {
    if ("MFI" %in% log_scale || "all" %in% log_scale) {
      return(log(x))
    }
    return(x)
  }

  log_if_needed_dilutions <- function(x) {
    if ("dilutions" %in% log_scale || "all" %in% log_scale) {
      return(log(x))
    }
    return(x)
  }

  # add line to the plot
  p <- p + geom_line(aes(x = log_if_needed_dilutions(x), y = log_if_needed_mfi(y)), color = "red", data = estimates, linewidth = 1)


  if (plot_asymptote) {
    p <- p + geom_hline(yintercept = log_if_needed_mfi(top_asymptote), linetype = "dashed", color = "gray") +
      geom_hline(yintercept = log_if_needed_mfi(bottom_asymptote), linetype = "dashed", color = "gray")
  }
  p
}

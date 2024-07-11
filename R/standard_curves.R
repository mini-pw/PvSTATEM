library(ggplot2)

#' PLot standard curves of plate or list of plates
#'
#'
#' @param plates A single plate object or a list of plates. Plates should contain the same number of standard curve samples and same diltuions
#'
#' @param antibody_name Name of the antibody of which standard curve we want to plot.
#' @param data_type Data type of the value we want to plot - the same datatype as in the plate file. By default equals to `Net MFI`
#' @param file_path where to save the output plot. If `NULL` the plot is displayed, `NULL` by default
#' @param decreasing_dilution_order If `TRUE` the dilutions are plotted in decreasing order, `TRUE` by default
#' @param log_scale Which elements on the plot should be displayed in log scale. By default c("dilutions"). If `NULL` no log scale is used, if "all" or c("dilutions", "MFI") all elements are displayed in log scale.
#' @param verbose If `TRUE` print messages, `TRUE` by default
#'
#' @import ggplot2
#'
#' @export
plot_standard_curve_antibody <- function(plates, antibody_name, data_type = "Median", file_path = NULL, decreasing_dilution_order = TRUE, log_scale = c("all"), verbose = TRUE) {
  if (inherits(plates, "Plate")) { # an instance of Plate
    plates <- list(plates)
  }
  if (!inherits(plates, "list")) {
    stop("plates object should be a plate or a list of plates")
  }
  for (plate in plates) {
    if (!inherits(plate, "Plate")) {
      stop("plates object should be a plate or a list of plates")
    }
  }

  # check if log_scale is a character vector and contains element from set
  available_log_scale_values <- c("all", "dilutions", "MFI")
  if (!is.null(log_scale) && !all(log_scale %in% available_log_scale_values)) {
    stop("log_scale should be a character vector containing elements from set: ", paste(available_log_scale_values, collapse = ", "))
  }

  dilutions_numeric_base <- NULL
  standard_curve_num_samples <- NULL

  standard_curve_values_list <- list()

  for (plate in plates) {
    standard_curves <- plate$standard_curve

    if (is.null(standard_curve_num_samples)) {
      standard_curve_num_samples <- length(standard_curves)
    } else if (standard_curve_num_samples != length(standard_curves)) {
      stop("Inconsistent number of positive control or standard curve samples accross plates")
    }

    if (!antibody_name %in% plate$analyte_names) {
      #stop("Antibody ", antibody_name, " not present in the plate")
    }

    dilutions <- sapply(standard_curves, function(sample) sample$sample_type$character_dilution_factor)
    dilutions_numeric <- sapply(standard_curves, function(sample) sample$sample_type$dilution_factor)


    if (is.null(dilutions_numeric_base)) {
      dilutions_numeric_base <- dilutions_numeric
    } else if (!all.equal(dilutions_numeric_base, dilutions_numeric)) {
      stop("Inconsistent dilutions accross plates")
    }

    curve_values <- sapply(standard_curves, function(sample) sample$data[data_type, antibody_name])

    if (any(is.na(curve_values))) {
      stop(data_type, " not present in the dataframe")
    }

    standard_curve_values_list <- append(standard_curve_values_list, list(curve_values))
  }

  plot_name <- paste0("Standard curve for analyte: ", antibody_name)

  if (length(plates) >= 3) {
    colors <- RColorBrewer::brewer.pal(length(plates), "Set1")
  } else {
    colors <- c("red", "blue")
  }

  par(mfrow = c(1, 1))

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

  # Determine if x and y axes need to be log-scaled
  x_log_scale <- "dilutions" %in% log_scale || "all" %in% log_scale
  y_log_scale <- "MFI" %in% log_scale || "all" %in% log_scale

  plot_data <- data.frame()


  for (i in 1:length(plates)) {
    temp_data <- data.frame(
      dilutions = log_if_needed_dilutions(dilutions_numeric),
      mfi = log_if_needed_mfi(standard_curve_values_list[[i]]),
      plate = plates[[i]]$plate_name,
      colors = colors[[i]]
    )
    plot_data <- rbind(plot_data, temp_data)
  }

  # Generate x and y labels
  xlab <- ifelse(x_log_scale, "log(dilutions)", "dilutions")
  ylab <- ifelse(y_log_scale, paste0("log(", data_type, ")"), data_type)

  x_ticks <- c(log_if_needed_dilutions(dilutions_numeric), max(log_if_needed_dilutions(dilutions_numeric)) + 1)
  x_labels <- c(dilutions, "")

  legend_position <- c(0.8, 0.2) # Automatically position the legend
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

  p <- ggplot(plot_data, aes(x = dilutions, y = mfi, color = plate)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = colors) +
    labs(title = plot_name, x = xlab, y = ylab) +
    scale_x_continuous(breaks = x_ticks, labels = x_labels, trans = if (decreasing_dilution_order) "reverse" else "identity") +
    scale_y_continuous() +
    theme_minimal() +
    theme(
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      legend.position = legend_position, # Automatically position the legend
      legend.background = element_rect(fill = "white", color = "black")
    )



  if (!is.null(file_path)) {
    ggsave(file_path, plot = p, width = 10, height = 7, units = "in", dpi = 300)
  } else {
    print(p)
  }
  return(p)
}


#' create model for standard curve of a certain antibody
#' @export
create_standard_curve_model_antibody = function(plate, antibody_name, data_type = "Median", verbose = TRUE) {

  # get standard curve values of certain antibody

  standard_curves <- plate$standard_curve
  dilutions <- sapply(standard_curves, function(sample) sample$sample_type$character_dilution_factor)
  dilutions_numeric <- sapply(standard_curves, function(sample) sample$sample_type$dilution_factor)
  curve_values <- sapply(standard_curves, function(sample) sample$data[data_type, antibody_name])

  # fit the model

  fit.data <- data.frame("MFI"=curve_values,"dilutions"=dilutions_numeric)

  # try catch this later
  if (length(standard_curves) >= 5) {
    model <- nplr::nplr(x = dilutions_numeric, y = curve_values, npars = 5)

    #sample_concentrations$dilution <- nplr::getEstimates(model, sample_concentrations$MFI, B = 1e4, conf.level = .95)$y

  } else {
    verbose_cat(
      "(",
      color_codes$red_start,
      "WARNING",
      color_codes$red_end,
      ")",
      "\n Using less than 5 samples to fit logistic modelFor now using the basic nplr method to fit the logistic model - should be modified in the future",
      verbose = private$verbose
    )
    model <- nplr::nplr(x = dilutions_numeric, y = curve_values, npars = length(standard_curves))

    #sample_concentrations$dilution <- nplr::getEstimates(model, sample_concentrations$MFI, B = 1e4, conf.level = .95)$y
  }
  return(model)

}
#' predict dilutions using fitted model
#' @export
predict_dilutions = function(plate, model, antibody_name, data_type = "Median", verbose = TRUE) {
  sample_concentrations <- data.frame(matrix(nrow=nrow(data), ncol=4))
  colnames(sample_concentrations) <- c("Location",  "Sample", "MFI", "dilution")

  sample_concentrations$Sample <- plate$sample_names

  sample_concentrations$Location <- sapply(1:plate$number_of_samples, function(i) plate$samples[[i]]$sample_location$location_name)

  sample_concentrations$MFI <- sapply(1:plate$number_of_samples, function(i) plate$samples[[i]]$data[data_type, antibody_name])


  if (inherits(model, "nplr")) {
    sample_concentrations$dilution <- nplr::getEstimates(model, sample_concentrations$MFI, B = 1e4, conf.level = .95)$x
  } else {
    stop("For now model should be an instance of nplr, other options not implemented yet")
  }

}

#' plot standard curve of a certain antibody with fitted model
#' @export
plot_standard_curve_antibody_with_model = function(plate, antibody_name, model, data_type = "Median", decreasing_dilution_order = TRUE, log_scale = c("all"), verbose = TRUE) {
  p <- plot_standard_curve_antibody(plate, antibody_name = antibody_name, data_type = data_type, decreasing_dilution_order = decreasing_dilution_order, log_scale = log_scale, verbose = verbose)
  p$layers[[1]] <- NULL

  curve_values <- sapply(plate$standard_curve, function(sample) sample$data[data_type, antibody_name])

  y <- seq(0, max(curve_values), length.out = 100)

  estimates <- nplr::getEstimates(model, y, B = 1e4, conf.level = .95)


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
  p + geom_line(aes(x = log_if_needed_dilutions(x), y = log_if_needed_mfi(y)), color = "red", data = estimates, size = 0.8)

  }



verbose_cat <- function(..., verbose = TRUE) {
  if (verbose) {
    cat(..., sep = "")
  }
}

color_codes <-
  list(
    yellow_start = "\033[33m",
    yellow_end = "\033[39m",
    red_start = "\033[31m",
    red_end = "\033[39m",
    green_start = "\033[32m",
    green_end = "\033[39m"
  )

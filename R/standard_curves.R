#' PLot standard curves of plate or list of plates
#'
#'
#' @param plates A single plate object or a list of plates. Plates should contain the same number of standard curve samples and same diltuions
#'
#' @param antibody_name Name of the antibody of which standard curve we want to plot.
#' @param data_type Data type of the value we want to plot - the same datatype as in the plate file. By default equals to `Net MFI`
#' @param file_path where to save the output plot. If `NULL` the plot is displayed, `NULL` by default
#' @param decreasing_dilution_order If `TRUE` the dilutions are plotted in decreasing order, `TRUE` by default
#' @param verbose If `TRUE` print messages, `TRUE` by default
#'
#' @export
plot_standard_curve_antibody = function(plates, antibody_name, data_type = "Median", file_path = NULL, decreasing_dilution_order = TRUE, verbose = TRUE) {

  if (inherits(plates, "Plate")) { # an instance of Plate
    plates <- list(plates)
  }
  if (!inherits(plates, "list")){
    stop("plates object should be a plate or a list of plates")
  }
  for( plate in plates ){
    if (!inherits(plate, "Plate")){
      stop("plates object should be a plate or a list of plates")
    }
  }

  dilutions_numeric_base <- NULL
  standard_curve_num_samples <- NULL

  standard_curve_values_list <- list()

  for (plate in plates){
    if (!plate$check_if_blanks_already_adjusted) {
      verbose_cat(
        "(",
        color_codes$red_start,
        "WARNING",
        color_codes$red_end,
        ")",
        "\nBlank values not adjusted - Consider adjusting the blank values using function `plate$blank_adjustment`\n",
        verbose = verbose
      )
    }

    standard_curves <- plate$get_sample_by_type("POSITIVE CONTROL")
    if (is.null(standard_curve_num_samples)) {
      standard_curve_num_samples = length(standard_curves)
    } else if (standard_curve_num_samples != length(standard_curves)) {
      stop("Inconsistent number of positive control samples accross plates")
    }

    if (!antibody_name %in% plate$analyte_names){
      stop("Antibody ", antibody_name, " not present in the plate")
    }


    dilutions <- sapply(standard_curves, function(sample) sample$sample_type$character_dilution_factor)
    dilutions_numeric <- sapply(standard_curves, function(sample) sample$sample_type$dilution_factor)
    # sort values according to dilutions
    sorted_order <- order(dilutions_numeric, decreasing = decreasing_dilution_order)

    # Sort the vectors according to the sorted order of the reference vector
    dilutions_numeric <- dilutions_numeric[sorted_order]
    dilutions <- dilutions[sorted_order]
    standard_curves <- standard_curves[sorted_order]

    if (is.null(dilutions_numeric_base)) {
      dilutions_numeric_base <- dilutions_numeric
    }
    else if(!all.equal(dilutions_numeric_base, dilutions_numeric)) {
      stop("Inconsistent dilutions accross plates")
    }

    curve_values <- sapply(standard_curves, function(sample) sample$data[data_type, antibody_name])

    if (any(is.na(curve_values))){
      stop(data_type, " not present in the dataframe")
    }

    standard_curve_values_list = append(standard_curve_values_list, list(curve_values))

  }

  if (!is.null(file_path)){
    if (grepl("\\.pdf$", file_path, ignore.case = TRUE))
      pdf(file  = file_path)
    else
      png(filename = file_path)
  }

  plot_name <- paste0("Standard curve for analyte: ", antibody_name)

  if (length(plates) >= 3){
    colors <- RColorBrewer::brewer.pal(length(plates), "Set1")
  }else {
    colors <- c("red", "blue")
  }

  par(mfrow=c(1,1))

  plot(log(dilutions_numeric), log(standard_curve_values_list[[1]]), type = "o", lwd=2, main=plot_name, xlab="dilutions", ylab = paste0("log(", data_type, ")"), col=colors[[1]],axes=F,bty='L', pch=19,
       ylim = c(min(log(unlist(standard_curve_values_list))), max(log(unlist(standard_curve_values_list)))))
  if (length(plates) > 1) {
    for (i in 2:length(plates)) {
      lines(log(dilutions_numeric), log(standard_curve_values_list[[i]]), type = "o", lwd = 2, col = colors[[i]])
    }
  }
  axis(1,at=c(log(dilutions_numeric),max(log(dilutions_numeric))+1),labels=c(dilutions,""),cex.axis=0.9)
  axis(2,cex.axis=0.9)

  legend("topleft", legend = paste("Plate", 1:length(plates)), col = colors, lty = 1, lwd = 2)

  if (!is.null(file_path))
    dev.off()


}



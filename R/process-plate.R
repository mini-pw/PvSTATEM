#' Process a plate and save computed dilutions to a CSV
#'
#' @description
#' The behavior can be summarized as follows:
#' 1. Adjust blanks if not already done.
#' 2. Fit a model to each analyte using standard curve samples.
#' 3. Compute dilutions for each analyte using the corresponding model.
#' 4. Aggregate computed dilutions into a single data frame.
#' 5. Save the computed dilutions to a CSV file.
#'
#' @param plate (`Plate()`) a plate object
#' @param output_path (`character(1)`) path to save the computed dilutions.
#' If not provided the file will be saved in the working directory with the name `dilutions_\{plate_name\}.csv`.
#' Where the \{plate_name\} is the name of the plate.
#' @param data_type (`character(1)`) type of data to use for the computation. Median is the default
#' @param ... Additional arguments to be passed to the fit model function (`create_standard_curve_model_analyte`)
#'
process_plate <- function(plate, output_path = NULL, data_type = "Median", ...) {
  stopifnot(inherits(plate, "Plate"))
  if (is.null(output_path)) {
    output_path <- paste0("dilutions_", plate$plate_name, ".csv")
  }
  stopifnot(is.character(output_path))
  stopifnot(is.character(data_type))

  if (!plate$blank_adjusted) {
    plate <- plate$blank_adjustment(in_place = FALSE)
  }

  test_sample_names <- plate$sample_names[plate$sample_types == "TEST"]
  output_list <- list(
    "SampleName" = test_sample_names
  )
  for (analyte in plate$analyte_names) {
    model <- create_standard_curve_model_analyte(plate, analyte, data_type = data_type, ...)
    test_samples_mfi <- plate$get_data(analyte, "TEST", data_type = data_type)
    test_sample_estimates <- predict(model, test_samples_mfi)
    output_list[[analyte]] <- test_sample_estimates[, "dilution"]
  }

  output_df <- data.frame(output_list)
  write.csv(output_df, output_path, row.names = FALSE)
}

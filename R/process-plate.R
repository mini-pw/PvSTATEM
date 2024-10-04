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
#' If not provided the file will be saved in the working directory with the name `dilutions_{plate_name}.csv`.
#' Where the `{plate_name}` is the name of the plate.
#' @param data_type (`character(1)`) type of data to use for the computation. Median is the default
#' @param adjust_blanks (`logical(1)`) adjust blanks before computing dilutions. Default is `FALSE`
#' @param verbose (`logical(1)`) print additional information. Default is `TRUE`
#' @param ... Additional arguments to be passed to the fit model function (`create_standard_curve_model_analyte`)
#'
#' @examples
#'
#' plate_file <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_layout.csv", package = "PvSTATEM")
#'
#' plate <- read_luminex_data(plate_file, layout_file)
#' 
#' tmp_dir <- tempdir(check = TRUE)
#' temporary_filepath <- file.path(tmp_dir, "output.csv")
#' process_plate(plate, output_path = temporary_filepath) 
#' # create and save dataframe with computed dilutions
#' 
#'
#' @export
process_plate <- function(plate, output_path = NULL, data_type = "Median", adjust_blanks = FALSE, verbose = TRUE, ...) {
  stopifnot(inherits(plate, "Plate"))
  if (is.null(output_path)) {
    output_path <- paste0("dilutions_", plate$plate_name, ".csv")
  }
  stopifnot(is.character(output_path))
  stopifnot(is.character(data_type))

  if (!plate$blank_adjusted && adjust_blanks) {
    plate <- plate$blank_adjustment(in_place = FALSE)
  }

  test_sample_names <- plate$sample_names[plate$sample_types == "TEST"]
  output_list <- list(
    "SampleName" = test_sample_names
  )
  verbose_cat("Fitting the models and computing the dilutions for each analyte\n", verbose = verbose)

  for (analyte in plate$analyte_names) {
    model <- create_standard_curve_model_analyte(plate, analyte, data_type = data_type, ...)
    test_samples_mfi <- plate$get_data(analyte, "TEST", data_type = data_type)
    test_sample_estimates <- predict(model, test_samples_mfi)
    output_list[[analyte]] <- test_sample_estimates[, "dilution"]
  }

  output_df <- data.frame(output_list)

  verbose_cat("Saving the computed dilutions to a CSV file located in: '", output_path, "'\n", verbose = verbose)
  write.csv(output_df, output_path, row.names = FALSE)
}

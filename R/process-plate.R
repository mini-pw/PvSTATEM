is_valid_normalisation_type <- function(normalisation_type) {
  normalisation_type %in% SerolyzeR.env$normalisation_types
}
#' @title
#' Process a Plate and Save Normalised Output to CSV
#'
#' @description
#' This function processes a plate and computes normalised values based on the specified `normalisation_type`.
#' The function supports three types of normalisation:
#' - **RAU** (Relative Antibody Units) - Default normalisation type.
#' - **nMFI** (Normalised Median Fluorescence Intensity).
#' - **MFI** (Median Fluorescence Intensity, blank-adjusted raw MFI values).
#'
#' Depending on the chosen normalisation type, the function:
#' - Adjusts for blanks (if `blank_adjustment = TRUE`).
#' - Computes the relevant normalised values for each analyte in the plate.
#' - Aggregates computed values into a single data frame.
#' - Optionally writes the results to a CSV file.
#'
#' **RAU Normalisation Workflow:**
#' 1. Blank adjustment (if enabled).
#' 2. Fit standard curve models for each analyte.
#' 3. Compute RAU values based on the fitted models.
#' 4. Aggregate RAU values into a data frame.
#' 5. Save the results to a CSV file.
#'
#' **nMFI Normalisation Workflow:**
#' 1. Blank adjustment (if enabled).
#' 2. Compute nMFI values using the target dilution.
#' 3. Aggregate nMFI values into a data frame.
#' 4. Save the results to a CSV file.
#'
#' **MFI Normalisation Workflow:**
#' 1. Blank adjustment (if enabled).
#' 2. Save the adjusted MFI values to a CSV file.
#'
#' If the plate is already blank-adjusted, setting `blank_adjustment = TRUE` has no effect.
#'
#' More details on the normalisation methods can be found in:
#' - RAU: \link[SerolyzeR]{create_standard_curve_model_analyte}.
#' - nMFI: \link[SerolyzeR]{get_nmfi}.
#'
#'
#' @param plate (`Plate`) A plate object containing fluorescence intensity data.
#' @param filename (`character(1)`, optional) Output CSV filename. Defaults to `{plate_name}_{normalisation_type}.csv`.
#'   - If omitted (`NULL`), the filename is auto-generated.
#'   - If the filename lacks `.csv`, the extension is automatically added.
#'   - If `output_dir` is specified, it is combined with `filename` unless `filename` is an absolute path.
#'   - If a file already exists at the path, it will be overwritten.
#' @param write_output (`logical(1)`, default = `TRUE`) Whether to save the output to a CSV file.
#' @param output_dir (`character(1)`, default = `'normalised_data'`) Directory for saving the output file.
#'   - If the directory does not exist, it will be created.
#'   - If `NULL`, the current working directory is used.
#' @param normalisation_type (`character(1)`, default = `'RAU'`) The normalisation method to apply.
#'   - Allowed values: \code{c(`r toString(SerolyzeR.env$normalisation_types)`)}.
#' @param data_type (`character(1)`, default = `'Median'`) The data type to use for calculations.
#' @param blank_adjustment (`logical(1)`, default = `FALSE`) Whether to perform blank adjustment before computing values.
#' @param verbose (`logical(1)`, default = `TRUE`) Whether to print additional information during execution.
#' @param reference_dilution (`numeric(1)` or `character(1)`, default = `1/400`)
#'   - The target dilution used as a reference for nMFI normalisation.
#'   - Ignored for RAU normalisation.
#'   - Can be numeric (e.g., `0.0025`) or a string (`'1/400'`).
#' @param ... Additional arguments passed to the model fitting function (`create_standard_curve_model_analyte`).
#'
#' @return A data frame containing the computed normalised values.
#'
#' @examples
#' plate_file <- system.file("extdata", "CovidOISExPONTENT_CO_reduced.csv", package = "SerolyzeR")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "SerolyzeR")
#' plate <- read_luminex_data(plate_file, layout_file, verbose = FALSE)
#' example_dir <- tempdir(check = TRUE)
#'
#' # Process plate with default settings (RAU normalisation)
#' process_plate(plate, output_dir = example_dir)
#'
#' # Process plate without blank adjustment, custom filename
#' process_plate(plate, filename = "plate_no_blank_adjustment.csv", output_dir = example_dir, blank_adjustment = FALSE)
#'
#' # Process plate with nMFI normalisation
#' process_plate(plate, output_dir = example_dir, normalisation_type = "nMFI", reference_dilution = 1 / 400)
#'
#' @export
process_plate <-
  function(plate,
           filename = NULL,
           output_dir = "normalised_data",
           write_output = TRUE,
           normalisation_type = "RAU",
           data_type = "Median",
           blank_adjustment = FALSE,
           verbose = TRUE,
           reference_dilution = 1 / 400,
           ...) {
    stopifnot(inherits(plate, "Plate"))

    stopifnot(is_valid_normalisation_type(normalisation_type))
    stopifnot(is.character(data_type))

    if (write_output) {
      output_path <- validate_filepath_and_output_dir(filename, output_dir,
        plate$plate_name, normalisation_type,
        "csv",
        verbose = verbose
      )
    } else {
      output_path <- NULL
    }


    if ((!plate$blank_adjusted) && blank_adjustment) {
      plate <- plate$blank_adjustment(in_place = FALSE)
    }

    test_sample_names <- plate$sample_names[plate$sample_types == "TEST"]
    if (normalisation_type == "MFI") {
      verbose_cat("Extracting the raw MFI to the output dataframe\n")
      output_df <- plate$get_data(
        "ALL", "TEST",
        data_type = data_type
      )
    } else if (normalisation_type == "nMFI") {
      verbose_cat("Computing nMFI values for each analyte\n", verbose = verbose)
      output_df <- get_nmfi(
        plate,
        reference_dilution = reference_dilution, data_type = data_type
      )
    } else if (normalisation_type == "RAU") {
      # RAU normalisation
      verbose_cat("Fitting the models and predicting RAU for each analyte\n", verbose = verbose)
      output_list <- list()
      for (analyte in plate$analyte_names) {
        model <- create_standard_curve_model_analyte(
          plate, analyte,
          data_type = data_type, ...
        )
        test_samples_mfi <- plate$get_data(
          analyte, "TEST",
          data_type = data_type
        )
        test_sample_estimates <- predict(model, test_samples_mfi)
        output_list[[analyte]] <- test_sample_estimates[, "RAU"]
      }
      output_df <- data.frame(output_list)
    }
    rownames(output_df) <- test_sample_names

    if (write_output) {
      verbose_cat("Saving the computed ", normalisation_type, " values to a CSV file located in: '",
        output_path,
        "'\n",
        verbose = verbose
      )
      write.csv(output_df, output_path)
    }

    return(output_df)
  }

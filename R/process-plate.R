VALID_NORMALISATION_TYPES <- c("RAU", "nMFI")

is_valid_normalisation_type <- function(normalisation_type) {
  normalisation_type %in% VALID_NORMALISATION_TYPES
}

#' @title
#' Process a plate and save output values to a CSV
#'
#' @description
#' Depending on the `normalisation_type` argument, the function will compute the RAU or nMFI values for each analyte in the plate.
#' **RAU** is the default normalisation type.
#'
#'
#' The behaviour of the function, in the case of RAU normalisation type, can be summarised as follows:
#' 1. Adjust blanks if not already done.
#' 2. Fit a model to each analyte using standard curve samples.
#' 3. Compute RAU values for each analyte using the corresponding model.
#' 4. Aggregate computed RAU values into a single data frame.
#' 5. Save the computed RAU values to a CSV file.
#'
#' More info about the RAU normalisation can be found in
#' `create_standard_curve_model_analyte` function documentation \link[PvSTATEM]{create_standard_curve_model_analyte} or in the Model reference \link[PvSTATEM]{Model}.
#'
#'
#'
#'
#' In case the normalisation type is **nMFI**, the function will:
#' 1. Adjust blanks if not already done.
#' 2. Compute nMFI values for each analyte using the target dilution.
#' 3. Aggregate computed nMFI values into a single data frame.
#' 4. Save the computed nMFI values to a CSV file.
#'
#' More info about the nMFI normalisation can be found in `get_nmfi` function documentation \link[PvSTATEM]{get_nmfi}.
#'
#' @param plate (`Plate()`) a plate object
#' @param output_path (`character(1)`) path to save the computed RAU values.
#' If not provided, the file will be saved in the working directory with the name `{normalisation_type}_{plate_name}.csv`.
#' Where the `{plate_name}` is the name of the plate.
#' @param normalisation_type (`character(1)`) type of normalisation to use. Available options are:
#' \cr \code{c(`r toString(VALID_NORMALISATION_TYPES)`)}.
#' @param data_type (`character(1)`) type of data to use for the computation. Median is the default
#' @param include_raw_mfi (`logical(1)`) include raw MFI values in the output. The default is `TRUE`.
#' In case this option is `TRUE`, the output dataframe contains two columns for each analyte: one for the normalised values and one for the raw MFI values.
#' The normalised columns are named as `AnalyteName` and `AnalyteName_raw`, respectively.
#' @param adjust_blanks (`logical(1)`) adjust blanks before computing RAU values. The default is `FALSE`
#' @param verbose (`logical(1)`) print additional information. The default is `TRUE`
#' @param reference_dilution (`numeric(1)`) target dilution to use as reference for the nMFI normalisation. Ignored in case of RAU normalisation.
#' Default is `1/400`.
#' It should refer to a dilution of a standard curve sample in the given plate object.
#' This parameter could be either a numeric value or a string.
#' In case it is a character string, it should have the format `1/d+`, where `d+` is any positive integer.
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
#' # nMFI normalisation
#' process_plate(plate,
#'   output_path = temporary_filepath,
#'   normalisation_type = "nMFI", reference_dilution = 1 / 400
#' )
#'
#' @return a data frame with normalised values
#' @export
process_plate <-
  function(plate,
           output_path = NULL,
           normalisation_type = "RAU",
           data_type = "Median",
           include_raw_mfi = TRUE,
           adjust_blanks = FALSE,
           verbose = TRUE,
           reference_dilution = 1 / 400,
           ...) {
    stopifnot(inherits(plate, "Plate"))

    stopifnot(is_valid_normalisation_type(normalisation_type))


    if (is.null(output_path)) {
      output_path <-
        paste0(normalisation_type, "_", plate$plate_name, ".csv")
    }
    stopifnot(is.character(output_path))
    stopifnot(is.character(data_type))

    if (!plate$blank_adjusted && adjust_blanks) {
      plate <- plate$blank_adjustment(in_place = FALSE)
    }
    if (normalisation_type == "nMFI") {
      verbose_cat("Computing nMFI values for each analyte\n", verbose = verbose)
      output_df <-
        get_nmfi(plate, reference_dilution = reference_dilution, data_type = data_type)
      verbose_cat(
        "Saving the computed nMFI values to a CSV file located in: '",
        output_path,
        "'\n",
        verbose = verbose
      )
    } else if (normalisation_type == "RAU") {
      # RAU normalisation

      test_sample_names <-
        plate$sample_names[plate$sample_types == "TEST"]
      output_list <- list("SampleName" = test_sample_names)
      verbose_cat("Fitting the models and predicting RAU for each analyte\n",
        verbose = verbose
      )

      for (analyte in plate$analyte_names) {
        model <-
          create_standard_curve_model_analyte(plate, analyte, data_type = data_type, ...)
        test_samples_mfi <-
          plate$get_data(analyte, "TEST", data_type = data_type)
        test_sample_estimates <- predict(model, test_samples_mfi)
        output_list[[analyte]] <- test_sample_estimates[, "RAU"]
      }

      output_df <- data.frame(output_list)

      rownames(output_df) <- test_sample_names
    }

    if (include_raw_mfi) {
      verbose_cat("Adding the raw MFI values to the output dataframe\n")
      raw_mfi <- plate$data[[data_type]][plate$sample_types == "TEST", ]
      colnames(raw_mfi) <- paste0(colnames(raw_mfi), "_raw")

      output_df <- cbind(output_df, raw_mfi)
    }

    verbose_cat("Saving the computed", normalisation_type, "values to a CSV file located in: '",
      output_path,
      "'\n",
      verbose = verbose
    )

    write.csv(output_df, output_path)

    return(output_df)
  }

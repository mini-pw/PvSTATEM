is_valid_normalisation_type <- function(normalisation_type) {
  normalisation_type %in% PvSTATEM.env$normalisation_types
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
#' In case the normalisation type is **nMFI**, the function will:
#' 1. Adjust blanks if not already done.
#' 2. Compute nMFI values for each analyte using the target dilution.
#' 3. Aggregate computed nMFI values into a single data frame.
#' 4. Save the computed nMFI values to a CSV file.
#'
#' More info about the nMFI normalisation can be found in `get_nmfi` function documentation \link[PvSTATEM]{get_nmfi}.
#'
#' @param plate (`Plate()`) a plate object
#' @param filename (`character(1)`) The name of the output CSV file with normalised MFI values.
#' If not provided or equals to `NULL`, the output filename will be based on the normalisation type
#' and the plate name, precisely: `{plate_name}_{normalisation_type}.csv`.
#' By default the `plate_name` is the filename of the input file that contains the plate data.
#' For more details please refer to \link[PvSTATEM]{Plate}.
#'
#' If the passed filename does not contain `.csv` extension, the default extension `.csv` will be added.
#' Filename can also be a path to a file, e.g. `path/to/file.csv`. In this case, the `output_dir` and `filename` will be joined together.
#' However, if the passed filepath is an absolute path and the `output_dir` parameter is also provided, the `output_dir` parameter will be ignored.
#' If a file already exists under a specified filepath, the function will overwrite it.
#'
#' @param write_output (`logical(1)`) whether or not to write the output to a file
#' specified by `filename` parameter. The default is `TRUE`.
#'
#' @param output_dir (`character(1)`) The directory where the output CSV file should be saved.
#' Please note that any directory path provided will create all necessary directories (including parent directories) if they do not exist.
#' If it equals to `NULL` the current working directory will be used. Default is 'normalised_data'.
#'
#' @param normalisation_type (`character(1)`) type of normalisation to use. Available options are:
#' \cr \code{c(`r toString(PvSTATEM.env$normalisation_types)`)}.
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
#' plate_file <- system.file("extdata", "CovidOISExPONTENT_CO_reduced.csv", package = "PvSTATEM")
#' # a plate file with reduced number of analytes to speed up the computation
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM")
#'
#' plate <- read_luminex_data(plate_file, layout_file, verbose = FALSE)
#'
#' example_dir <- tempdir(check = TRUE) # a temporary directory
#' # create and save dataframe with computed dilutions
#' process_plate(plate, output_dir = example_dir)
#'
#' # process plate without adjusting blanks and save the output to a file with a custom name
#' process_plate(plate,
#'   filename = "plate_without_blanks_adjusted.csv",
#'   output_dir = example_dir, adjust_blanks = FALSE
#' )
#'
#'
#' # nMFI normalisation
#' process_plate(plate,
#'   output_dir = example_dir,
#'   normalisation_type = "nMFI", reference_dilution = 1 / 400
#' )
#'
#' @return a data frame with normalised values
#' @export
process_plate <-
  function(plate,
           filename = NULL,
           output_dir = "normalised_data",
           write_output = TRUE,
           normalisation_type = "RAU",
           data_type = "Median",
           include_raw_mfi = TRUE,
           adjust_blanks = FALSE,
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


    if (!plate$blank_adjusted && adjust_blanks) {
      plate <- plate$blank_adjustment(in_place = FALSE)
    }
    if (normalisation_type == "nMFI") {
      verbose_cat("Computing nMFI values for each analyte\n", verbose = verbose)
      output_df <-
        get_nmfi(plate, reference_dilution = reference_dilution, data_type = data_type)
    } else if (normalisation_type == "RAU") {
      # RAU normalisation

      test_sample_names <-
        plate$sample_names[plate$sample_types == "TEST"]
      output_list <- list()
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

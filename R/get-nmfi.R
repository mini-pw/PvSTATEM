#' @title Calculate normalised MFI values for a plate
#'
#' @description
#' The function calculates the normalised MFI (nMFI) values for each of the analytes in the plate.
#'
#' The nMFI values are calculated as the ratio of the test samples' MFI values to the standard curve samples with the target dilution.
#'
#'
#'
#' **When nMFI could be used?**
#' In general, it is preferred to use Relative Antibody Unit (RAU) values for any analysis.
#' However, it is sometimes impossible to fit a model to the standard curve samples.
#' This may happen if the MFI values of test samples are much higher than the MFI of standard curve samples.
#' Then, the prediction would require significant data extrapolation, which could lead to unreliable results.
#'
#' In such cases, the nMFI values could be used as a proxy for RAU values if we want, for instance, to account for plate-to-plate variation.
#'
#' @param plate (`Plate()`) a plate object for which to calculate the nMFI values
#' @param reference_dilution (`numeric(1) or character(1)`) the dilution value of the standard curve sample
#' to use as a reference for normalisation. The default is `1/400`.
#' It should refer to a dilution of a standard curve sample in the given plate object.
#' This parameter could be either a numeric value or a string.
#' In case it is a character string, it should have format `1/d+`, where `d+` is any positive integer.
#' @param data_type (`character(1)`) type of data for the computation. Median is the default
#' @param verbose (`logical(1)`) print additional information. The default is `TRUE`
#'
#' @return nmfi (`data.frame`) a data frame with normalised MFI values for each analyte in the plate and all test samples.
#'
#' @examples
#'
#' # read the plate
#' plate_file <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_layout.csv", package = "PvSTATEM")
#'
#' plate <- read_luminex_data(plate_file, layout_file)
#'
#' # artificially bump up the MFI values of the test samples (the Median data type is default one)
#' plate$data[["Median"]][plate$sample_types == "TEST", ] <-
#'   plate$data[["Median"]][plate$sample_types == "TEST", ] * 10
#'
#' # calculate the nMFI values
#' nmfi <- get_nmfi(plate, reference_dilution = 1 / 400)
#'
#' # we don't do any extrapolation and the values should be comparable across plates
#' head(nmfi)
#' # different params
#' nmfi <- get_nmfi(plate, reference_dilution = "1/50")
#'
#' @export
get_nmfi <-
  function(plate,
           reference_dilution = 1 / 400,
           data_type = "Median",
           verbose = TRUE) {
    stopifnot(inherits(plate, "Plate"))

    stopifnot(length(reference_dilution) == 1)

    # check if data_type is valid
    stopifnot(is_valid_data_type(data_type))

    # check if reference_dilution is numeric or string
    if (is.character(reference_dilution)) {
      reference_dilution <-
        convert_dilutions_to_numeric(reference_dilution)
    }

    stopifnot(is.numeric(reference_dilution))
    stopifnot(reference_dilution > 0)

    if (!reference_dilution %in% plate$get_dilution_values("STANDARD CURVE")) {
      stop(
        "The target ",
        reference_dilution,
        " dilution is not present in the plate."
      )
    }


    # get index of standard curve sample with the target dilution
    reference_standard_curve_id <-
      which(
        plate$dilution_values == reference_dilution &
          plate$sample_types == "STANDARD CURVE"
      )
    stopifnot(length(reference_standard_curve_id) == 1)

    plate_data <-
      plate$get_data(
        analyte = "ALL",
        sample_type = "ALL",
        data_type = data_type
      )

    reference_mfi <- plate_data[reference_standard_curve_id, ]

    test_mfi <-
      plate$get_data(
        analyte = "ALL",
        sample_type = "TEST",
        data_type = data_type
      )
    reference_mfi <- reference_mfi[rep(1, nrow(test_mfi)), ]

    nmfi <- test_mfi / reference_mfi

    rownames(nmfi) <-
      plate$sample_names[plate$sample_types == "TEST"]


    return(nmfi)
  }

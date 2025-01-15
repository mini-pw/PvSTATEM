VALID_SAMPLE_TYPES <- c(
  "ALL",
  "BLANK",
  "TEST",
  "NEGATIVE CONTROL",
  "STANDARD CURVE",
  "POSITIVE CONTROL"
)

VALID_DATA_TYPES <- c(
  "Median",
  "Net MFI",
  "Count",
  "Avg Net MFI",
  "Mean",
  "Peak"
)

globalVariables(c("VALID_SAMPLE_TYPES", "VALID_DATA_TYPES"))

#' Check validity of given sample type
#' @description
#' Check if the sample type is valid. The sample type is valid if it is one of the
#' elements of the `VALID_SAMPLE_TYPES` vector. The valid sample types are:
#'
#' \code{c(`r toString(VALID_SAMPLE_TYPES)`)}.
#'
#' @param sample_type A string representing the sample type.
#' @return `TRUE` if the sample type is valid, `FALSE` otherwise.
#'
#' @export
is_valid_sample_type <- function(sample_type) {
  sample_type %in% VALID_SAMPLE_TYPES
}

#' Check validity of given data type
#' @description
#' Check if the data type is valid. The data type is valid if it is one of the
#' elements of the `VALID_DATA_TYPES` vector. The valid data types are:
#' \cr \code{c(`r toString(VALID_DATA_TYPES)`)}.
#'
#' @param data_type A string representing the data type.
#' @return `TRUE` if the data type is valid, `FALSE` otherwise.
#'
#' @export
is_valid_data_type <- function(data_type) {
  data_type %in% VALID_DATA_TYPES
}


#' @title Plate object
#'
#' @description
#' A class to represent the luminex plate. It contains information about
#' the samples and analytes that were examined on the plate as well as
#' some additional metadata and batch info
#'
#' @importFrom R6 R6Class
#'
Plate <- R6::R6Class(
  "Plate",
  public = list(

    ## Fields ----------------------------------------------------------------
    ## Must be set ---

    #'
    #' @field plate_name  (`character(1)`)\cr
    #'  Name of the plate. Set to the name of the file from which the plate was read.
    plate_name = "",
    #'
    #' @field analyte_names (`character()`)\cr
    #' Names of the analytes that were examined on the plate.
    analyte_names = character(),
    #'
    #' @field sample_names (`character()`)\cr
    #' Names of the samples that were examined on the plate. The order of the
    #' samples in this vector is identical with order in the CSV source file.
    sample_names = character(),
    ## Must be set if validated ---
    #'
    #' @field batch_name (`character(1)`)\cr
    #' Name of the batch to which the plate belongs.
    batch_name = "",
    #'
    #' @field plate_datetime (`POSIXct()`)\cr
    #' A date and time when the plate was created by the machine
    plate_datetime = NULL,
    #'
    #' @field sample_locations (`character()`)\cr
    #' Locations of the samples on the plate. This vector is in the same order
    #' as the `sample_names` vector.
    sample_locations = NULL,
    #'
    #' @field sample_types (`character()`)\cr
    #' Types of the samples that were examined on the plate.
    #' The possible values are \cr \code{c(`r toString(VALID_SAMPLE_TYPES)`)}.
    #' This vector is in the same order as the `sample_names` vector.
    sample_types = NULL,
    #'
    #' @field dilutions (`character()`)\cr
    #' A list containing names of the samples as keys and string representing dilutions as values.
    #' The dilutions are represented as strings. This vector is in the same order as the `sample_names` vector.
    dilutions = NULL,
    #'
    #' @field dilution_values (`numeric()`)\cr
    #' A list containing names of the samples as keys and numeric values representing dilutions as values.
    #' It is in the same order as the `sample_names` vector.
    dilution_values = NULL,
    #'
    #' @field default_data_type (`character(1)`)\cr
    #' The default data type that will be returned by the `get_data` method.
    #' By default is set to `Median`.
    default_data_type = NULL,
    #'
    #' @field data (`list()`)\cr
    #' A list containing dataframes with the data for each sample and analyte.
    #' The possible data types - the keys of the list are:
    #' \cr \code{c(`r toString(VALID_DATA_TYPES)`)}.
    #'
    #' In each dataframe, the rows represent samples and the columns represent analytes.
    data = NULL,
    #'
    #' @field batch_info (`list()`)\cr
    #' A list containing additional, technical information about the batch.
    batch_info = NULL,
    #'
    #' @field layout (`character()`)\cr
    #' A list containing information about the layout of the plate.
    #' The layout is read from the separate file and usually provides additional
    #' information about the dilutions, sample names, and the sample layout
    #' on the actual plate.
    layout = NULL,
    #'
    ## Fields that will be set by the methods ---
    #'
    #' @field blank_adjusted (`logical`)\cr
    #' A flag indicating whether the blank values have been adjusted.
    blank_adjusted = FALSE,


    #' @description
    #' Method to initialize the Plate object
    #'
    #' @param plate_name  (`character(1)`)\cr
    #'  Name of the plate.
    #'  By default is set to an empty string,
    #'  during the reading process it is set to the name
    #'  of the file from which the plate was read.
    #'
    #' @param sample_names (`character()`)\cr
    #'  Names of the samples that were examined on the plate.
    #'
    #' @param analyte_names (`character()`)\cr
    #'  Names of the analytes that were examined on the plate.
    #'
    #' @param batch_name (`character(1)`)\cr
    #'  Name of the batch to which the plate belongs.
    #'  By default is set to an empty string, during the reading process it is set to
    #'  the `batch` field of the plate
    #'
    #' @param plate_datetime (`POSIXct()`)\cr
    #' Datetime object representing the date and time when the plate was created by the machine.
    #'
    #' @param sample_locations (`character()`)\cr
    #'  Locations of the samples on the plate.
    #'
    #' @param sample_types (`character()`)\cr
    #'  Types of the samples that were examined on the plate.
    #'  The possible values are \cr \code{c(`r toString(VALID_SAMPLE_TYPES)`)}.
    #'
    #' @param dilutions (`character()`)\cr
    #'  A list containing names of the samples as keys and string representing dilutions as values.
    #'  The dilutions are represented as strings.
    #'
    #' @param dilution_values (`numeric()`)\cr
    #'  A list containing names of the samples as keys and numeric values representing dilutions as values.
    #'
    #' @param default_data_type (`character(1)`)\cr
    #'  The default data type that will be returned by the `get_data` method.
    #'  By default is set to `Median`.
    #'
    #' @param data (`list()`)\cr
    #'  A list containing dataframes with the data for each sample and analyte.
    #'  The possible data types - the keys of the list are \cr \code{c(`r toString(VALID_DATA_TYPES)`)}.
    #'  In each dataframe, the rows represent samples and the columns represent analytes.
    #'
    #' @param batch_info (`list()`)\cr
    #'  A list containing additional, technical information about the batch.
    #'
    #' @param layout (`character()`)\cr
    #'  A list containing information about the layout of the plate.
    #'  The layout is read from the separate file and usually provides additional
    #'  information about the dilutions, sample names, and the sample layout
    #'  on the actual plate.
    #'
    initialize = function(plate_name, sample_names, analyte_names,
                          batch_name = "", plate_datetime = NULL,
                          sample_locations = NULL, sample_types = NULL,
                          dilutions = NULL, dilution_values = NULL,
                          default_data_type = NULL, data = NULL,
                          batch_info = NULL, layout = NULL) {
      self$plate_name <- plate_name
      self$analyte_names <- analyte_names
      self$sample_names <- sample_names
      if (!is.null(batch_name) && length(batch_name) != 0) self$batch_name <- batch_name
      if (!is.null(sample_locations)) self$sample_locations <- sample_locations
      if (!is.null(sample_types)) self$sample_types <- sample_types
      if (!is.null(plate_datetime)) self$plate_datetime <- plate_datetime
      if (!is.null(dilutions)) self$dilutions <- dilutions
      if (!is.null(dilution_values)) self$dilution_values <- dilution_values
      if (!is.null(default_data_type)) self$default_data_type <- default_data_type
      if (!is.null(data)) self$data <- data
      if (!is.null(batch_info)) self$batch_info <- batch_info
      if (!is.null(layout)) self$layout <- layout
    },

    #' @description
    #' Function prints the basic information about the plate
    #' such as the number of samples and analytes
    #' @param ... Additional parameters to be passed to the print function
    print = function(...) {
      cat(
        "Plate with",
        length(self$sample_names),
        "samples and",
        length(self$analyte_names),
        "analytes\n",
        ... = ...
      )
    },

    #' Print the summary of the plate
    #' @description
    #' Function outputs basic information about the plate, such as
    #' examination date, batch name, and sample types.
    #'
    #' @param include_names If `include_names` parameter is `TRUE`, a
    #' part from count of control samples, provides also their names.
    #' By default `FALSE`
    #' @param ... Additional parameters to be passed to the print function
    summary = function(..., include_names = FALSE) {
      positive_control_num <- sum(self$sample_types == "POSITIVE CONTROL")
      negative_control_num <- sum(self$sample_types == "NEGATIVE CONTROL")
      standard_curve_num <- sum(self$sample_types == "STANDARD CURVE")
      test_samples_num <- sum(self$sample_types == "TEST")
      blank_samples_num <- sum(self$sample_types == "BLANK")

      positive_control_names <- ""
      negative_control_names <- ""
      standard_curve_names <- ""

      if (include_names) {
        if (positive_control_num > 0) {
          positive_control_names <- self$sample_names[self$sample_types == "POSITIVE CONTROL"]
          positive_control_names <- paste(sapply(positive_control_names, function(sample) paste0("'", sample, "'")), collapse = ", ")
          positive_control_names <- paste0("\nSample names: ", positive_control_names)
        }

        if (negative_control_num > 0) {
          negative_control_names <- self$sample_names[self$sample_types == "NEGATIVE CONTROL"]
          negative_control_names <- paste(sapply(negative_control_names, function(sample) paste0("'", sample, "'")), collapse = ", ")
          negative_control_names <- paste0("\nSample names: ", negative_control_names)
        }
        if (standard_curve_num > 0) {
          standard_curve_names <- self$sample_names[self$sample_types == "STANDARD CURVE"]
          standard_curve_names <- paste(sapply(standard_curve_names, function(sample) paste0("'", sample, "'")), collapse = ", ")
          standard_curve_names <- paste0("\nSample names: ", standard_curve_names)
        }
      }

      cat(
        "Summary of the plate with name '", self$plate_name, "':\n",
        "Plate examination date: ",
        as.character(self$plate_datetime), "\n",
        "Total number of samples: ",
        length(self$sample_names), "\n",
        "Number of blank samples: ",
        blank_samples_num, "\n",
        "Number of standard curve samples: ",
        standard_curve_num,
        standard_curve_names, "\n",
        "Number of positive control samples: ",
        positive_control_num,
        positive_control_names, "\n",
        "Number of negative control samples: ",
        negative_control_num,
        negative_control_names, "\n",
        "Number of test samples: ",
        test_samples_num, "\n",
        "Number of analytes: ",
        length(self$analyte_names), "\n",
        sep = ""
      )

      invisible(self)
    },


    #' Get data for a specific analyte and sample type
    #' @description
    #' Function returns data for a specific analyte and sample.
    #'
    #' @param analyte An analyte name or its id of which data we want to extract.
    #'  If set to 'ALL' returns data for all analytes.
    #'
    #' @param sample_type is a type of the sample we want to extract data from.
    #'  The possible values are \cr \code{c(`r toString(VALID_SAMPLE_TYPES)`)}. Default value is `ALL`.
    #' @param data_type The parameter specifying which data type should be returned.
    #'  This parameter has to take one of values: \cr \code{c(`r toString(VALID_DATA_TYPES)`)}.
    #'  What's more, the `data_type` has to be present in the plate's data
    #'  Default value is plate's `default_data_type`, which is usually `Median`.
    #'
    #' @return Dataframe containing information about a given sample type and analyte
    get_data = function(analyte, sample_type = "ALL", data_type = self$default_data_type) {
      # check if the analyte exists in analytes_names
      if (!is.null(analyte) && !is.na(analyte)) {
        if (!(analyte %in% c(self$analyte_names, "ALL"))) {
          stop("Analyte ", analyte, " does not exist in plate's field analyte_names")
        }
      } else {
        stop("Passed analyte is either NULL or NA")
      }

      # check if the sample_type is a valid sample type
      if (!is.null(sample_type) && !is.na(sample_type)) {
        if (!is_valid_sample_type(sample_type)) {
          stop("Sample type ", sample_type, " is not a valid sample type")
        }
      } else {
        stop("Passed sample type is either NULL or NA")
      }

      # check if the data_type is a valid data type
      if (!is.null(data_type) && !is.na(data_type)) {
        if (!is_valid_data_type(data_type)) {
          stop("Data type ", data_type, " is not a valid data type")
        } else if (!data_type %in% names(self$data)) {
          stop("Data type ", data_type, " does not exist in the plate")
        }
      } else {
        stop("Passed data type is either NULL or NA")
      }

      # get samples of the given type, data_type and analyte and return them
      if (sample_type == "ALL") {
        valid_samples <- rep(TRUE, length(self$sample_types))
      } else {
        valid_samples <- self$sample_types == sample_type
      }

      data_of_specified_type <- self$data[[data_type]]
      if (analyte == "ALL") {
        return(data_of_specified_type[valid_samples, ])
      } else {
        return(data_of_specified_type[valid_samples, analyte])
      }
    },

    #' Get the string representation of dilutions
    #' @description
    #' Function returns the dilution represented as strings for a specific sample type.
    #' @param sample_type type of the samples that we want to obtain the dilution for.
    #' The possible values are \cr \code{c(`r toString(VALID_SAMPLE_TYPES)`)} Default value is `ALL`.
    #' @return
    #' A list containing names of the samples as keys and string representing dilutions as values.
    get_dilution = function(sample_type) {
      if (!is_valid_sample_type(sample_type)) {
        stop("Sample type ", sample_type, " is not a valid sample type")
      }
      if (is.null(self$dilutions)) {
        stop("Plate does not have dilutions set.
             The plate object is probably not initialized properly,
             or your data is incorrectly formatted.")
      }
      if (sample_type == "ALL") {
        return(self$dilutions)
      } else {
        return(self$dilutions[self$sample_types == sample_type])
      }
    },

    #' Get the numeric representation of dilutions
    #' @description
    #' Function returns the dilution values for a specific sample type.
    #' @param sample_type type of the samples that we want to obtain the dilution values for.
    #' The possible values are \cr \code{c(`r toString(VALID_SAMPLE_TYPES)`)} Default value is `ALL`.
    #' @return
    #' A list containing names of the samples as keys and numeric values representing dilutions as values.
    #'
    get_dilution_values = function(sample_type) {
      if (!is_valid_sample_type(sample_type)) {
        stop("Sample type ", sample_type, " is not a valid sample type")
      }
      if (is.null(self$dilution_values)) {
        stop("Plate does not have dilution values set.
             Check your layout or luminex file,
             and ensure it contains dilution information that is correctly formatted")
      }
      if (sample_type == "ALL") {
        return(self$dilution_values)
      } else {
        return(self$dilution_values[self$sample_types == sample_type])
      }
    },

    #' Adjust the MFI values by subtracting the background
    #' @description
    #' Function adjusts the values of samples (all samples excluding the blanks) by clamping the
    #' values to the aggregated value of the `BLANK` samples for each analyte separately.
    #'
    #' The purpose of this operation is to unify the data by clamping values below the background noise.
    #' how this method works was inspired by the paper https://doi.org/10.1038/s41598-020-57876-0 which covers the quality control in the MBA.
    #'
    #' In short, this operation firstly calculates the aggregate of MFI in the `BLANK` samples
    #' (available methods are: `min`, `max`, `mean`, `median`)
    #' and then replaces all values below this threshold with the threshold value.
    #'
    #' Method does not modifies the data of type `Count`.
    #'
    #'  This operation is recommended to be performed before any further analysis, but is optional.
    #'  Skipping it before further analysis is allowed, but will result in a warning.
    #'
    #' @param threshold The method used to calculate the background value for each analyte.
    #' Every value below this threshold will be clamped to the threshold value.
    #' By default `max`. Available methods are: `min`, `max`, `mean`, `median`.
    #' @param in_place Whether the method should produce new plate with adjusted
    #' values or not, By default `TRUE` - operates on the current plate.
    blank_adjustment = function(threshold = "max", in_place = TRUE) {
      if (self$blank_adjusted) {
        stop("Blank values have been already adjusted in this plate,
             If you want to try doing it using different method, consider reversing this process")
      }
      method <- switch(threshold,
        "min" = min,
        "max" = max,
        "mean" = mean,
        "median" = median,
        stop(threshold, " not available for `threshold`, consider using one of the following: ", available_methods)
      )

      plate <- if (in_place) self else self$clone(deep = TRUE)

      blanks_filter <- plate$sample_types == "BLANK"
      if (!any(blanks_filter)) {
        stop("No blank samples found in the plate, cannot perform blank adjustment")
      }

      for (datatype in names(plate$data)) {
        if (datatype == "Count") {
          next
        }

        df <- plate$data[[datatype]]
        blanks_df <- df[blanks_filter, , drop = FALSE]
        clamp_value <- as.numeric(apply(blanks_df, 2, method))

        for (col in seq_len(ncol(df))) {
          df[(!blanks_filter), col] <- clamp(df[(!blanks_filter), col], lower = clamp_value[col])
        }

        plate$data[[datatype]] <- df
      }

      plate$blank_adjusted <- TRUE
      return(plate)
    }
  ),
  private = list(

    ## Private Fields ---------------------------------------------------------
    verbose = TRUE
  )
)


#' @export
summary.Plate <- function(object, ...) {
  object$summary(...)
}

#' @export
as.data.frame.Plate <- function(x, row.names, optional, ...) {
  x$get_data("ALL", "ALL")
}

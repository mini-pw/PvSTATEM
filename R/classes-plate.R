#'
#'

VALID_SAMPLE_TYPES <- c(
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
  "Avg net MFI",
  "Mean",
  "%CV",
  "Peak",
  "Std Dev"
)

globalVariables(c("VALID_SAMPLE_TYPES", "VALID_DATA_TYPES"))

#' Check if the sample type is valid. The sample type is valid if it is one of the
#' elements of the `VALID_SAMPLE_TYPES` vector.
#'
#' @param sample_type A string representing the sample type.
#' @return `TRUE` if the sample type is valid, `FALSE` otherwise.
#'
#' @export
is_valid_sample_type <- function(sample_type) {
  sample_type %in% VALID_SAMPLE_TYPES
}

#' Check if the data type is valid. The data type is valid if it is one of the
#' elements of the `VALID_DATA_TYPES` vector.
#'
#' @param data_type A string representing the data type.
#' @return `TRUE` if the data type is valid, `FALSE` otherwise.
#'
#' @export
is_valid_data_type <- function(data_type) {
  data_type %in% VALID_DATA_TYPES
}


#' @title Plate
#' @description
#' A class to represent the luminex plate. It contains information about
#' the samples and analytes that were examined on the plate as well as
#' some additional metadata and batch info
Plate <- R6::R6Class(
  "Plate",
  public = list(

    ## Fields ----------------------------------------------------------------
    ## Must be set ---
    plate_name = "",
    analyte_names = character(),
    sample_names = character(),
    ## Must be set if validated ---
    sample_locations = NULL,
    sample_types = NULL,
    dilutions = NULL,
    dilution_values = NULL,
    data = NULL,
    default_data_type = NULL,
    batch_info = NULL,

    ## Methods ---------------------------------------------------------------

    #' @description
    #' Method to initialize the Plate object
    initialize = function(plate_name, sample_names, analyte_names,
                          dilutions = NULL, dilution_values = NULL,
                          sample_types = NULL, data = NULL,
                          sample_locations = NULL, default_data_type = NULL, batch_info = NULL) {
      self$plate_name <- plate_name
      self$analyte_names <- analyte_names
      self$sample_names <- sample_names
      if (!is.null(sample_locations)) self$sample_locations <- sample_locations
      if (!is.null(dilutions)) self$dilutions <- dilutions
      if (!is.null(dilution_values)) self$dilution_values <- dilution_values
      if (!is.null(sample_types)) self$sample_types <- sample_types
      if (!is.null(data)) self$data <- data
      if (!is.null(default_data_type)) self$default_data_type <- default_data_type
      if (!is.null(batch_info)) self$batch_info <- batch_info
    },

    #' @description
    #' Function prints the basic information about the plate
    #' such as the number of samples and analytes
    print = function(...) {
      cat(
        "Plate with",
        length(self$sample_names),
        "samples and",
        length(self$analyte_names),
        "analytes\n"
      )
    },

    #' @description
    #' Function outputs basic information about the plate, such as
    #' examination date, batch name, and sample types
    #'
    #' @param include_names If `include_names` parameter is `TRUE`, a
    #' part from count of control samples, provides also their names.
    #' By default `FALSE`
    summary = function(..., include_names = FALSE) {},


    #' @description
    #' Function returns data for a specific analyte and sample.
    #'
    #' @param analyte An analyte name or its id of which data we want to extract
    #'
    #' @param sample sample name or id
    #' @param data_type if `NULL` returns whole column of the dataframe
    #' containing information about the sample. Default value is plate's
    #' `data_type_used` usually `Median`.
    #'
    #' @return Data about a sample and analyte
    get_data = function(analyte, sample_type, data_type = self$data_type_used) {
      # check if the analyte exists in analytes_names
      if (!is.null(analyte) && !is.na(analyte)) {
        if (!analyte %in% self$analyte_names) {
          stop("Analyte does not exist in analytes_names")
        }
      } else {
        stop("Analyte is either NULL or NA")
      }

      # check if the sample_type is a valid sample type
      if (!is.null(sample_type) && !is.na(sample_type)) {
        if (!is_valid_sample_type(sample_type)) {
          stop("Sample type is not a valid sample type")
        }
      } else {
        stop("Sample type is either NULL or NA")
      }

      # check if the data_type is a valid data type
      if (!is.null(data_type) && !is.na(data_type)) {
        if (!is_valid_data_type(data_type)) {
          stop("Data type is not a valid data type")
        }
      } else {
        stop("Data type is either NULL or NA")
      }

      # get samples of the given type, data_type and analyte and return them
      valid_samples <- self$sample_types == sample_type
      data_of_specified_type <- self$data[[data_type]]
      return(data_of_specified_type[valid_samples, analyte])
    },

    #'
    get_dilution = function(sample_type) {
      if (!is_valid_sample_type(sample_type)) {
        stop("Sample type is not a valid sample type")
      }
      if (is.null(self$dilutions)) {
        stop("Dilutions are not set for the plate")
      }
      return(self$dilutions[self$sample_types == sample_type])
    },

    #'
    get_dilution_values = function(sample_type) {
      if (!is_valid_sample_type(sample_type)) {
        stop("Sample type is not a valid sample type")
      }
      if (is.null(self$dilution_values)) {
        stop("Dilution values are not set for the plate")
      }
      return(self$dilution_values[self$sample_types == sample_type])
    },

    #' @description
    #' Function adjusts the values of test samples by subtracting average of
    #' BLANK samples purpose of this operation is to remove background light
    #' In short it subtracts the values from data in all samples, except from
    #' Blanks. It does not subtract values from `Count` values, even if this
    #' step seems logical, people from the lab do not do it always.
    #'
    #' @param method How the values of different blanks should be aggregated.
    #' By default `avg`. For now it is the only available method
    #' @param inplace Whether the method should produce new plate with adjusted
    #' values or not, By default `TRUE` - operates on the current plate.
    blank_adjustment = function(method = "avg", in_place = "TRUE") {},

    #' @description
    #' Function takes the data 1D array and returns the data in the form of
    #' a 2D array, that represents the spatial arrangement of the samples
    #' on the plate, ie. 12x8 grid.
    #'
    #' @param data 1D array of data represents for example counts, dilutions
    #' or sample types
    #'
    #' @return A 2D array representing data with the spatial arrangement
    #' of the samples
    layout = function(data) {}
  ),
  private = list(

    ## Private Fields ---------------------------------------------------------
    blank_adjusted = FALSE,
    verbose = TRUE
  )
)

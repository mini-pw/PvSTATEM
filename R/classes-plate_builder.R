#' @title PlateBuilder
#'
#' @export
PlateBuilder <- R6::R6Class(
  "PlateBuilder",
  public = list(
    plate_name = NULL,
    analyte_names = NULL,
    sample_names = NULL,
    sample_locations = NULL,
    dilutions = NULL,
    dilution_values = NULL,
    sample_types = NULL,
    data = NULL,
    data_type_used = "Median",
    batch_info = NULL,


    #' @description
    #' Initialize the PlateBuilder object
    #'
    #' @param plate_name - plate name obtained from filename
    #' @param sample_names - vector of sample names measured during
    #' an examination in the same order as in the data
    #' @param analyte_names - vector of analytes names measured during
    #' an examination in the same order as in the data
    #'
    initialize = function(plate_name, sample_names, analyte_names) {
      stopifnot(is.character(plate_name) && is.scalar(plate_name))
      self$plate_name <- plate_name
      stopifnot(is.character(sample_names) && length(sample_names) > 0)
      self$sample_names <- sample_names
      stopifnot(is.character(analyte_names) && length(analyte_names) > 0)
      self$analyte_names <- analyte_names
    },


    #' @description
    #' Set the sample types used during the examination
    #' @param sample_locations vector of sample locations pretty name ie. A1, B2
    set_sample_locations = function(sample_locations) {
      stopifnot(is.character(sample_locations) && length(sample_locations) > 0)
      stopifnot(length(sample_locations) == length(self$sample_names))
      stopifnot(all(stringr::str_detect(sample_locations, "^[A-Z][0-9]+$")))

      self$sample_locations <- sample_locations
    },

    #' @description
    #' Set the dilutions (end extract numeric values) used during the examination
    #' @param dilutions vector of dilutions used during the examination
    #' due to the nature of data it's a vector of strings,
    #' the numeric vales are created from those strings
    set_dilutions = function(dilutions) {
      stopifnot(is.character(dilutions) && length(dilutions) > 0)
      stopifnot(length(dilutions) == length(self$sample_names))
      self$dilutions <- dilutions
      self$dilution_values <- convert_dilutions_to_numeric(dilutions)
    },

    #' @description
    #' Use the sample names to extract the sample types
    extract_sample_types = function() {
      stopifnot(is.null(self$sample_types))
      sample_types <- translate_sample_names_to_sample_types(self$sample_names)
      for (sample_type in sample_types) {
        if (!sample_type %in% private$valid_sample_types) {
          stop("Sample type `", sample_type, "` is not a valid sample type")
        }
      }
      self$sample_types <- sample_types
    },

    #' @description
    #' Set the data used during the examination
    #' @param data a named list of data frames containing information about
    #' the samples and analytes. The list is named by the type of the data
    #' e.g. `Median`, `Net MFI`, etc.
    #' The data frames contain information about the samples and analytes
    #' The rows are different measures, whereas the columns represent
    #' different analytes
    #' Example of how `data$Median` looks like:
    #' | Sample  | Analyte1 | Analyte2 | Analyte3 |
    #' |---------|----------|----------|----------|
    #' | Sample1 | 1.2      | 2.3      | 3.4      |
    #' | Sample2 | 4.5      | 5.6      | 6.7      |
    #' | ...     | ...      | ...      | ...      |
    #' | Sample96| 7.8      | 8.9      | 9.0      |
    set_data = function(data) {
      stopifnot(is.list(data))
      stopifnot(length(data) > 0)
      for (name in names(data)) {
        if (!name %in% private$valid_data_types) {
          stop("Data type `", name, "` is not a valid data type")
        }
      }
      stopifnot(all(sapply(data, function(x) is.data.frame(x))))
      for (data_type_df in data) {
        if (nrow(data_type_df) != length(self$sample_names)) {
          stop("Number of rows in data frame does not match the number of samples")
        }
        if (ncol(data_type_df) != length(self$analyte_names)) {
          stop("Number of columns in data frame does not match the number of analytes")
        }
        for (colname in colnames(data_type_df)) {
          if (!colname %in% self$analyte_names) {
            stop("Column `", colname, "` is not a valid analyte name")
          }
        }
        for (rowname in rownames(data_type_df)) {
          if (!rowname %in% self$sample_names) {
            stop("Row `", rowname, "` is not a valid sample name")
          }
        }
      }

      self$data <- data
    },

    #' @description
    #' Set the data type used for calculations
    #' @param data_type a character value representing the type of data
    #' that is currently used for calculations. By default, it is set to Median
    set_default_data_type = function(data_type = "Median") {
      stopifnot(data_type %in% private$valid_data_types)
      self$default_data_type <- data_type
    },

    #' @description
    #' Set the batch info for the plate
    #' @param batch_info a raw list containing metadata about
    #' the plate read from the Luminex file
    set_batch_info = function(batch_info) {
      stopifnot(is.list(batch_info))
      self$batch_info <- batch_info
    },

    #' @description
    #' Create a Plate object from the PlateBuilder object
    build = function(validate = TRUE) {
      if (validate) {
        self$validate()
      }
      Plate$new(
        plate_name = self$plate_name,
        analyte_names = self$analyte_names,
        sample_names = self$sample_names,
        sample_locations = self$sample_locations,
        dilutions = self$dilutions,
        dilution_values = self$dilution_values,
        sample_types = self$sample_types,
        data = self$data,
        default_data_type = self$default_data_type,
        batch_info = self$batch_info
      )
    }
  ),
  private = list(
    valid_sample_types = c(
      "BLANK",
      "TEST",
      "NEGATIVE CONTROL",
      "STANDARD CURVE",
      "POSITIVE CONTROL"
    ),
    valid_data_types = c(
      "Median",
      "Net MFI",
      "Count",
      "Avg net MFI",
      "Mean",
      "%CV",
      "Peak",
      "Std Dev"
    ),
    validate = function() {
      errors <- list()
      if (lengh(self$sample_names) != length(self$sample_locations)) {
        append(errors, "Length of sample_names and sample_locations is not equal")
      }
      if (lengh(self$sample_names) != length(self$dilutions)) {
        append(errors, "Length of sample_names and dilutions is not equal")
      }
      if (lengh(self$sample_names) != length(self$sample_types)) {
        append(errors, "Length of sample_names and sample_types is not equal")
      }
      if (!(self$data_type_used %in% self$valid_data_types)) {
        append(errors, "Data type used is not valid")
      }
      if (length(self$data) == 0) {
        append(errors, "Data is empty")
      }
      if (!(self$data_type_used %in% names(self$data))) {
        append(errors, "Data type used is not present in data")
      }
      if (length(self$analyte_names) == 0) {
        append(errors, "Analyte names are empty")
      }
    }
  )
)


convert_dilutions_to_numeric <- function(dilutions) {
  stopifnot(is.character(dilutions))
  splitted_dilutions <- stringr::str_split(dilutions, "/")
  for (splitted_dilution in splitted_dilutions) {
    stopifnot(length(splitted_dilution) == 2)
    stopifnot(all())
  }
  as.numeric(sapply(splitted_dilutions, function(x) {
    x <- as.numeric(x)
    x[1] / x[2]
  }))
}

#' @description
#' Function translates sample names to sample types
#' The function uses regular expressions to match the sample names
#' to the sample types
#'
#' @param sample_names A vector of sample names
#' @return A vector of sample types
#'
#' @examples
#' translate_sample_names_to_sample_types(c("B", "BLANK", "TEST1"))
#' translate_sample_names_to_sample_types(c("S", "CP3"))
#'
translate_sample_names_to_sample_types <- function(sample_names) {
  # TODO: Move the tymek code here
  rep("TEST", length(sample_names))
}

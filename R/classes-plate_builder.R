#' @title PlateBuilder
#'
#' @import R6
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
    default_data_type = "Median",
    batch_info = NULL,
    layout = NULL,


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

    #' Use the sample names to extract the sample types
    #'
    #' @param use_layout_types logical value indicating whether to use names extracted from layout files
    #' to extract sample types
    #'
    #' @param values a vector of sample types to overwrite the extraction process
    #'
    extract_sample_types = function(use_layout_types = TRUE, values = NULL) {
      stopifnot(is.null(self$sample_types))
      if (!is.null(values)) {
        sample_types <- values
      } else if (use_layout_types) {
        if (is.null(self$layout)) {
          stop("Layout is not provided. But `use_layout_types` is set to `TRUE`")
        }
        layout_names <- c(self$layout) # TODO: Make it into a function and explain
        sample_types <- translate_sample_names_to_sample_types(self$sample_names, layout_names)
      } else {
        sample_types <- translate_sample_names_to_sample_types(self$sample_names)
      }
      for (sample_type in sample_types) {
        if (!is_valid_sample_type(sample_type)) {
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
        if (!is_valid_data_type(name)) {
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
      }

      self$data <- data
    },

    #' @description
    #' Set the data type used for calculations
    #' @param data_type a character value representing the type of data
    #' that is currently used for calculations. By default, it is set to Median
    set_default_data_type = function(data_type = "Median") {
      stopifnot(is_valid_data_type(data_type))
      self$default_data_type <- data_type
    },

    #' @description
    #' Set the batch info for the plate
    #' @param batch_info a raw list containing metadata about
    #' the plate read from the Luminex file
    set_batch_info = function(batch_info) {
      self$batch_info <- batch_info
    },

    #' @description
    #' Set the layout matrix for the plate
    #' @param layout_matrix a matrix containing information about the
    set_layout = function(layout_matrix) {
      stopifnot(is.matrix(layout_matrix))
      # TODO: Additional validation probably needed
      self$layout <- layout_matrix
    },

    #' @description
    #' Create a Plate object from the PlateBuilder object
    build = function(validate = TRUE) {
      if (validate) {
        private$validate()
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
        batch_info = self$batch_info,
        layout = self$layout
      )
    }
  ),
  private = list(
    validate = function() {
      errors <- list()
      if (length(self$sample_names) != length(self$sample_locations)) {
        append(errors, "Length of sample_names and sample_locations is not equal")
      }
      if (length(self$sample_names) != length(self$dilutions)) {
        append(errors, "Length of sample_names and dilutions is not equal")
      }
      if (length(self$sample_names) != length(self$sample_types)) {
        append(errors, "Length of sample_names and sample_types is not equal")
      }
      if (!is_valid_data_type(self$default_data_type)) {
        append(errors, "Data type used is not valid")
      }
      if (length(self$data) == 0) {
        append(errors, "Data is empty")
      }
      if (!(self$default_data_type %in% names(self$data))) {
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
  non_na_filter <- !is.na(dilutions)

  splitted_dilutions <- stringr::str_split(dilutions[non_na_filter], "/")
  for (splitted_dilution in splitted_dilutions) {
    stopifnot(length(splitted_dilution) == 2)
    stopifnot(all())
  }

  dilution_values <- rep(NA, length(dilutions))
  dilution_values[non_na_filter] <- as.numeric(sapply(splitted_dilutions, function(x) {
    x <- as.numeric(x)
    x[1] / x[2]
  }))
  dilution_values
}

#' Translate sample names to sample types
#'
#' @description
#' Function translates sample names to sample types based on the sample name
#' from Luminex file and the sample name from the layout file, which may not
#' be provided. The function uses regular expressions to match the sample names
#' to the sample types.
#'
#' It parses the names as follows:
#' If `sample_names` or `sample_names_from_layout` equals to `BLANK`, `BACKGROUND` or `B`,
#' then SampleType equals to `BLANK`
#' If `sample_names` or `sample_names_from_layout` equals to `STANDARD CURVE`,
#' `SC`, `S`, contains substring `1/\d+` and has prefix ` `, `S_`, `S `,
#' `S` or `CP3`, then SampleType equals to `STANDARD CURVE`
#' If `sample_names` or `sample_names_from_layout` equals to `NEGATIVE CONTROL`, `N`,
#' or contains substring `NEG`, then SampleType equals to `NEGATIVE CONTROL`
#' If `sample_names` or `sample_names_from_layout` starts with `P` followed by
#' whitespace, `POS` followed by whitespace, some sample name followed by
#' substring `1/\d+` SampleType equals to `POSITIVE CONTROL`
#' otherwise, the returned SampleType is `TEST`
#'
#' @param sample_names (`character`)\cr
#' Vector of sample names from Luminex file
#'
#' @param sample_names_from_layout (`character`)\cr
#' Vector of sample names from Layout file
#' values in this vector may be different than `sample_names` and may
#' contain additional information about the sample type like dilution
#'
#' @return A vector of valid sample_type strings
#'
#' @examples
#' translate_sample_names_to_sample_types(c("B", "BLANK", "TEST1"))
#' translate_sample_names_to_sample_types(c("S", "CP3"))
#'
#' @export
translate_sample_names_to_sample_types <- function(sample_names, sample_names_from_layout = "") {
  # Handle case when sample name from layout is not provided
  # Ensure sample_names_from_layout is a character vector of the same length as sample_names
  if (length(sample_names_from_layout) != length(sample_names)) {
    sample_names_from_layout <- rep("", length(sample_names))
  }
  # Initialize the result vector
  sample_types <- vector("character", length(sample_names))
  # Iterate over each sample
  for (i in seq_along(sample_names)) {
    name <- sample_names[i]
    name_layout <- sample_names_from_layout[i]
    # Default sample type
    sample_type <- "TEST"
    # Check if the sample is a blank
    blank_types <- c("BLANK", "BACKGROUND", "B")
    if (name %in% blank_types || name_layout %in% blank_types) {
      sample_type <- "BLANK"
    }
    # Check if the sample is a positive control
    positive_control_pattern <- "^(P.|POS.+|[A-Za-z0-9/-_]+ )(1/\\d+)$"
    if (grepl(positive_control_pattern, name) || grepl(positive_control_pattern, name_layout)) {
      sample_type <- "POSITIVE CONTROL"
    }
    # Check if the sample is a negative control
    negative_types <- c("NEGATIVE CONTROL", "N")
    negative_pattern <- "^(N..|.*\\bNEG\\b)"
    if (name %in% negative_types || grepl(negative_pattern, name) || grepl(negative_pattern, name_layout)) {
      sample_type <- "NEGATIVE CONTROL"
    }
    # Check if the sample is a standard curve
    standard_curve_types <- c("STANDARD CURVE", "SC", "S")
    standard_curve_pattern <- "^(S_|S|S\\s|CP.+)(1/\\d+)$"
    standard_curve_loc_pattern <- "^(1/\\d+)$"
    if (name %in% standard_curve_types || grepl(standard_curve_pattern, name) || grepl(standard_curve_loc_pattern, name_layout)) {
      sample_type <- "STANDARD CURVE"
    }
    # Assign the determined sample type
    sample_types[i] <- sample_type
  }

  return(sample_types)
}

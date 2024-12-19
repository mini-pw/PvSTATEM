#' @title PlateBuilder
#'
#' @description
#' This class helps creating the Plate object.
#' It is used to store the data and validate the final fields.
#'
#' @import R6
#'
#' @export
PlateBuilder <- R6::R6Class(
  "PlateBuilder",
  public = list(
    plate_name = NULL,
    sample_names = NULL,
    analyte_names = NULL,
    batch_name = NULL,
    plate_datetime = NULL,
    sample_locations = NULL,
    sample_types = NULL,
    dilutions = NULL,
    dilution_values = NULL,
    default_data_type = "Median",
    data = NULL,
    batch_info = NULL,
    layout = NULL,


    #' @description
    #' Initialize the PlateBuilder object
    #'
    #' @param sample_names - vector of sample names measured during
    #' an examination in the same order as in the data
    #'
    #' @param analyte_names - vector of analytes names measured during
    #' an examination in the same order as in the data
    #'
    #' @param batch_name - name of the batch during which the plate was examined
    #' obtained from the plate info. An optional parameter, by default set to
    #' `""` - an empty string.
    #'
    #' @param verbose - logical value indicating whether to print additional
    #' information. This parameter is stored as a private attribute of the object
    #' and reused in other methods
    #'
    initialize = function(sample_names, analyte_names, batch_name = "", verbose = TRUE) {
      stopifnot(is.character(sample_names) && length(sample_names) > 0)
      self$sample_names <- sample_names
      stopifnot(is.character(analyte_names) && length(analyte_names) > 0)
      self$analyte_names <- analyte_names

      stopifnot(is.character(batch_name) && is.scalar(batch_name))
      self$batch_name <- batch_name

      stopifnot(is.logical(verbose) && is.scalar(verbose))
      private$verbose <- verbose
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
    #' Extract and set the dilutions from layout, sample names or use a provided vector of values.
    #' The provided vector should be the same length as the number of samples
    #' and should contain dilution factors saved as strings
    #' @param use_layout_dilutions logical value indicating whether to use names
    #' extracted from layout files to extract dilutions. If set to `FALSE` the
    #' function uses the sample names as a source for dilution
    #'
    #' @param values a vector of dilutions to overwrite the extraction process
    #'
    set_dilutions = function(use_layout_dilutions = TRUE, values = NULL) {
      stopifnot(is.null(self$dilutions))
      if (!is.null(values)) {
        dilutions <- extract_dilutions_from_layout(values)
      } else if (use_layout_dilutions) {
        if (is.null(self$layout)) {
          stop("Layout is not provided, but `use_layout_dilutions` is set to `TRUE` - cannot extract the dilutions from the layout\n")
        }
        layout_names <- self$layout_as_vector
        dilutions <- extract_dilutions_from_layout(layout_names)

        all_locations <- get_location_matrix(nrow = 8, ncol = 12)

        dilutions <- dilutions[all_locations %in% self$sample_locations]

        if (length(dilutions) != length(self$sample_names)) {
          stop("Number of layout fields is lower than the number of samples. Can't extract the dilution values")
        }
      } else {
        if (is.null(self$sample_names)) {
          stop("Sample names are not provided and `use_layout_dilutions` is set to `FALSE` - cannot extract the dilutions from sample names")
        }

        dilutions <- extract_dilution_from_names(self$sample_names)
      }

      if (all(is.na(dilutions))) {
        verbose_cat(
          "(",
          color_codes$red_start,
          "WARNING",
          color_codes$red_end,
          ")",
          "\nAll dilutions in the plate are set to NA. Please check the dilutions in the layout file or sample names.",
          verbose = private$verbose
        )
      }

      if (length(dilutions) != length(self$sample_names)) {
        stop("Number of dilutions does not match the number of samples")
      }

      self$dilutions <- dilutions
      self$dilution_values <- convert_dilutions_to_numeric(dilutions)
    },

    #' Set and extract sample types from the sample names.
    #' Optionally use the layout file to extract the sample types
    #'
    #' @param use_layout_types logical value indicating whether to use names extracted from layout files
    #' to extract sample types
    #'
    #' @param values a vector of sample types to overwrite the extraction process
    #'
    set_sample_types = function(use_layout_types = TRUE, values = NULL) {
      stopifnot(is.null(self$sample_types))
      if (!is.null(values)) {
        sample_types <- values
      } else if (use_layout_types) {
        if (is.null(self$layout)) {
          stop("Layout is not provided. But `use_layout_types` is set to `TRUE` - cannot extract the sample types from the layout")
        }
        layout_names <- self$layout_as_vector
        all_locations <- get_location_matrix(nrow = 8, ncol = 12)
        # select only the relevant samples that are mentioned in the layout file
        layout_names <- layout_names[all_locations %in% self$sample_locations]

        sample_types <- translate_sample_names_to_sample_types(self$sample_names, layout_names)
      } else {
        sample_types <- translate_sample_names_to_sample_types(self$sample_names)
      }
      for (sample_type in sample_types) {
        if (!is_valid_sample_type(sample_type)) {
          stop("Sample type `", sample_type, "` is not a valid sample type")
        }
      }

      if (!"STANDARD CURVE" %in% sample_types) {
        stop("No standard curve samples found in the plate")
      }

      self$sample_types <- sample_types
    },

    #' @description
    #' Set the sample names used during the examination. If the layout is provided,
    #' extract the sample names from the layout file. Otherwise, uses the original sample names from the Luminex file
    #'
    #' @param use_layout_sample_names logical value indicating whether
    #' to use names extracted from layout files. If set to false, this function only checks if the sample names are provided in the plate
    set_sample_names = function(use_layout_sample_names = TRUE) {
      if (use_layout_sample_names) {
        if (is.null(self$layout)) {
          stop("Layout is not provided. But `use_layout_sample_names` is set to `TRUE` - cannot extract the sample names from the layout")
        }
        locations <- self$sample_locations
        layout_names <- self$layout_as_vector
        self$sample_names <- extract_sample_names_from_layout(layout_names, locations)
      }
      stopifnot(!is.null(self$sample_names))
    },

    #' @description
    #' Set the plate datetime for the plate
    #'
    #' @param plate_datetime a POSIXct datetime object
    #' representing the date and time of the examination
    set_plate_datetime = function(plate_datetime) {
      if (is.null(plate_datetime)) {
        stop("Plate datetime is not provided")
      }
      if (!lubridate::is.POSIXct(plate_datetime)) {
        stop("Plate datetime is not a valid datetime")
      }
      self$plate_datetime <- plate_datetime
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
    #' Set the plate name for the plate.
    #' The plate name is extracted from the filepath
    set_plate_name = function(file_path) {
      stopifnot(is.character(file_path) && is.scalar(file_path))
      file_name_without_extension <- sub("\\.[^.]*$", "", basename(file_path))
      self$plate_name <- file_name_without_extension
    },

    #' @description
    #' Set the layout matrix for the plate. This function performs basic validation
    #' - verifies if the plate is a matrix of shape 8x12 with 96 wells
    #' @param layout_matrix a matrix containing information about the sample names. dilutions, etc.
    set_layout = function(layout_matrix) {
      stopifnot(is.matrix(layout_matrix))
      if (nrow(layout_matrix) != 8 || ncol(layout_matrix) != 12) {
        stop("Layout matrix has to be 8x12 - a standard layout for a 96-well plate")
      }
      self$layout <- layout_matrix
    },

    #' @description
    #' Create a Plate object from the PlateBuilder object
    build = function(validate = TRUE) {
      if (validate) {
        private$validate()
      }

      plate <- Plate$new(
        plate_name = self$plate_name,
        sample_names = self$sample_names,
        analyte_names = self$analyte_names,
        batch_name = self$batch_name,
        plate_datetime = self$plate_datetime,
        sample_locations = self$sample_locations,
        sample_types = self$sample_types,
        dilutions = self$dilutions,
        dilution_values = self$dilution_values,
        default_data_type = self$default_data_type,
        data = self$data,
        batch_info = self$batch_info,
        layout = self$layout
      )

      plate
    }
  ),
  active = list(
    #' @field layout_as_vector
    #' Print the layout associated with the plate as a flattened vector of values.
    layout_as_vector = function() {
      if (is.null(self$layout)) {
        stop("Layout is not provided")
      }
      c(t(self$layout))
    }
  ),
  private = list(
    verbose = TRUE,
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
      # BUG:: Issue #164 - Unhandled errors
    }
  )
)

#' Extract sample names from layout
#' @description
#' Function extracts sample names from the layout file based on the provided locations.
#' Function assumes that the plate is 96-well and extracts
#' the sample names according to the provided location strings.
#' @param layout_names a vector of sample names from the layout file
#' @param locations a vector of locations in the form of A1, B2, etc.
#' @keywords internal
extract_sample_names_from_layout <- function(layout_names, locations) {
  stopifnot(is.character(layout_names) && length(layout_names) > 0)
  stopifnot(is.character(locations) && length(locations) > 0)

  all_locations <- get_location_matrix(nrow = 8, ncol = 12)

  stopifnot(length(layout_names) == length(all_locations))

  sample_names <- layout_names[all_locations %in% locations]
  sample_names
}

#' Extract dilution factor from the sample name
#' @description
#' function extracts dilution factor from the sample name - useful for detecting
#' dilution from sample names
#' @param sample_name a vector of sample names from which we want to extract the dilutions
#' @return a vector of dilutions represented as strings extracted from the sample names
#' @keywords internal
extract_dilution_from_names <- function(sample_name) {
  dilution_regex <- "1/\\d+"

  dilution_factor <- stringr::str_extract(sample_name, dilution_regex)

  dilution_factor
}

#' Extract dilutions from the layout representation
#' @description
#' Extract dilution factor represented as string from vector of characters.
#' The matches has to be exact and the dilution factor has to be in the form of `1/\d+`
#' @param dilutions vector of dilutions used during the examination
#' due to the nature of data it's a vector of strings,
#' the numeric vales are created from those strings
#' @keywords internal
extract_dilutions_from_layout <- function(dilutions) {
  stopifnot(is.character(dilutions) && length(dilutions) > 0)

  is_dilution_filter <- is_dilution(dilutions)
  dilutions[!is_dilution_filter] <- NA
  if (all(is.na(dilutions))) {
    warning("No valid dilutions found")
  }
  dilutions
}

is_dilution <- function(character_vector) {
  stopifnot(is.character(character_vector))

  dilution_regex <- "^\\d+/\\d+$"
  is_valid_dilution <- (!is.na(character_vector)) & (stringr::str_detect(character_vector, dilution_regex))
  is_valid_dilution
}



#' Convert dilutions to numeric values
#' @description
#' Convert dilutions saved as strings in format `1/\d+` into numeric values
#' @param dilutions vector of dilutions used during the examination saved
#' as strings in format `1/\d+`
#' @return a vector of numeric values representing the dilutions
#' @keywords internal
convert_dilutions_to_numeric <- function(dilutions) {
  stopifnot(is.character(dilutions))

  non_na_filter <- is_dilution(dilutions)

  splitted_dilutions <- stringr::str_split(dilutions[non_na_filter], "/")
  stopifnot(all(lengths(splitted_dilutions) == 2))

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
#'
#' If `sample_names` or `sample_names_from_layout` equals to `BLANK`, `BACKGROUND` or `B`,
#' then SampleType equals to `BLANK`
#'
#' If `sample_names` or `sample_names_from_layout` equals to `STANDARD CURVE`,
#' `SC`, `S`, contains substring `1/\d+` and has prefix ` `, `S_`, `S `,
#' `S` or `CP3`, then SampleType equals to `STANDARD CURVE`
#'
#' If `sample_names` or `sample_names_from_layout` equals to `NEGATIVE CONTROL`, `N`,
#' or contains substring `NEG`, then SampleType equals to `NEGATIVE CONTROL`
#'
#' If `sample_names` or `sample_names_from_layout` starts with `P` followed by
#' whitespace, `POS` followed by whitespace, some sample name followed by
#' substring `1/\d+` SampleType equals to `POSITIVE CONTROL`
#'
#' Otherwise, the returned SampleType is `TEST`
#'
#' @param sample_names (`character()`)\cr
#' Vector of sample names from Luminex file
#'
#' @param sample_names_from_layout (`character()`)\cr
#' Vector of sample names from Layout file
#' values in this vector may be different than `sample_names` and may
#' contain additional information about the sample type like dilution.
#' This vector when set has to have at least the length of `sample_names`.
#'
#' @return A vector of valid sample_type strings of length equal to the length of `sample_names`
#'
#' @examples
#' translate_sample_names_to_sample_types(c("B", "BLANK", "NEG", "TEST1"))
#' translate_sample_names_to_sample_types(c("S", "CP3"))
#'
#' @export
translate_sample_names_to_sample_types <- function(sample_names, sample_names_from_layout = NULL) {
  stopifnot(is.character(sample_names))
  # Handle case when sample name from layout is not provided
  if (is.null(sample_names_from_layout)) {
    sample_names_from_layout <- rep("", length(sample_names))
  }
  # Ensure sample_names_from_layout is a character vector at least as long as sample_names
  stopifnot(length(sample_names) <= length(sample_names_from_layout))

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



#' Generate the matrix of plate locations
#' @description
#' The function generates a matrix of plate locations. The locations are represented in a nrow x ncol matrix.
#' Usually number of rows equals to 8 and number of columns to 12, and the total matrix size is 96.
#'
#' The fields are represented as `E3`, where the letter corresponds to the row and the number to the column.
#' @param nrow Number of rows in the plate
#' @param ncol Number of columns in the plate
#' @return a matrix with locations
#' @keywords internal
get_location_matrix <- function(nrow = 8, ncol = 12) {
  rows <- rep(LETTERS[1:nrow], each = ncol)
  columns <- as.character(1:ncol)
  all_locations <- paste0(rows, columns)
  matrix(all_locations, nrow = nrow, ncol = ncol)
}

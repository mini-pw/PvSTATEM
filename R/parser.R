intelliflex_to_xponent_mapping <- VALID_DATA_TYPES
names(intelliflex_to_xponent_mapping) <- c(
  "MEDIAN", "COUNT", "NET.MEDIAN", "NET.AVERAGE.MEDIAN", "AVERAGE.MFI"
)
data_must_contain <- c("Median", "Count") # Median is a must have
location_column_name <- "Location"
sample_column_name <- "Sample"
non_analyte_columns <- c("Location", "Sample", "Total.Events")

filter_list <- function(lst, allowed_names) {
  lst[names(lst) %in% allowed_names]
}

check_data <- function(data) {
  for (must_contain in data_must_contain) {
    if (!(must_contain %in% names(data))) {
      stop("Could not find at least one of the following data types: ", paste(data_must_contain, collapse = ", "))
    }
    if (!(location_column_name %in% colnames(data[[must_contain]]))) {
      stop("Could not find location column in datatype: ", must_contain)
    }
  }
}

find_analyte_names <- function(data) {
  analytes <- colnames(data[])
  analytes <- analytes[!(analytes %in% non_analyte_columns)]
  analytes
}

remove_non_analyte_columns <- function(data) {
  for (data_type in names(data)) {
    df <- data[[data_type]]
    df <- df[, !(colnames(df) %in% non_analyte_columns)]
    data[[data_type]] <- df
  }
  data
}

parse_xponent_locations <- function(xponent_locations) {
  # Convert strings like this 77(1,E10) to E10
  regex <- "(\\d+?)\\((\\d+),(\\w\\d+)\\)"
  locations <- gsub(regex, "\\3", xponent_locations)
  locations
}

postprocess_intelliflex <- function(intelliflex_output, verbose = TRUE) {
  data <- intelliflex_output$Results
  names(data) <- intelliflex_to_xponent_mapping[names(data)]
  data <- filter_list(data, VALID_DATA_TYPES)
  check_data(data)

  analyte_names <- find_analyte_names(data$Median)
  data <- remove_non_analyte_columns(data)

  list(
    batch_name = intelliflex_output$SystemMetadata[["PLATE.NAME"]],
    sample_locations = intelliflex_output$SampleMetadata[["WELL.LOCATION"]],
    sample_names = intelliflex_output$SampleMetadata[["SAMPLE.ID"]],
    analyte_names = analyte_names,
    data = data,
    batch_info = intelliflex_output$SampleMetadata
  )
}

postprocess_xponent <- function(xponent_output, verbose = TRUE) {
  data <- xponent_output$Results
  data <- filter_list(data, VALID_DATA_TYPES)
  check_data(data)

  xponent_locations <- data$Median[[location_column_name]]
  sample_locations <- parse_xponent_locations(xponent_locations)
  sample_names <- data$Median[[sample_column_name]]
  analyte_names <- find_analyte_names(data$Median)
  data <- remove_non_analyte_columns(data)

  list(
    batch_name = xponent_output$ProgramMetadata[["Batch"]],
    sample_locations = sample_locations,
    sample_names = sample_names,
    analyte_names = analyte_names,
    data = data,
    batch_info = xponent_output$Header
  )
}


valid_formats <- c("xPONENT", "INTELLIFLEX")

#' Read Luminex Data
#'
#' @description
#' Reads a file containing Luminex data and returns a Plate object.
#' If provided, can also read a layout file, which usually contains
#' information about the sample names, sample types or its dilutions.
#'
#' The function is capable of reading data in two different formats:
#' - xPONENT
#' - INTELLIFLEX
#' which are produced by two different Luminex machines.
#'
#'
#' @param plate_filepath Path to the Luminex plate file
#' @param layout_filepath Path to the Luminex layout file
#' @param format The format of the Luminex data. Select from: xPONENT, INTELLIFLEX
#' @param plate_file_separator The separator used in the plate file
#' @param plate_file_encoding The encoding used in the plate file
#' @param use_layout_sample_names Whether to use names from the layout file in extracting sample names.
#' @param use_layout_types Whether to use names from the layout file in extracting sample types.
#' Works only when layout file is provided
#' @param use_layout_dilutions Whether to use dilutions from the layout file in extracting dilutions.
#' Works only when layout file is provided
#' @param default_data_type The default data type to use if none is specified
#' @param sample_types a vector of sample types to use instead of the extracted ones
#' @param dilutions a vector of dilutions to use instead of the extracted ones
#' @param verbose Whether to print additional information and warnings. `TRUE` by default
#'
#' @return Plate file containing the Luminex data
#'
#' @examples
#' plate_file <- system.file("extdata", "CovidOISExPONTENT.csv", package = "PvSTATEM")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_layout.csv", package = "PvSTATEM")
#' plate <- read_luminex_data(plate_file, layout_file)
#'
#' plate_file <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "PvSTATEM")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_CO_layout.xlsx", package = "PvSTATEM")
#' # To suppress warnings and additional information use verbose = FALSE
#' plate <- read_luminex_data(plate_file, layout_file, verbose = FALSE)
#'
#' @export
read_luminex_data <- function(plate_filepath,
                              layout_filepath = NULL,
                              format = "xPONENT",
                              plate_file_separator = ",",
                              plate_file_encoding = "UTF-8",
                              use_layout_sample_names = TRUE,
                              use_layout_types = TRUE,
                              use_layout_dilutions = TRUE,
                              default_data_type = "Median",
                              sample_types = NULL,
                              dilutions = NULL,
                              verbose = TRUE) {
  if (!(format %in% valid_formats)) {
    stop("Invalid format: ", format, ". Select from: ", paste(valid_formats, collapse = ", "))
  }

  verbose_cat("Reading Luminex data from: ", plate_filepath, "\nusing format ", format, "\n", verbose = verbose)

  parser_output <- switch(format,
    "xPONENT" = {
      output <- read_xponent_format(plate_filepath, verbose = verbose)
      postprocess_xponent(output, verbose = verbose)
    },
    "INTELLIFLEX" = {
      output <- read_intelliflex_format(plate_filepath, verbose = verbose)
      postprocess_intelliflex(output, verbose = verbose)
    }
  )

  plate_builder <- PlateBuilder$new(
    batch_name = parser_output$batch_name,
    sample_names = parser_output$sample_names,
    analyte_names = parser_output$analyte_names,
    verbose = verbose
  )

  plate_builder$set_plate_name(plate_filepath) # set a new plate name based on the file name

  plate_builder$set_sample_locations(parser_output$sample_locations)
  layout_matrix <- NULL
  if (!is.null(layout_filepath)) {
    layout_matrix <- read_layout_data(layout_filepath)
    plate_builder$set_layout(layout_matrix)
  }
  if (is.null(layout_filepath) && (use_layout_types || use_layout_dilutions || use_layout_sample_names)) {
    use_layout_types <- FALSE
    use_layout_sample_names <- FALSE
    use_layout_dilutions <- FALSE
    verbose_cat(
      "(",
      color_codes$red_start,
      "WARNING",
      color_codes$red_end,
      ")",
      "\nLayout file not provided. Setting `use_layout_sample_names`,
      `use_layout_types` and `use_layout_dilutions` to FALSE.\n",
      verbose = verbose
    )
  }

  plate_builder$set_sample_names(use_layout_sample_names)

  plate_builder$set_sample_types(use_layout_types, sample_types)

  plate_builder$set_data(parser_output$data)
  plate_builder$set_default_data_type(default_data_type)
  plate_builder$set_batch_info(parser_output$batch_info)
  plate_builder$set_data(parser_output$data)

  plate_builder$set_dilutions(use_layout_dilutions, dilutions)


  plate <- plate_builder$build(validate = TRUE)

  verbose_cat(color_codes$green_start, "\nNew plate object has been created with name: ",
    plate$plate_name, "!\n", color_codes$green_end, "\n",
    verbose = verbose
  )

  plate
}

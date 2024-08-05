unified_datatypes <- c("Median", "Count", "Net MFI", "Avg Net MFI", "Mean")
intelliflex_to_xponent_mapping <- unified_datatypes
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

postprocess_intelliflex <- function(intelliflex_output) {
  data <- intelliflex_output$Results
  names(data) <- intelliflex_to_xponent_mapping[names(data)]
  data <- filter_list(data, unified_datatypes)
  check_data(data)

  analyte_names <- find_analyte_names(data$Median)
  data <- remove_non_analyte_columns(data)

  list(
    plate_name = intelliflex_output$SystemMetadata[["PLATE.NAME"]],
    sample_locations = intelliflex_output$SampleMetadata[["WELL.LOCATION"]],
    sample_names = intelliflex_output$SampleMetadata[["SAMPLE.ID"]],
    analyte_names = analyte_names,
    data = data,
    batch_info = intelliflex_output$SampleMetadata
  )
}

postprocess_xponent <- function(xponent_output) {
  data <- xponent_output$Results
  data <- filter_list(data, unified_datatypes)
  check_data(data)

  xponent_locations <- data$Median[[location_column_name]]
  sample_locations <- parse_xponent_locations(xponent_locations)
  sample_names <- data$Median[[sample_column_name]]
  analyte_names <- find_analyte_names(data$Median)
  data <- remove_non_analyte_columns(data)

  list(
    plate_name = xponent_output$ProgramMetadata[["Batch"]],
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
#' @param plate_filepath Path to the Luminex plate file
#' @param layout_filepath Path to the Luminex layout file
#' @param format The format of the Luminex data. Select from: xPONENT, INTELLIFLEX
#' @param plate_file_separator The separator used in the plate file
#' @param plate_file_encoding The encoding used in the plate file
#' @param use_layout_types Whether to use names from the layout file in extracting sample types.
#' Works only when layout file is provided
#' @param default_data_type The default data type to use if none is specified
#'
#' @return Plate file containing the Luminex data
#'
#' @export
read_luminex_data <- function(plate_filepath,
                              layout_filepath = NULL,
                              format = "xPONENT",
                              plate_file_separator = ",",
                              plate_file_encoding = "UTF-8",
                              use_layout_types = TRUE,
                              default_data_type = "Median") {
  if (!(format %in% valid_formats)) {
    stop("Invalid format: ", format, ". Select from: ", paste(valid_formats, collapse = ", "))
  }
  parser_output <- switch(format,
    "xPONENT" = {
      output <- read_xponent_format(plate_filepath)
      postprocess_xponent(output)
    },
    "INTELLIFLEX" = {
      output <- read_intelliflex_format(plate_filepath)
      postprocess_intelliflex(output)
    }
  )
  layout_matrix <- read_layout_data(layout_filepath)

  plate_builder <- PlateBuilder$new(
    parser_output$plate_name,
    parser_output$sample_names,
    parser_output$analyte_names
  )
  plate_builder$set_sample_locations(parser_output$sample_locations)
  plate_builder$set_data(parser_output$data)
  plate_builder$extract_sample_types() # HACK: This is the dummy implementation
  plate_builder$set_default_data_type(default_data_type)
  plate_builder$set_batch_info(parser_output$batch_info)
  plate_builder$set_data(parser_output$data)
  plate_builder$set_layout(layout_matrix)

  plate <- plate_builder$build(validate = FALSE) # HACK: This should be set to TRUE after the extract_sample_types

  plate
}

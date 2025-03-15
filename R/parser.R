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

#' Handle differences in datetimes
#'
#' @description
#' Handle differences in the datetime format between xPONENT and INTELLIFLEX
#' and output POSIXct datetime object containing the correct datetime with the default timezone.
#'
#' @import lubridate
#'
#' @param datetime_str The datetime string to parse
#' @param file_format The format of the file. Select from: xPONENT, INTELLIFLEX
#'
#' @return POSIXct datetime object
#'
#' @keywords internal
#'
handle_datetime <- function(datetime_str, file_format = "xPONENT") {
  if (file_format == "xPONENT") {
    possible_orders <- c(
      "Ymd HM", "mdY IM p", "mdY HM", "mdY IMS p",
      "mdY HMS", "Ymd IM p", "Ymd IMS p", "Ymd HMS",
      "dmY IM p", "dmY HM", "dmY IMS p", "dmY HMS"
    )
  } else if (file_format == "INTELLIFLEX") {
    possible_orders <- c(
      "Ymd IMS p", "Ymd HMS", "Ymd IM p", "Ymd HM",
      "mdY IMS p", "mdY HMS", "mdY IM p", "mdY HM",
      "dmY IMS p", "dmY HMS", "dmY IM p", "dmY HM"
    )
  } else {
    stop("Invalid file format: ", file_format)
  }

  first_attempt <- lubridate::parse_date_time2(datetime_str, orders = possible_orders[1], tz = "")
  if (!is.na(first_attempt)) {
    return(first_attempt)
  } else {
    message("Could not parse datetime string using default datetime format. Trying other possibilies.")
    for (order in possible_orders[-1]) {
      datetime <- lubridate::parse_date_time2(datetime_str, orders = order, tz = "")
      if (!is.na(datetime)) {
        message("Successfully parsed datetime string using order: ", order)
        return(datetime)
      }
    }
    stop("Could not parse datetime string: ", datetime_str)
  }
}

postprocess_intelliflex <- function(intelliflex_output, verbose = TRUE) {
  data <- intelliflex_output$Results
  names(data) <- intelliflex_to_xponent_mapping[names(data)]
  data <- filter_list(data, VALID_DATA_TYPES)
  check_data(data)

  datetime_str <- intelliflex_output$SystemMetadata[["PLATE.START"]]
  plate_datetime <- handle_datetime(datetime_str, "INTELLIFLEX")

  analyte_names <- find_analyte_names(data$Median)
  data <- remove_non_analyte_columns(data)

  list(
    batch_name = intelliflex_output$SystemMetadata[["PLATE.NAME"]],
    sample_locations = intelliflex_output$SampleMetadata[["WELL.LOCATION"]],
    sample_names = intelliflex_output$SampleMetadata[["SAMPLE.ID"]],
    plate_datetime = plate_datetime,
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

  datetime_str <- paste(
    xponent_output$ProgramMetadata[["Date"]],
    xponent_output$ProgramMetadata[["Time"]]
  )
  plate_datetime <- handle_datetime(datetime_str, "xPONENT")

  list(
    batch_name = xponent_output$ProgramMetadata[["Batch"]],
    plate_datetime = plate_datetime,
    sample_locations = sample_locations,
    sample_names = sample_names,
    analyte_names = analyte_names,
    data = data,
    batch_info = xponent_output$Header
  )
}


valid_formats <- c("xPONENT", "INTELLIFLEX")

#' @title
#' Read Luminex Data
#'
#' @description
#' Reads a Luminex plate file and returns a [`Plate`] object containing the extracted data.
#' Optionally, a layout file can be provided to specify the arrangement of samples on the plate.
#'
#' The function supports two Luminex data formats:
#' - **xPONENT**: Used by older Luminex machines.
#' - **INTELLIFLEX**: Used by newer Luminex devices.
#'
#' ## Workflow
#' 1. Validate input parameters, ensuring the specified format is supported.
#' 2. Read the plate file using the appropriate parser:
#'    - xPONENT files are read using [read_xponent_format()].
#'    - INTELLIFLEX files are read using [read_intelliflex_format()].
#' 3. Post-process the extracted data:
#'    - Validate required data columns (`Median`, `Count`).
#'    - Extract sample locations and analyte names.
#'    - Parse the date and time of the experiment.
#'
#' ## File Structure
#' - **Plate File (`plate_filepath`)**: A CSV file containing Luminex fluorescence intensity data.
#' - **Layout File (`layout_filepath`)** (optional): An Excel or CSV file containing the plate layout.
#'   - The layout file should contain a table with **8 rows and 12 columns**, where each cell corresponds to a well location.
#'   - The values in the table represent the sample names for each well.
#'
#' ## Sample types detection
#'
#' The [`read_luminex_data`] method automatically detects the sample types based on the sample names, unless provided the `sample_types` parameter.
#' The sample types are detected used the [`translate_sample_names_to_sample_types`] method.
#' In the documentation of this method, which can be accessed with command `?translate_sample_names_to_sample_types`, you can find the detailed description of the sample types detection.
#'
#'
#' @param plate_filepath (`character(1)`) Path to the Luminex plate file.
#' @param layout_filepath (`character(1)`, optional) Path to the Luminex layout file.
#' @param format (`character(1)`, default = `'xPONENT'`)
#'   - The format of the Luminex data file.
#'   - Supported formats: `'xPONENT'`, `'INTELLIFLEX'`.
#' @param plate_file_separator (`character(1)`, default = `','`)
#'   - The delimiter used in the plate file (CSV format). Used only for the xPONENT format.
#' @param plate_file_encoding (`character(1)`, default = `'UTF-8'`)
#'   - The encoding used for reading the plate file. Used only for the xPONENT format.
#' @param use_layout_sample_names (`logical(1)`, default = `TRUE`)
#'   - Whether to use sample names from the layout file.
#' @param use_layout_types (`logical(1)`, default = `TRUE`)
#'   - Whether to use sample types from the layout file (requires a layout file).
#' @param use_layout_dilutions (`logical(1)`, default = `TRUE`)
#'   - Whether to use dilution values from the layout file (requires a layout file).
#' @param default_data_type (`character(1)`, default = `'Median'`)
#'   - The default data type used if none is explicitly provided.
#' @param sample_types (`character()`, optional) A vector of sample types to override extracted values.
#' @param dilutions (`numeric()`, optional) A vector of dilutions to override extracted values.
#' @param verbose (`logical(1)`, default = `TRUE`)
#'   - Whether to print additional information and warnings.
#'
#' @return A [`Plate`] object containing the parsed Luminex data.
#'
#' @examples
#' # Read a Luminex plate file with an associated layout file
#' plate_file <- system.file("extdata", "CovidOISExPONTENT.csv", package = "SerolyzeR")
#' layout_file <- system.file("extdata", "CovidOISExPONTENT_layout.csv", package = "SerolyzeR")
#' plate <- read_luminex_data(plate_file, layout_file)
#'
#' # Read a Luminex plate file without a layout file
#' plate_file <- system.file("extdata", "CovidOISExPONTENT_CO.csv", package = "SerolyzeR")
#' plate <- read_luminex_data(plate_file, verbose = FALSE)
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
  tryCatch({
    parser_output <- switch(format,
      "xPONENT" = {
        output <- read_xponent_format(
          plate_filepath,
          verbose = verbose,
          separator = plate_file_separator,
          encoding = plate_file_encoding
        )
        postprocess_xponent(output, verbose = verbose)
      },
      "INTELLIFLEX" = {
        output <- read_intelliflex_format(plate_filepath, verbose = verbose)
        postprocess_intelliflex(output, verbose = verbose)
      }
    )
  }, error = function(e) {
    error_messsage <- paste("Error reading Luminex plate file from: ", plate_filepath, " using the ", format, " format.\n")

    stop("Error reading Luminex plate file from: ", plate_filepath, " using the ", format, " format.\n",
         ifelse(format == "xPONENT", paste0("Check if the separator and encoding are correct. Currently using separator: '", plate_file_separator, "' and the '", plate_file_encoding, "' encoding.\n"), ""),
         "\n", e$message)
  })


  plate_builder <- PlateBuilder$new(
    batch_name = parser_output$batch_name,
    sample_names = parser_output$sample_names,
    analyte_names = parser_output$analyte_names,
    verbose = verbose
  )

  plate_builder$set_plate_name(plate_filepath) # set a new plate name based on the file name
  plate_builder$set_plate_datetime(parser_output$plate_datetime)

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

  # Setting of samples names has to happen before setting sample types
  plate_builder$set_sample_names(use_layout_sample_names)
  plate_builder$set_sample_types(use_layout_types, sample_types)

  plate_builder$set_batch_info(parser_output$batch_info)
  plate_builder$set_default_data_type(default_data_type)
  plate_builder$set_data(parser_output$data)

  plate_builder$set_dilutions(use_layout_dilutions, dilutions)

  plate <- plate_builder$build(validate = TRUE, reorder = TRUE)

  verbose_cat(color_codes$green_start, "\nNew plate object has been created with name: ",
    plate$plate_name, "!\n", color_codes$green_end, "\n",
    verbose = verbose
  )

  plate
}

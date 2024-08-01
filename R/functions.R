#' @description
#' Parses the sample type based on the sample name from Luminex file and
#' the sample name from the layout file, which may not be provided.
#'
#' It parses the names as follows:
#' If `sample_name` or `sample_name_from_layout` equals to `BLANK`, `BACKGROUND` or `B`,
#' then SampleType equals to `BLANK`
#' If `sample_name` or `sample_name_from_layout` equals to `STANDARD CURVE`,
#' `SC`, `S` or contains substring `1/\d+` and has prefix ` `, `S_`, `S `,
#' `S` or `CP3`, then SampleType equals to `STANDARD CURVE`
#' If `sample_name` or `sample_name_from_layout` equals to `NEGATIVE CONTROL`, `N`,
#' or contains substring `NEG`, then SampleType equals to `NEGATIVE CONTROL`
#' If `sample_name` or `sample_name_from_layout` starts with `P` followed by
#' whitespace, `POS` followed by whitespace, `B770` or `10/190`
#' contains substring `1/\d+` SampleType equals to `POSITIVE CONTROL`
#' otherwise, the returned SampleType is `TEST`
#'
#' @param sample_name (`character`)\cr
#' Vector of sample names from Luminex file
#'
#' @param sample_name_from_layout (`character`)\cr
#' Vector of sample names from Layout file
#' values in this vector may be different than `sample_name` and may
#' contain additional information about the sample type like dilution
#'
#' @return A vector of valid sample_type strings
#'
#' @export
derive_sample_type <- function(sample_name,
                               sample_name_from_layout = "") {
  
  # handle case when sample name from layout is not provided
  # Ensure sample_name_from_layout is a character vector of the same length as sample_name
  if (is.null(sample_name_from_layout) || is.na(sample_name_from_layout)) {
    sample_name_from_layout <- rep("", length(sample_name))
  }

  # Initialize the result vector
  sample_types <- vector("character", length(sample_name))

  # Iterate over each sample
  for (i in seq_along(sample_name)) {
    name <- sample_name[i]
    name_layout <- sample_name_from_layout[i]

    # Default sample type
    sample_type <- "TEST"

    # Check if the sample is a blank
    blank_types <- c("BLANK", "BACKGROUND", "B")
    if (name %in% blank_types || name_layout %in% blank_types) {
      sample_type <- "BLANK"
    }

    # Check if the sample is a positive control
    positive_control_pattern <- "^(P.|POS.+|B770.+|10/198.+)(1/\\d+)$"
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
    standard_curve_loc_pattern <- "(1/\\d+)"
    if (name %in% standard_curve_types || grepl(standard_curve_pattern, name) || grepl(standard_curve_loc_pattern, name_layout)) {
      sample_type <- "STANDARD CURVE"
    }

    # Assign the determined sample type
    sample_types[i] <- sample_type
  }

  return(sample_types)
}
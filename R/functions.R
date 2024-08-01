#' @description
#' Parses the sample type based on the sample name from Luminex file and
#' the sample name from the layout file, but this one might not be provided
#'
#' It parses the names as follows:
#' If `sample_name` or `sample_name_from_layout` equals to `BLANK`, `
#' BACKGROUND` or `B`, then SampleType equals to `BLANK`
#' If `sample_name` or `sample_name_from_layout` equals to `STANDARD CURVE`,
#' `SC`, `S` or contains substring `1/\d+` and has prefix ` `, `S_`, `S `,
#' `S` or `CP3` then SampleType equals to `STANDARD CURVE`
#' If `sample_name` or `sample_name_from_layout` equals to `NEGATIVE CONTROL`, `N`,
#' or contains substring `NEG`, then SampleType equals to `NEGATIVE CONTROL`
#' If `sample_name` or `sample_name_from_layout` starts with `P` following by
#' whitespace, `POS` following by whitespace, `B770` or `10/190`
#' contains substring `1/\d+` SampleType equals to `POSITIVE CONTROL`
#' otherwise, the returned SampleType is `TEST`
#'
#'
#' @param sample_name (`character`)\cr
#' Vector of sample names from Luminex file
#'
#' @param sample_name_from_layout (`character`)\cr
#' Vector of sample names from Layout file
#' values in this vector may be different than `sample_name` and may
#' contain additional information about the sample type like dilution
#'
#'
#' @return a vector of valid sample_types strings
#'
#'
#' @export
derive_sample_type <- function(sample_name,
                               sample_name_from_layout = "") {
  # handle case when sample name from layout is not provided
  if (is.null(sample_name_from_layout) || is.na(sample_name_from_layout)) {
    sample_name_from_layout <- ""
  }

  # default sample type is test
  sample_type <- "TEST"

  # check if the sample is a blank
  blank_types <- c("BLANK", "BACKGROUND", "B")
  if (sample_name %in% blank_types ||
    sample_name_from_layout %in% blank_types) {
    sample_type <- "BLANK"
  }


  # check if the sample is a positive control
  positive_control_pattern <- c("^(P.|POS.+|B770.+|10/198.+)(1/\\d+)$")
  if (grepl(positive_control_pattern, sample_name) ||
    grepl(positive_control_pattern, sample_name_from_layout)) {
    sample_type <- "POSITIVE CONTROL"
  }


  # check if the sample is a negative control
  negative_types <- c("NEGATIVE CONTROL", "N")
  negative_pattern <-
    "^(N..|.*\\bNEG\\b)" # check if it starts with N or contains NEG string
  if (sample_name %in% negative_types ||
    grepl(negative_pattern, sample_name) ||
    grepl(negative_pattern, sample_name_from_layout)) {
    sample_type <- "NEGATIVE CONTROL"
  }


  # check if the sample is a standard curve
  standard_curve_types <- c("STANDARD CURVE", "SC", "S") # CP3 - tanzania, PRISM - uganda, Brefet - gambia, NIBSC 10/198
  standard_curve_pattern <- "^(S_|S|S\\s|CP.+)(1/\\d+)$"
  standard_curve_loc_pattern <- "(1/\\d+)"
  if (sample_name %in% standard_curve_types ||
    grepl(standard_curve_pattern, sample_name) ||
    grepl(standard_curve_loc_pattern, sample_name_from_layout)) {
    sample_type <- "STANDARD CURVE"
  }

  return(sample_type)
}

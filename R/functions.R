#' @description
#' Parses the sample type based on the sample name and dilution factor
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
#' @param sample_name (`character(1)`)\cr
#' The name of the sample
#'
#' @param sample_name_from_layout (`character(1)`)\cr
#' This param represent the name of the sample provided in the layout file.
#' This filed may be different than `sample_name` and may contain additional
#' information about the sample type
#'
#' @param dilution_factor (`numeric(1)`)\cr
#' The dilution factor of the sample from the base luminex file.
#' This parameter is ignored for now
#'
#' @return a new `SampleType` object
#'
#'
#' @export
derive_sample_type_and_dilution <- function(sample_name,
                                         sample_name_from_layout = "",
                                         dilution_factor = 1) {
  if (is.null(sample_name_from_layout) || is.na(sample_name_from_layout)) {
    sample_name_from_layout <- ""
  }

  blank_types <- c("BLANK", "BACKGROUND", "B")

  if (sample_name %in% blank_types ||
    sample_name_from_layout %in% blank_types) {
    return(SampleType$new("BLANK"))
  }

  sample_type <- "TEST"

  positive_control_pattern <- c("^(P.|POS.+|B770.+|10/198.+)(1/\\d+)$")
  if (grepl(positive_control_pattern, sample_name) ||
    grepl(positive_control_pattern, sample_name_from_layout)) {
    sample_type <- "POSITIVE CONTROL"
  }

  negative_types <- c("NEGATIVE CONTROL", "N")
  negative_pattern <-
    "^(N..|.*\\bNEG\\b)" # check if it starts with N or contains NEG string

  if (sample_name %in% negative_types ||
    grepl(negative_pattern, sample_name) ||
    grepl(negative_pattern, sample_name_from_layout)) {
    sample_type <- "NEGATIVE CONTROL"
  }


  standard_curve_types <- c("STANDARD CURVE", "SC", "S") # CP3 - tanzania, PRISM - uganda, Brefet - gambia, NIBSC 10/198
  standard_curve_pattern <- "^(S_|S|S\\s|CP.+)(1/\\d+)$"
  standard_curve_loc_pattern <- "(1/\\d+)"
  if (sample_name %in% standard_curve_types ||
    grepl(standard_curve_pattern, sample_name) ||
    grepl(standard_curve_loc_pattern, sample_name_from_layout)) {
    sample_type <- "STANDARD CURVE"
  }

  if (sample_type %in% c("STANDARD CURVE", "POSITIVE CONTROL", "NEGATIVE CONTROL")) {
    dilution_factor_pattern <- "1/\\d+"
    match <- ""
    if (!is.null(sample_name_from_layout) && sample_name_from_layout != "" ||
      !is.na(sample_name_from_layout) && sample_name_from_layout != "") {
      match <- regmatches(sample_name_from_layout, regexpr(
        dilution_factor_pattern,
        sample_name_from_layout
      ))
    } else {
      match <- regmatches(sample_name, regexpr(
        dilution_factor_pattern,
        sample_name
      ))
    }
    dilution_factor <- eval(parse(text = match))

    if (is.null(dilution_factor)) {
      dilution_factor <- NA # this value needs to be updated later
    }

    return(SampleType$new("STANDARD CURVE",
      dilution_factor = dilution_factor,
      validate_dilution = FALSE
    ))
  }

  negative_types <- c("NEGATIVE CONTROL", "N")
  negative_pattern <-
    "^(N..|.*\\bNEG\\b)" # check if it starts with N or contains NEG string

  if (sample_name %in% negative_types ||
    grepl(negative_pattern, sample_name) ||
    grepl(negative_pattern, sample_name_from_layout)) {
    return(SampleType$new("NEGATIVE CONTROL"))
  }



  positive_control_pattern <- c("^(P.+|POS.+|CP.+)(1/\\d+)$")
  if (grepl(positive_control_pattern, sample_name) ||
    grepl(positive_control_pattern, sample_name_from_layout)) {
    dilution_factor_pattern <- "1/\\d+"
    match <- ""
    if (!is.null(sample_name_from_layout) && sample_name_from_layout != "" ||
      !is.na(sample_name_from_layout) && sample_name_from_layout != "") {
      match <- regmatches(sample_name_from_layout, regexpr(
        dilution_factor_pattern,
        sample_name_from_layout
      ))
    } else {
      match <- regmatches(sample_name, regexpr(
        dilution_factor_pattern,
        sample_name
      ))
    }
    dilution_factor <- eval(parse(text = match))

    if (is.null(dilution_factor)) {
      dilution_factor <- NA # this value needs to be updated later
    }

    return(SampleType$new(sample_type,
      dilution_factor = dilution_factor,
      validate_dilution = FALSE
    ))
  }

  return(SampleType$new("TEST"))
}

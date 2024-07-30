#' @title SampleType Object
#' @description
#' SampleType defines the type of the sample and the dilution factor
#'
#' @field sample_type
#' A type of the sample.
#' The possible sample types are:
#' 1. (`"BLANK"`)\cr - background sample used for testing
#' 2. (`"STANDARD CURVE"`)\cr - a sample used to create a standard curve
#' 3. (`"POSITIVE CONTROL"`)\cr - a positive control sample used for testing
#' 4. (`"NEGATIVE CONTROL"`)\cr - a negative control sample used for testing
#' 5. (`"TEST"`)\cr - the actual sample used for testing
#'
#' @field dilution_factor A numeric value that represents
#' the dilution factor of the sample. Used only in the case of control samples
#'
#' @examples
#' sample_type <- SampleType$new("POSITIVE CONTROL", 1)
#'
#' @export
SampleType <- R6::R6Class(
  "SampleType",
  public = list(
    sample_type = NULL,
    dilution_factor = NA,

    #' @description
    #' Creates a new instance of the SampleType class
    #'
    #' @param sample_type (`character(1)`)\cr
    #' A character value that represents the type of the sample
    #' Possible values are: `"BLANK"`, `"POSITIVE CONTROL"`, `"STANDARD CURVE"`
    #' `"NEGATIVE CONTROL"`, `"TEST"`.
    #' @param dilution_factor (`numeric(1)`)\cr
    #' A numeric value that represents the dilution factor of the sample.
    #' Used only in the case of control samples
    #' @param validate_dilution Whether to validate the object for the dilution factor.
    #' Used in the reading scripts to dynamically assign values. Default to `TRUE`
    initialize = function(sample_type, dilution_factor = NA, validate_dilution = TRUE) {
      # for now there should passed a sample type only
      # check for valid input
      stopifnot(length(sample_type) == 1 &&
        is.character(sample_type))
      stopifnot(length(dilution_factor) == 1 &&
        (is.na(dilution_factor) ||
          is.numeric(dilution_factor)))

      SampleType$validate_sample_type(sample_type)

      # allow for lazy loading - don't verify the dilution factor after creation, but wait until checks
      if (validate_dilution) {
        SampleType$validate_dilution_factor(sample_type, dilution_factor)
        private$is_validated <- TRUE
      } else {
        private$is_validated <- FALSE
      }

      self$sample_type <- sample_type
      self$dilution_factor <- dilution_factor
    },

    #' @description
    #' Prints the sample type and the dilution factor
    print = function(...) {
      cat("Sample Type: ", self$sample_type, "\n")
      if (!is.na(self$dilution_factor)) {
        cat("Dilution Factor: ", self$character_dilution_factor, "\n")
      }
      invisible(self)
    },

    #' @description
    #' Joins the data of two sample types
    #' Performs checks to ensure that the sample types can be joined
    #' Essentially checks if the two sample types are identical or
    #' at least one of them is empty
    #'
    #' @param new_sample_type (`SampleType`)\cr
    #'
    #' @return returns the updated sample type object
    join = function(new_sample_type) {
      # join the data of two samples

      if (!verify_character_join(
        self$sample_type,
        new_sample_type$sample_type
      )) {
        stop("Cannot join samples of different types")
      }
      if (!verify_numeric_join(
        self$dilution_factor,
        new_sample_type$dilution_factor
      )) {
        stop("Cannot join samples with different dilution factors")
      }

      self$dilution_factor <-
        get_join_value(self$dilution_factor, new_sample_type$dilution_factor)
      self$sample_type <-
        get_join_value(self$sample_type, new_sample_type$sample_type)

      invisible(self)
    }
  ),
  private = list(
    is_validated = FALSE
  ),
  active = list(
    #' @field character_dilution_factor the dilution factor in the format `1/x`
    #' - useful for printing
    character_dilution_factor = function() {
      if (is.na(self$dilution_factor)) {
        return("NA")
      } else {
        return(paste0("1/", as.character(1 / self$dilution_factor)))
      }
    }
  )
)

#'
#' @description
#' Possible types for the samples
#'
#' @export
SampleType$valid_sample_types <-
  c(
    "BLANK",
    "TEST",
    "NEGATIVE CONTROL",
    "STANDARD CURVE",
    "POSITIVE CONTROL"
  )
#'
#' @description
#' Validates the sample type using five possible values: `"BLANK"`,
#' `"POSITIVE CONTROL"`,`"STANDARD CURVE"`, `"NEGATIVE CONTROL"`, `"TEST"`.
#'
#' @param sample_type (`character(1)`)\cr
#'
#' @return Returns `TRUE` if the sample type is valid, otherwise throws an error
#'
#' @examples
#' SampleType$validate_sample_type("BLANK")
#' SampleType$validate_sample_type("POSITIVE CONTROL")
#'
#' @export
SampleType$validate_sample_type <- function(sample_type) {
  if (!(sample_type %in% SampleType$valid_sample_types)) {
    stop("Invalid sample type")
  }
  return(TRUE)
}

#' @description
#' Validates the dilution factor based on the sample type. Only positive control
#' and standard curve samples should have a dilution factor
#'
#' @param sample_type (`character(1)`)\cr
#' The type of the sample
#'
#' @param dilution_factor (`numeric(1)`)\cr
#' dilution factor to be verified
#'
#' @returns returns `TRUE` if the dilution factor is valid, otherwise throws
#' an error
#'
#' @examples
#' SampleType$validate_dilution_factor("POSITIVE CONTROL", 1)
#'
#' @export
SampleType$validate_dilution_factor <- function(sample_type, dilution_factor) {
  if (sample_type == "POSITIVE CONTROL" && is.na(dilution_factor)) {
    stop("Positive control samples must have a dilution factor")
  }
  if (sample_type == "STANDARD CURVE" && is.na(dilution_factor)) {
    stop("Standard curve samples must have a dilution factor")
  }
  if (!sample_type %in% c("POSITIVE CONTROL", "STANDARD CURVE") &&
    !is.na(dilution_factor)) {
    stop("Only positive control or standard curve samples should have
         a dilution factor")
  }
  return(TRUE)
}

#' @description
#' Parses the sample type based on the sample name and dilution factor
#'
#' It parses the names as follows:
#' If `sample_name` or `sample_name_loc` equals to `BLANK`, `BACKGROUND` or `B`,
#'  then SampleType equals to `BLANK`
#' If `sample_name` or `sample_name_loc` equals to `STANDARD CURVE`, `SC`, `S`
#' or contains substring `1/\d+` and has prefix ` `, `S_`, `S `, `S` or `CP3`
#' then SampleType equals to `STANDARD CURVE`
#' If `sample_name` or `sample_name_loc` equals to `NEGATIVE CONTROL`, `N`,
#' or contains substring `NEG`, then SampleType equals to `NEGATIVE CONTROL`
#' If `sample_name` or `sample_name_loc` starts with `P` following by
#' whitespace, `POS` following by whitespace, `B770` or `10/190`
#' contains substring `1/\d+` SampleType equals to `POSITIVE CONTROL`
#' otherwise, the returned SampleType is `TEST`
#'
#'
#'
#'
#' @param sample_name (`character(1)`)\cr
#' The name of the sample
#'
#' @param sample_name_loc (`character(1)`)\cr
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
SampleType$parse_sample_type <- function(sample_name,
                                         sample_name_loc = "",
                                         dilution_factor = 1) {
  if (is.null(sample_name_loc) || is.na(sample_name_loc)) {
    sample_name_loc <- ""
  }

  blank_types <- c("BLANK", "BACKGROUND", "B")

  if (sample_name %in% blank_types ||
    sample_name_loc %in% blank_types) {
    return(SampleType$new("BLANK"))
  }

  sample_type <- "TEST"

  positive_control_pattern <- c("^(P.|POS.+|B770.+|10/198.+)(1/\\d+)$")
  if (grepl(positive_control_pattern, sample_name) ||
    grepl(positive_control_pattern, sample_name_loc)) {
    sample_type <- "POSITIVE CONTROL"
  }

  negative_types <- c("NEGATIVE CONTROL", "N")
  negative_pattern <-
    "^(N..|.*\\bNEG\\b)" # check if it starts with N or contains NEG string

  if (sample_name %in% negative_types ||
    grepl(negative_pattern, sample_name) ||
    grepl(negative_pattern, sample_name_loc)) {
    sample_type <- "NEGATIVE CONTROL"
  }


  standard_curve_types <- c("STANDARD CURVE", "SC", "S") # CP3 - tanzania, PRISM - uganda, Brefet - gambia, NIBSC 10/198
  standard_curve_pattern <- "^(S_|S|S\\s|CP.+)(1/\\d+)$"
  standard_curve_loc_pattern <- "(1/\\d+)"
  if (sample_name %in% standard_curve_types ||
    grepl(standard_curve_pattern, sample_name) ||
    grepl(standard_curve_loc_pattern, sample_name_loc)) {
    sample_type <- "STANDARD CURVE"
  }

  if (sample_type %in% c("STANDARD CURVE", "POSITIVE CONTROL", "NEGATIVE CONTROL")) {
    dilution_factor_pattern <- "1/\\d+"
    match <- ""
    if (!is.null(sample_name_loc) && sample_name_loc != "" ||
      !is.na(sample_name_loc) && sample_name_loc != "") {
      match <- regmatches(sample_name_loc, regexpr(
        dilution_factor_pattern,
        sample_name_loc
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
    grepl(negative_pattern, sample_name_loc)) {
    return(SampleType$new("NEGATIVE CONTROL"))
  }



  positive_control_pattern <- c("^(P.+|POS.+|CP.+)(1/\\d+)$")
  if (grepl(positive_control_pattern, sample_name) ||
    grepl(positive_control_pattern, sample_name_loc)) {
    dilution_factor_pattern <- "1/\\d+"
    match <- ""
    if (!is.null(sample_name_loc) && sample_name_loc != "" ||
      !is.na(sample_name_loc) && sample_name_loc != "") {
      match <- regmatches(sample_name_loc, regexpr(
        dilution_factor_pattern,
        sample_name_loc
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

library(R6)


verify_numeric_join <- function(x, y) {
  # check if two numeric values are equal
  if (is.na(x) || is.na(y)) {
    return(TRUE)
  }
  return(x == y)
}

verify_character_join <- function(x, y) {
  # check if two character values are equal
  if (is.null(x) || is.null(y)) {
    return(TRUE)
  }
  return(x == y)
}

get_join_value <- function(x, y) {
  if (is.na(x) || is.null(x)) {
    return(y)
  }
  if (is.na(y) || is.null(y)) {
    return(x)
  }

  if (x == y) {
    return(x)
  }
}



#' @title Analyte Object
#'
#' @description
#' Analyte class is an object that gathers information about one specific Analyte on the Luminex plate
#'
#' @field id A numeric value that represents the unique identifier of the analyte
#'
#' @field analyte_name A character value that represents the name of the analyte
#'
#' @field bead_count ? not exactly sure what this field means tbh
#'
#' @field analysis_type Type of the analyte
#'
#' @field units Units of the analyte in which the results are expressed
#'
#' @examples
#' etramp <- Analyte$new(id = 73, analyte_name = "Etramp5_ag1", bead_count = 50)
#' print(etramp)
#'
#' @export
Analyte <- R6Class(
  "Analyte",
  list(
    id = NA,
    analyte_name = NULL,
    bead_count = NA,
    analysis_type = NULL,
    units = NULL,

    #' @description
    #' Creates a new instance of the `Analyte` class
    #'
    #' @param id (`numeric(1)`)\cr
    #' A numeric value that represents the unique identifier of the analyte
    #'
    #' @param analyte_name (`character(1)`)\cr
    #' A character value that represents the name of the analyte
    #'
    #' @param bead_count (`numeric(1)`)\cr
    #'
    #' @param analysis_type (`character(1)`)\cr
    #' Type of the analyte
    #'
    #' @param units (`character(1)`)\cr
    #' Units of the analyte in which the results are expressed
    #'
    #'
    initialize = function(id,
                          analyte_name,
                          bead_count = NA,
                          analysis_type = NULL,
                          units = NULL) {
      # check for valid input
      stopifnot(length(id) == 1 && is.numeric(id))

      stopifnot(length(analyte_name) == 1 &&
        is.character(analyte_name))
      stopifnot(length(bead_count) == 1 &&
        (is.na(bead_count) || is.numeric(bead_count)))
      stopifnot(length(analysis_type) == 0 ||
        (is.character(analysis_type) &&
          length(analysis_type) == 1))
      stopifnot(length(units) == 0 ||
        (is.character(units) && length(units) == 1))

      self$id <- id
      self$analyte_name <- analyte_name
      self$bead_count <- bead_count
      self$analysis_type <- analysis_type
      self$units <- units
    },

    #' @description
    #' Prints the information about the analyte
    print = function(...) {
      cat("Analyte: ", self$analyte_name, "\n")
      cat("ID: ", self$id, "\n")
      cat("Bead Count: ", self$bead_count, "\n")
      cat("Analysis Type: ", self$analysis_type, "\n")
      invisible(self)
    },

    #' @description
    #' Joins the data of two analytes
    #' Performs checks to ensure that the analytes can be joined
    #' If the analytes can be joined, the bead_count, analysis_type and units are updated in the current analyte object
    #'
    #' @param new_analyte (`Analyte`)\cr
    #' The analyte object to be joined with the current analyte object - this is rather a technical inner functionand should rarely be used by the user
    join = function(new_analyte) {
      # join the data of two samples

      if (!verify_numeric_join(self$id, new_analyte$id)) {
        stop("Cannot join analytes of different IDs")
      }
      self$id <- get_join_value(self$id, new_analyte$id)

      if (!verify_numeric_join(self$bead_count, new_analyte$bead_count)) {
        stop("Cannot join analytes with different bead counts")
      }
      if (!verify_character_join(self$analysis_type, new_analyte$analysis_type)) {
        stop("Cannot join analytes with different analysis types")
      }
      if (!verify_character_join(self$units, new_analyte$units)) {
        stop("Cannot join analytes with different units")
      }

      self$bead_count <-
        get_join_value(self$bead_count, new_analyte$bead_count)
      self$analysis_type <-
        get_join_value(self$analysis_type, new_analyte$analysis_type)
      self$units <- get_join_value(self$units, new_analyte$units)
    }
  )
)

#' @title SampleLocation Object
#' @description
#' SampleLocation defines a location of a sample on the Luminex plate
#' The location is defined by the row and column number and explicitly states the location of the sample
#'
#' @field col column number of sample on the Luminex plate
#'
#' @field row row number of sample on the Luminex plate
#'
#' @field row_letter row where sample is located as a letter. For instance `1` -> `A`
#'
#' @field location_name Lcation of the sample formatted in readable way - `(row_letter, column)`
#'
#' @examples
#'
#' sample_location <- SampleLocation$new(col = 1, row = 4)
#'
#' sample_location$location_name
#'
#' sample_location <- SampleLocation$parse_sample_location("65(1,F5)")
#' sample_location$location_name
#'
#' @export
SampleLocation <- R6Class(
  "SampleLocation",
  public = list(
    col = NA,
    row = NA,

    #' @description
    #' Creates a new instance of the SampleLocation class
    #' @param row (`numeric(1)`)\cr
    #' A numeric value that represents the row number of the sample location
    #' @param col (`numeric(1)`)\cr
    #' A numeric value that represents the column number of the sample location
    initialize = function(row = NA, col = NA) {
      # check for valid input
      stopifnot(is.na(col) || (length(col) == 1 && is.numeric(col)))
      stopifnot(is.na(row) || (length(row) == 1 && is.numeric(row)))

      self$col <- col
      self$row <- row
    },

    #' @description
    #' Prints the information about the sample location
    print = function(...) {
      cat("Sample Location: ", self$location_name, "\n")
      invisible(self)
    },

    #' @description
    #' Joins the data of two sample locations
    #' Performs checks to ensure that the sample locations can be joined
    #' Essentially checks if the two sample locations are identical or at least one of them is empty
    join = function(new_location) {
      # join the data of two samples

      if (!verify_numeric_join(self$row, new_location$row) ||
        !verify_numeric_join(self$col, new_location$col)) {
        stop("Cannot join samples of different locations")
      }

      self$col <- get_join_value(self$col, new_location$col)
      self$row <- get_join_value(self$row, new_location$row)
    }
  ),
  active = list(
    #' @description
    #' Returns the letter corresponding to the row number - useful for printing the location
    row_letter = function() {
      return(LETTERS[self$row])
    },

    #' @description
    #' Returns the location of the sample in the format `(row_letter, column)`
    location_name = function() {
      return(paste0("(", self$row_letter, ", ", self$col, ")"))
    }
  )
)


#' @description
#' Function parses location string and returns an object of class SampleLocation
#'
#' @param location_string variable that represents the sample location on the plate formatted as in the Luminex file
#' It should be formatted as: `sample_id(plate_id, location_name)`
#'
#' @returns New SampleLocation object with parsed location information.
#'
SampleLocation$parse_sample_location <- function(location_string) {
  cleaned_string <- gsub("\\\"", "", location_string)
  cleaned_string <- unlist(strsplit(cleaned_string, "[()]"))

  if (length(cleaned_string) > 1) {
    id <- as.numeric(cleaned_string[1])
    cleaned_string <- unlist(strsplit(cleaned_string[2], "[;,]"))
    plate_id <- as.numeric(cleaned_string[1])
    location <- cleaned_string[2]
  } else {
    cleaned_string <- unlist(strsplit(cleaned_string[1], "[;,]"))
    plate_id <- as.numeric(cleaned_string[1])
    location <- cleaned_string[2]
  }

  matches <-
    unlist(regmatches(location, regexec("([A-Z]+)(\\d+)", location)))

  row_letter <- matches[2]
  row_number <-
    as.numeric(match(toupper(row_letter), LETTERS))
  col <- as.numeric(matches[3])

  # could also utilize the plate id along with location id

  location <- SampleLocation$new(row_number, col)
}

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
#' @field dilution_factor A numeric value that represents the dilution factor of the sample. Used only in the case of control samples
#'
#'
#' @export
SampleType <- R6Class(
  "SampleType",
  public = list(
    sample_type = NULL,
    dilution_factor = NA,

    #' @description
    #' Creates a new instance of the SampleType class
    #'
    #' @param sample_type (`character(1)`)\cr
    #' A character value that represents the type of the sample
    #' Possible values are: `"BLANK"`, `"POSITIVE CONTROL"`, `"NEGATIVE CONTROL"`, `"TEST"`.
    #' @param dilution_factor (`numeric(1)`)\cr
    #' A numeric value that represents the dilution factor of the sample. Used only in the case of control samples
    #' @param Whether to validate the object for the dilution factor. Used in the reading scripts to dynamically assign values. Default to `TRUE`
    initialize = function(sample_type, dilution_factor = NA, validate_dilution = TRUE) {
      # for now there should passed a sample type only
      # check for valid input
      stopifnot(length(sample_type) == 1 &&
        is.character(sample_type))
      stopifnot(length(dilution_factor) == 1 &&
        (is.na(dilution_factor) ||
          is.numeric(dilution_factor)))

      SampleType$validate_sample_type(sample_type)

      # allow for lazy loading - dont verify the dilution factor after creation, but wait until checks
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
    #' Essentially checks if the two sample types are identical or at least one of them is empty
    join = function(new_sample_type) {
      # join the data of two samples

      if (!verify_character_join(self$sample_type, new_sample_type$sample_type)) {
        stop("Cannot join samples of different types")
      }
      if (!verify_numeric_join(self$dilution_factor, new_sample_type$dilution_factor)) {
        stop("Cannot join samples with different dilution factors")
      }

      self$dilution_factor <-
        get_join_value(self$dilution_factor, new_sample_type$dilution_factor)
      self$sample_type <-
        get_join_value(self$sample_type, new_sample_type$sample_type)
    }
  ),
  private = list(
    is_validated = FALSE
  ),
  active = list(
    #' @field character_dilution_factor the dilution factor in the format `1/x` - useful for printing
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
#' Validates the sample type using five possible values: `"BLANK"`, `"POSITIVE CONTROL"`, `"NEGATIVE CONTROL"`, `"TEST"`.
#'
SampleType$validate_sample_type <- function(sample_type) {
  if (!(sample_type %in% SampleType$valid_sample_types)) {
    stop("Invalid sample type")
  }
}

#' @description
#' Validates the dilution factor based on the sample type
SampleType$validate_dilution_factor <- function(sample_type, dilution_factor) {
  if (sample_type == "POSITIVE CONTROL" && is.na(dilution_factor)) {
    stop("Positive control samples must have a dilution factor")
  }
  if (sample_type == "STANDARD CURVE" && is.na(dilution_factor)) {
    stop("Standard curve samples must have a dilution factor")
  }
  if (sample_type %in% c("POSITIVE CONTROL", "STANDARD CURVE") &&
    !is.na(dilution_factor)) {
    stop("Only positive control or standard curve samples should have a dilution factor")
  }
}

#' @description
#' Parses the sample type based on the sample name and dilution factor
#'
#' It parses the names as follows:
#' If `sample_name` or `sample_name_loc` equals to `BLANK`, `BACKGROUND` or `B` , then SampleType equals to `BLANK`
#' If `sample_name` or `sample_name_loc` equals to `STANDARD CURVE`, `SC`, `S` or contains substring `1/\d+` and has prefix ` `, `S_`, `S `, `S` or `CP3` then SampleType equals to `STANDARD CURVE`
#' If `sample_name` or `sample_name_loc` equals to `NEGATIVE CONTROL`, `N`, or contains substring `NEG`, then SampleType equals to `NEGATIVE CONTROL`
#' If `sample_name` or `sample_name_loc` starts with `P` following by whitespace, `POS` following by whitespace, `B770` or `10/190`   contains substring `1/\d+` SampleType equals to `POSITIVE CONTROL`
#' otherwise, the returned SampleType is `TEST`
#'
#'
#'
#'
#' @param sample_name (`character(1)`)\cr
#' The name of the sample
#'
#' @param sample_name_loc (`character(1)`)\cr
#' This param represent the name of the sample provided in the layout file. This filed may be different than `sample_name` and may contain additional information about the sample type
#'
#' @param dilution_factor (`numeric(1)`)\cr
#' The dilution factor of the sample from the base luminex file. This parameter is ignored for now
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
    if (!is.null(sample_name_loc) && sample_name_loc != "" || !is.na(sample_name_loc) && sample_name_loc != "") {
      match <- regmatches(sample_name_loc, regexpr(dilution_factor_pattern, sample_name_loc))
    } else {
      match <- regmatches(sample_name, regexpr(dilution_factor_pattern, sample_name))
    }
    dilution_factor <- eval(parse(text = match))

    if (is.null(dilution_factor)) {
      dilution_factor <- NA # this value needs to be updated later
    }

    return(SampleType$new("STANDARD CURVE", dilution_factor = dilution_factor, validate_dilution = FALSE))
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
    if (!is.null(sample_name_loc) && sample_name_loc != "" || !is.na(sample_name_loc) && sample_name_loc != "") {
      match <- regmatches(sample_name_loc, regexpr(dilution_factor_pattern, sample_name_loc))
    } else {
      match <- regmatches(sample_name, regexpr(dilution_factor_pattern, sample_name))
    }
    dilution_factor <- eval(parse(text = match))

    if (is.null(dilution_factor)) {
      dilution_factor <- NA # this value needs to be updated later
    }

    return(SampleType$new(sample_type, dilution_factor = dilution_factor, validate_dilution = FALSE))
  }

  return(SampleType$new("TEST"))
}


#' @title Sample
#' @description
#' A class to represent the sample. It contains all the necessary information about the sample
#' @examples
#' # TODO
#'
#' @export
Sample <- R6Class(
  "Sample",
  list(
    #' @field id Sample id, when reading from a plate it is a id extracted from the sample position on the plate
    id = NA,
    #' @field sample_name Sample name
    sample_name = NULL,

    #' @field sample_type A sample type - is the sample used for testing, is it blank, positive or negative control
    sample_type = NULL,

    #' @field sample_location Position on the Plate
    sample_location = NULL,

    #' @field warnings list of warnings warnings are read from the file or generated during the reading process
    warnings = list(),

    #' @field errors lit of errors; errors are read from the file or generated during the reading process
    errors = list(),

    #' @field data The dataframe containing informations about the analytes within the sample.
    #' The dataframe rows are different measures, such as `Median`, `Net MFI`, etc. whereas the columns represent different analytes
    #'
    data = data.frame(),


    #' @description
    #' Initializes the sample object
    #'
    #' @param id (`numeric(1)`)\cr
    #' The id of the sample - usually just the sample row number, can be derived from the sample location
    #'
    #' @param sample_name (`character(1)`)\cr
    #' The name of the sample
    #'
    #' @param sample_type (`SampleType`)\cr
    #' The type of the sample
    #'
    #' The location of the sample on the plate
    #'
    #' @param warnings (`list`)\cr
    #' List containing all the warnings connected to the sample - generated by the validity checks or read from file
    #' @param errors (`list`)\cr
    #' List containing all the critical errors connected to the sample - generated by the validity checks or read from file
    #'
    #' @param data (`data.frame`)\cr
    #' The dataframe containing the informations about analytes within the sample
    #'
    initialize = function(id,
                          sample_name,
                          sample_type = NULL,
                          sample_location = NULL,
                          warnings = list(),
                          errors = list(),
                          data = data.frame()) {
      # check for valid input
      stopifnot(length(id) == 1 && is.numeric(id))
      stopifnot(length(sample_name) == 1 &&
        is.character(sample_name))

      stopifnot(is.null(sample_type) ||
        "SampleType" %in% class(sample_type))
      stopifnot(is.null(sample_location) ||
        "SampleLocation" %in% class(sample_location))
      stopifnot(is.data.frame(data))

      self$id <- id
      self$sample_name <- sample_name
      if (is.null(sample_type)) {
        self$sample_type <- SampleType$new()
      } else {
        self$sample_type <- sample_type
      }
      if (is.null(sample_location)) {
        self$sample_location <- SampleLocation$new()
      } else {
        self$sample_location <- sample_location
      }

      # check if data is all numeric
      all_numeric <- all(sapply(data, is.numeric))
      if (!all_numeric) {
        data_num <- as.data.frame(matrix(ncol = ncol(data), nrow = nrow(data)))
        colnames(data_num) <- colnames(data)
        rownames(data_num) <- rownames(data)

        # fix conversion TODO
        for (i in 1:ncol(data)) {
          data_num[, i] <- as.numeric(as.character(data[, i]))
        }

        rownames(data_num) <- rownames(data)
        colnames(data_num) <- colnames(data)
        data <- data_num
      }

      self$warnings <- warnings
      self$errors <- errors

      self$data <- data
    },

    #' @description
    #' Prints the sample information
    print = function(...) {
      cat("A sample", self$sample_name, "with ID: ", self$id, "\n")
      self$sample_type$print()
      self$sample_location$print()

      invisible(self)
    },

    #' @description
    #' Joins the data of two samples
    join = function(new_sample) {
      if (!verify_numeric_join(self$id, new_sample$id)) {
        stop("Cannot join samples of different IDs")
      }
      self$id <- get_join_value(self$id, new_sample$id)

      self$sample_type$join(new_sample$sample_type)
      self$sample_location$join(new_sample$sample_location)


      self$data <- dplyr::bind_rows(self$data, new_sample$data)
    }
  )
)



#' @title Plate
#' @description
#' A class to represent the luminex plate. It contains information about the samples and analytes that were examined on the plate as well as some additional metadata and batch info
#'
#'
#'
#' @examples
#' # TODO
#'
#' @export
Plate <- R6Class(
  "Plate",
  public = list(

    #' @field analytes list of all analytes measured during an examination
    analytes = list(),

    #' @field samples list of all samples examined on a plate
    samples = list(),

    #' @field batch_info a raw list containing all the metadata about the plate read from the Luminex file
    batch_info = list(),

    #' @field calibration_info a list containing calibration logs read from Luminex file
    calibration_info = list(),

    #' @field audit_logs a list containing audit logs read from Luminex file
    audit_logs = list(),

    #' @field plate_name - plate name obtained from filename
    plate_name = "",


    #' @description
    #' creates a new `Plate` object
    #'
    #' @param analytes (`list`)
    #' list of all analytes measured during an examination

    #' @param samples (`list`)
    #' list of all samples examined on a plate

    #' @param batch_info (`list`) \cr
    #' a raw list containing all the metadata about the plate read from the Luminex file

    #' @param calibration_info (`list`) \cr
    #' a list containing calibration logs read from Luminex file

    #' @param audit_logs (`list`)
    #' a list containing audit logs read from Luminex file
    initialize = function(analytes = list(),
                          samples = list(),
                          batch_info = list(),
                          calibration_info = list(),
                          audit_logs = list(),
                          plate_name = "") {
      # check for valid input
      self$analytes <- analytes
      self$samples <- samples
      self$batch_info <- batch_info
      self$calibration_info <- calibration_info
      self$audit_logs <- audit_logs
      self$plate_name <- plate_name
    },
    print = function(...) {
      cat(
        "Plate with",
        length(self$samples),
        "samples and",
        length(self$analytes),
        "analytes\n"
      )
      invisible(self)
    },


    #' @description
    #' Function outputs basic information about the plate, such as examination date,
    #' batch name, and sample types
    #'
    #' @param include_names If `include_names` parameter is `TRUE`, apart from count of control samples, provides also their names.By default `FALSE`
    summary = function(..., include_names = FALSE) {
      positive_control_samples_list <- self$get_sample_by_type("POSITIVE CONTROL")
      negative_control_samples_list <- self$get_sample_by_type("NEGATIVE CONTROL")
      standard_curve_samples_list <- self$get_sample_by_type("STANDARD CURVE")

      blank_samples_num <-
        length(self$get_sample_by_type("BLANK"))
      positive_control_num <- length(positive_control_samples_list)
      negative_control_num <- length(negative_control_samples_list)
      standard_curve_num <- length(standard_curve_samples_list)

      positive_control_names <- ""
      negative_control_names <- ""
      standard_curve_names <- ""

      if (include_names) {
        if (positive_control_num > 0) {
          positive_control_names <- paste(sapply(positive_control_samples_list, function(sample) paste0("'", sample$sample_name, "'")), collapse = ", ")
          positive_control_names <- paste0("\nSample names: ", positive_control_names)
        }

        if (negative_control_num > 0) {
          negative_control_names <- paste(sapply(negative_control_samples_list, function(sample) paste0("'", sample$sample_name, "'")), collapse = ", ")
          negative_control_names <- paste0("\nSample names: ", negative_control_names)
        }
        if (standard_curve_num > 0) {
          standard_curve_names <- paste(sapply(standard_curve_samples_list, function(sample) paste0("'", sample$sample_name, "'")), collapse = ", ")
          standard_curve_names <- paste0("\nSample names: ", standard_curve_names)
        }
      }

      cat(
        "Summary of the plate generated on ", as.character(self$examination_date),
        "\nwith name '", plate$plate_name, "':\n",
        "Total number of samples: ",
        self$number_of_samples,
        "\n",
        "Number of blank samples: ",
        blank_samples_num,
        "\n",
        "Number of standard curve samples: ",
        standard_curve_num,
        standard_curve_names, "\n",
        "Number of positive control samples: ",
        positive_control_num,
        positive_control_names, "\n",
        "Number of negative control samples: ",
        negative_control_num,
        negative_control_names, "\n",
        sep = ""
      )

      invisible(self)
    },

    #' @description
    #' function adds block of information to the current plate
    #'
    #'
    #' the analysis type should be added after the analysis, otherwise there is no enough information about samples
    add_results_block = function(data_type, parsed_block) {
      # verify the data type
      if (data_type == "CRC") {
        return(0)
      }
      if (data_type == "Audit Logs") {
        self$audit_logs <- parsed_block
        return(0)
      }
      if (data_type == "Warnings/Errors") {
        # warnings should be at the end of the file, thus we assume that there exists samples in given locations
        for (warning in parsed_block) {
          location <- SampleLocation$parse_sample_location(warning$Location)
          sample <- self$get_sample(location)
          if (warning$Status == "Warning") {
            sample$warnings <- c(sample$warnings, warning$Message)
          } else if (warning$Status == "Error") {
            sample$errors <- c(sample$errors, warning$Message)
          }
        }
      }
      if (data_type == "samples") {
        if (length(self$samples) == 0) {
          self$samples <- parsed_block
        } else {
          # join the lists of analytes
          new_samples <- parsed_block
          for (i in seq_along(new_samples)) {
            new_sample <- new_samples[[i]]
            if (new_sample$id %in% sapply(self$samples, function(x) {
              x$id
            })) {
              # add new data to the existing sample
              self$samples[[new_sample$id]]$join(new_sample)
            } else {
              self$samples[[new_sample$id]] <- new_sample
            }
          }
        }
        return(0)
      }
      if (data_type == "analytes") {
        if (length(self$analytes) == 0) {
          self$analytes <- parsed_block
        }
        # join the lists of analytes
        new_analytes <- parsed_block
        for (i in seq_along(new_analytes)) {
          new_analyte <- new_analytes[[i]]
          if (new_analyte$id %in% sapply(self$analytes, function(x) {
            x$id
          })) {
            # add new data to the existing sample
            self$analytes[[as.character(new_analyte$id)]]$join(new_analyte)
          } else {
            self$analytes[[as.character(new_analyte$id)]] <- new_analyte
          }
        }
        return(0)
      }
      if (data_type == "analyte_types") {
        parsed_block <- Filter(Negate(is.null), parsed_block)

        if (length(self$analytes) == 0 || length(parsed_block)) {
          return(0)
        } # nothing to add

        for (analyte_name in names(parsed_block)) {
          analyte_type <- parsed_block[[analyte_name]]

          analyte_id <- self$get_analyte_id(analyte_name)
          if (length(analyte_id) > 0) {
            self$analytes[[analyte_id]]$analysis_type <- analyte_type
          }
        }
        return(0)
      }
    },

    #' @description
    #' Function verifies if the batch info contain valid information
    check_batch_info = function() {
      # we will require only some of the fields

      if (is.null(self$examination_date)) {
        stop("No examination date provided or in bad format")
      }

      if (is.null(self$batch_name)) {
        stop("No batch name provided or is in bad format")
      }


      return(TRUE)
    },

    #' @description
    #' checks analyte consistency - verifies if all of the analytes contained within the samples are listed in the `analytes` list of the plate object
    check_analyte_consistency = function() {
      additional_column_names <- c("Total Events")

      is_consistent <- TRUE

      analytes_in_plate <- self$analyte_names

      analytes_in_plate <- c(analytes_in_plate, additional_column_names)

      # check if all analytes in self$samples are saved in self$analytes
      for (sample in self$samples) {
        analytes_in_sample <- colnames(sample$data)
        diff_elements <-
          setdiff(analytes_in_sample, analytes_in_plate)


        if (length(diff_elements) > 0) {
          # Print differing elements
          warning_message <- paste0(
            "Analytes in the sample ",
            sample$sample_name,
            " that are not in plate: ",
            diff_elements
          )
          sample$warnings <- append(sample$warnings, warning_message)

          # Raise an error
          is_consistent <- FALSE
          # stop("Error: There are analytes in the sample not defined in the plate.")
        }
      }

      return(is_consistent)
    },

    #' @description
    #' function performs validity check - verifies which samples and analytes saved in the plate have the bead count lower than `min_events_per_bead`
    #'
    #' @param min_events_per_bead lower bound of acceptable number of events. By default equals to `min_events_per_bead` parameter saved in the plate object
    #'
    check_beads_number = function(min_events_per_bead = self$min_events_per_bead) {
      below_min_list <- list()

      below_min_flag <- FALSE

      for (sample in self$samples) {
        if ("Count" %in% row.names(sample$data)) { # TODO there should be option for lowercase
          below_min <- which(sample$data["Count", ] < min_events_per_bead, arr.ind = TRUE)
          if (length(below_min) > 0) {
            below_min_analytes <- names(sample$data["Count", ])[below_min[, "col"]]
            new_warnings <- paste0("An analyte ", below_min_analytes, " did not reach the specified count in the given sample")
            sample$warnings <- c(sample$warnings, new_warnings)
          }
        }
      }

      return(below_min_flag)
    },

    #'
    #' @param sample sample name or its id
    #' @returns sample object of given sample name or id
    get_sample = function(sample) {
      # get the sample by its name, id or location
      if ("SampleLocation" %in% class(sample)) {
        sample <-
          which(sapply(self$samples, function(x) {
            x$sample_location$location_name
          }) == sample$location_name)
      } else if (is.numeric(sample)) {
        if (sample < 0 || sample > plate$number_of_samples) {
          stop("Sample ID out of range")
        }
      } else {
        sample <-
          which(sapply(self$samples, function(x) {
            x$sample_name
          }) == sample)

        sample_by_loc <-
          which(sapply(self$samples, function(x) {
            x$sample_location$location_name
          }) == sample)

        sample <- c(sample, sample_by_loc)

        if (length(sample) == 0) {
          stop("Sample of given name nor location not found")
        }
      }
      return(self$samples[[sample]])
    },

    #' @description
    #' Function returns list of samples filtered by the type
    #'
    #' @param sample_type type of the sample to be filtered. Possible values are:
    #' SampleType$valid_sample_types
    #'
    #'
    #' @param exclude If `FALSE` returns list of samples with given `sample_type`,
    #' otherwise returns all samples except for the specified `sample_type`
    #'
    get_sample_by_type = function(sample_type, exclude = FALSE) {
      stopifnot(sample_type %in% SampleType$valid_sample_types)

      samples_by_type <- list()
      for (sample in self$samples) {
        if (sample$sample_type$sample_type == sample_type && !exclude) {
          samples_by_type <- append(samples_by_type, sample)
        } else if (sample$sample_type$sample_type != sample_type && exclude) {
          samples_by_type <- append(samples_by_type, sample)
        }
      }

      return(samples_by_type)
    },

    #'
    #' @returns  analyte id of given name
    get_analyte_id = function(analyte_name) {
      analyte_id <-
        which(sapply(self$analytes, function(x) {
          x$analyte_name
        }) == analyte_name)
    },

    #' @description
    #' Function returns data for a specific analyte and sample.
    #'
    #' @param analyte An analyte name or its id of which data we want to extract
    #'
    #' @param sample sample name or id
    #' @param data_type if `NULL` returns whole column of the dataframe containing information about the sample.
    #' Otherwise tries to select the exact `data_type` from the dataframe and returns a single value
    #'
    #' @return Data about a sample and analyte
    get = function(analyte, sample, data_type = NULL) {
      # get the data for a specific analyte and sample

      if (length(sample) != 1) {
        stop("Only one sample can be passed, for now.")
      }

      if (is.numeric(analyte)) {
        # if there is passed id of analyte
        analyte <- as.character(analyte)
        analyte_name <- self$analytes[[analyte]]$analyte_name
      } else {
        analyte_name <- analyte
      }
      analyte_id <- self$get_analyte_id(analyte_name)
      if (length(analyte_id) == 0) {
        stop("Analyte of given name not found")
      }

      sample <- self$get_sample(sample)

      if (is.null(data_type)) {
        return(sample$data[analyte_name])
      } else {
        if (!data_type %in% row.names(sample$data[analyte_name])) {
          stop(paste0("Incorrect value for `data_type`: ", data_type))
        }
        return(sample$data[data_type, analyte_name])
      }


      return(sample)
    },

    #' @description
    #' Function adjusts the values of test samples substracting values from BLANK samples to remove background light
    #' In short it substracts the values from data in all samples, except from Blanks. It does not substract values from
    #' `Count` values
    #'
    #' @param method How the values of different blanks should be aggregated. By default `avg`. For now it is the only available method
    #' @param inplace Whether the method should produce new plate with adjusted values or not, By default `TRUE` - operates on the current plate.
    blank_adjustment = function(method = "avg", inplace = "TRUE") {
      if (private$blank_already_adjusted) {
        stop("Blank values have been already adjusted in this plate, if you want to try doing it using different method consider reversing this process")
      }

      private$standard_curve_private <- NULL
      private$blank_already_adjusted <- TRUE
      available_methods <- c("avg")
      if (!method %in% available_methods) {
        stop(paste0(method, "not available for now, consider using one of the following: ", available_methods))
      }

      if (inplace == FALSE) {
        newplate <- self$copy()
      } else {
        newplate <- self
      }

      blank_samples <- self$get_sample_by_type("BLANK") # these values will be substracted
      non_blank_samples <- self$get_sample_by_type("BLANK", exclude = TRUE) # from these values

      # aggregate blank values

      if (method == "avg") {
        agg_dataframe <- NULL
        for (sample in blank_samples) {
          if (is.null(agg_dataframe)) {
            agg_dataframe <- sample$data
          } else {
            agg_dataframe <- agg_dataframe + sample$data
          }
        }
        if ("Count" %in% rownames(agg_dataframe)) {
          agg_dataframe["Count", ] <- 0 # count row is ommited
        }
        if ("Total Events" %in% colnames(agg_dataframe)) {
          agg_dataframe["Total Events"] <- 0
        }

        agg_dataframe <- agg_dataframe / length(blank_samples) # average the results


        for (sample in non_blank_samples) {
          sample$data <- sample$data - agg_dataframe # substract the aggregated values
        }
      }
    },


    #' @description
    #' Function verifies if there are any MFI values below zero after blank removal
    check_MFI_after_adjustment = function() {
      if (!self$check_if_blanks_already_adjusted) {
        stop("Consider adjusting the blanks first")
      }

      below_zero_list <- list()

      below_zero_flag <- FALSE

      for (sample in self$get_sample_by_type("BLANK", exclude = TRUE)) {
        below_min <- which(sample$data < 0, arr.ind = TRUE)
        if (length(below_min) > 0) {
          below_min_analytes <- names(sample$data)[below_min[, "col"]]
          new_warnings <- paste0("An analyte ", below_min_analytes, " has value below 0 after blank adjustment")
          sample$warnings <- c(sample$warnings, new_warnings)
        }
      }

      return(below_zero_flag)
    },


    #' @description performs copy of the plate
    copy = function() {
      stop("Not implemented yet")
    }
  ),
  private = list(
    blank_already_adjusted = FALSE,
    verbose = TRUE,
    standard_curve_private = NULL
  ),
  active = list(
    #' @field number_of_samples number of samples stored in the current plate
    number_of_samples = function() {
      return(length(self$samples))
    },

    #' @field analyte_names list of all analyte names saved in the plate
    analyte_names = function() {
      analyte_names <- c()
      for (analyte in self$analytes) {
        analyte_names <- c(analyte_names, analyte$analyte_name)
      }
      return(analyte_names)
    },

    #' @field sample_names list of all sample names
    sample_names = function() {
      sample_names <- c()
      for (sample in self$samples) {
        sample_names <- c(sample_names, sample$sample_name)
      }
      return(sample_names)
    },

    #' @field examination_date Metadata: date of the examination
    examination_date = function() {
      return(self$batch_info$Date)
    },

    #' @field batch_name Metdata: batch name
    batch_name = function() {
      if (!is.null(self$batch_info$batch_name) && !is.na(self$batch_info$batch_name)) {
        return(self$batch_info$batch_name)
      }
      return("___")
    },

    #' @field min_events_per_bead minimal number of events that is valid for one bead - sample and analyte
    min_events_per_bead = function() {
      if (is.null(self$batch_info$min_events_per_bead)) {
        return(50)
      }
      return(self$batch_info$min_events_per_bead)
    },
    #' @field check_if_blanks_already_adjusted flag that specifies if the blanks were already adjusted and its MFI values subtracted from remaining samples
    check_if_blanks_already_adjusted = function() {
      return(private$blank_already_adjusted)
    },

    #' @field warnings list of lists of all warnings from all samples
    warnings = function() {
      warnings <- lapply(self$samples, function(sample) sample$warnings)
      remove_empty_lists(warnings)
    },

    #' @field errors list of lists of all errors from all samples
    errors = function() {
      errors <- lapply(self$samples, function(sample) sample$errors)
      remove_empty_lists(errors)
    },

    standard_curve = function() {
      if (! is.null(private$standard_curve_private)) {
        return(private$standard_curve_private)
      }

      if (!self$check_if_blanks_already_adjusted) {
        verbose_cat(
          "(",
          color_codes$red_start,
          "WARNING",
          color_codes$red_end,
          ")",
          "\nBlank values not adjusted - Consider adjusting the blank values using function `plate$blank_adjustment`\n",
          verbose = private$verbose
        )
      }

      standard_curves <- self$get_sample_by_type("STANDARD CURVE")
      if (length(standard_curves) == 0){
        verbose_cat(
          "(",
          color_codes$red_start,
          "WARNING",
          color_codes$red_end,
          ")",
          "\nNo standard curve samples found in the plate\nUsing positive control samples",
          verbose = private$verbose
        )
        standard_curves <- plate$get_sample_by_type("POSITIVE CONTROL")
      }


      dilutions <- sapply(standard_curves, function(sample) sample$sample_type$character_dilution_factor)
      dilutions_numeric <- sapply(standard_curves, function(sample) sample$sample_type$dilution_factor)
      # sort values according to dilutions
      sorted_order <- order(dilutions_numeric)

      # Sort the vectors according to the sorted order of the reference vector
      dilutions_numeric <- dilutions_numeric[sorted_order]
      dilutions <- dilutions[sorted_order]
      standard_curves <- standard_curves[sorted_order]
      private$standard_curve_private <- standard_curves
      return(standard_curves)
    }
  )
)


remove_empty_lists <- function(lst) {
  # Filter out elements that are empty lists
  result <- lst[!sapply(lst, function(x) is.list(x) && length(x) == 0)]
  return(result)
}

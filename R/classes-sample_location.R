#' @title SampleLocation Object
#' @description
#' SampleLocation defines a location of a sample on the Luminex plate
#' The location is defined by the row and column number
#' and explicitly states the location of the sample
#'
#' @field col column number of sample on the Luminex plate
#'
#' @field row row number of sample on the Luminex plate
#'
#' @field row_letter row where sample is located as a letter.
#' For instance `1` -> `A`
#'
#' @field location_name Location of the sample formatted
#' in readable way - `(row_letter, column)`
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
SampleLocation <- R6::R6Class(
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
    #' Essentially checks if the two sample locations are identical or
    #' at least one of them is empty
    #'
    #' @param new_location (`SampleLocation`)\cr
    #' The sample location object to be joined with the current sample location
    #' object
    #'
    #' @return The updated sample location object - an invisible object,
    #' the outputs may be stacked together
    join = function(new_location) {
      # join the data of two samples

      if (!verify_numeric_join(self$row, new_location$row) ||
        !verify_numeric_join(self$col, new_location$col)) {
        stop("Cannot join samples of different locations")
      }

      self$col <- get_join_value(self$col, new_location$col)
      self$row <- get_join_value(self$row, new_location$row)

      invisible(self)
    }
  ),
  active = list(
    #' @description
    #' Returns the letter corresponding to the row number -
    #' useful for printing the location
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
#' @param location_string variable that represents the sample
#' location on the plate formatted as in the Luminex file
#' It should be formatted as: `sample_id(plate_id, location_name)`
#'
#' @returns New SampleLocation object with parsed location information.
#'
#' @examples
#' SampleLocation$parse_sample_location("2(1,A2)")
#'
#' SampleLocation$parse_sample_location("1(3, A1)")
#'
#' @export
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

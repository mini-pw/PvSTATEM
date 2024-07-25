#' @title Analyte Object
#'
#' @description
#' Analyte class is an object that gathers information about one specific
#' Analyte on the Luminex plate
#'
#' @field id A numeric value that represents the unique identifier
#' of the analyte
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
Analyte <- R6::R6Class(
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
    #' If the analytes can be joined, the bead_count, analysis_type and units
    #' are updated in the current analyte object
    #'
    #' @param new_analyte (`Analyte`)\cr
    #' The analyte object to be joined with the current analyte object
    #'
    #' @return The updated analyte object
    join = function(new_analyte) {
      # join the data of two samples

      if (!verify_numeric_join(self$id, new_analyte$id)) {
        stop("Cannot join analytes of different IDs")
      }
      self$id <- get_join_value(self$id, new_analyte$id)

      if (!verify_numeric_join(self$bead_count, new_analyte$bead_count)) {
        stop("Cannot join analytes with different bead counts")
      }
      if (!verify_character_join(self$analysis_type,
                                 new_analyte$analysis_type)) {
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
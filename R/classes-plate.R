#' @title Plate
#' @description
#' A class to represent the luminex plate. It contains information about
#' the samples and analytes that were examined on the plate as well as
#' some additional metadata and batch info
#'
#' @export
Plate <- R6::R6Class(
  "Plate",
  public = list(

    ## Fields ----------------------------------------------------------------

    #' @field plate_name - plate name obtained from filename
    plate_name = "",

    #' @field analytes_names vector of analytes names measured during
    #' an examination in the same order as in the data
    analytes_names = character(),

    #' @field sample_names vector of sample names measured during
    #' an examination in the same order as in the data
    sample_names = character(),

    #' @field sample_locations vector of sample locations pretty name ie. A1, B2
    sample_locations = character(),

    #' @field dilutions vector of dilutions used during the examination
    #' due to the nature of data it's a vector of strings, logic to transform
    #' it to numeric has to be implemented in the future
    dilutions = character(),

    #' @field sample_types vector of sample types used during the examination
    #' those values are derived from the sample names in the Luminex file
    #' using regular expressions
    #' possibly in the future it will be possible to specify the sample types
    #' manually either by passing a vector or by select it in the GUI
    sample_types = character(),

    #' @field data a named list of data frames containing information about
    #' the samples and analytes. The list is named by the type of the data
    #' e.g. `Median`, `Net MFI`, etc.
    #' The data frames contain information about the samples and analytes
    #' The rows are different measures, whereas the columns represent
    #' different analytes
    #' Example of how `data$Median` looks like:
    #' | Sample  | Analyte1 | Analyte2 | Analyte3 |
    #' |---------|----------|----------|----------|
    #' | Sample1 | 1.2      | 2.3      | 3.4      |
    #' | Sample2 | 4.5      | 5.6      | 6.7      |
    #' | ...     | ...      | ...      | ...      |
    #' | Sample96| 7.8      | 8.9      | 9.0      |
    data = list(),

    #' @field data_type_used a character value representing the type of data
    #' that is currently used for calculations. By default, it is set to Median
    data_type_used = "Median",

    #' @field batch_info a raw list containing metadata about
    #' the plate read from the Luminex file
    batch_info = list(),

    ## Methods ---------------------------------------------------------------

    #' @description
    #' Function prints the basic information about the plate
    #' such as the number of samples and analytes
    print = function(...) {
      cat(
        "Plate with",
        length(self$sample_names),
        "samples and",
        length(self$analytes_names),
        "analytes\n"
      )
    },

    #' @description
    #' Function outputs basic information about the plate, such as
    #' examination date, batch name, and sample types
    #'
    #' @param include_names If `include_names` parameter is `TRUE`, a
    #' part from count of control samples, provides also their names.
    #' By default `FALSE`
    summary = function(..., include_names = FALSE) {},


    #' @description
    #' Function returns data for a specific analyte and sample.
    #'
    #' @param analyte An analyte name or its id of which data we want to extract
    #'
    #' @param sample sample name or id
    #' @param data_type if `NULL` returns whole column of the dataframe
    #' containing information about the sample. Default value is plate's
    #' `data_type_used` usually `Median`.
    #'
    #' @return Data about a sample and analyte
    get = function(analyte, sample_type, data_type = self$data_type_used) {
      # check if the analyte exists in analytes_names
      if (!is.null(analyte) && !is.na(analyte)) {
        if (!analyte %in% analytes_names) {
          stop("Analyte does not exist in analytes_names")
        }
      } else {
        stop("Analyte is either NULL or NA")
      }

      # check if the sample_type exists in valid_sample_types
      if (!is.null(sample_type) && !is.na(sample_type)) {
        if (!sample_type %in% valid_sample_types) {
          stop("Sample type does not exist in valid_sample_types")
        }
      } else {
        stop("Sample type is either NULL or NA")
      }

      # check if the data_type exists in valid_data_types
      if (!is.null(data_type) && !is.na(data_type)) {
        if (!data_type %in% valid_data_types) {
          stop("Data type does not exist in valid_data_types")
        }
      } else {
        stop("Data type is either NULL or NA")
      }

      # get samples of the given type, data_type and analyte and return them
      valid_samples <- sample_types == sample_type
      data_of_specified_type <- self$data[[data_type]]
      return(data_of_specified_type[valid_samples, analyte])
    },

    #' @description
    #' Function adjusts the values of test samples by subtracting average of
    #' BLANK samples purpose of this operation is to remove background light
    #' In short it subtracts the values from data in all samples, except from
    #' Blanks. It does not subtract values from `Count` values, even if this
    #' step seems logical, people from the lab do not do it always.
    #'
    #' @param method How the values of different blanks should be aggregated.
    #' By default `avg`. For now it is the only available method
    #' @param inplace Whether the method should produce new plate with adjusted
    #' values or not, By default `TRUE` - operates on the current plate.
    blank_adjustment = function(method = "avg", in_place = "TRUE") {},

    #' @description
    #' Function takes the data 1D array and returns the data in the form of
    #' a 2D array, that represents the spatial arrangement of the samples
    #' on the plate, ie. 12x8 grid.
    #'
    #' @param data 1D array of data represents for example counts, dilutions
    #' or sample types
    #'
    #' @return A 2D array representing data with the spatial arrangement
    #' of the samples
    layout = function(data) {}
  ),
  private = list(

    ## Private Fields ---------------------------------------------------------
    blank_adjusted = FALSE,
    verbose = TRUE,
    valid_sample_types = c(
      "BLANK",
      "TEST",
      "NEGATIVE CONTROL",
      "STANDARD CURVE",
      "POSITIVE CONTROL"
    ),
    # this is a vector of valid data types but it is not complete
    # because I do not know all the possible values, I haven't
    # interact that much with the Luminex data
    valid_data_types = c(
      "Median",
      "Net MFI",
      "Count",
      "Avg net MFI",
      "Mean",
      "%CV",
      "Peak",
      "Std Dev"
    ),

    ## Private Methods --------------------------------------------------------

    #' @description
    #' Function translates sample names to sample types
    #' The function uses regular expressions to match the sample names
    #' to the sample types
    #'
    #' @param sample_names A vector of sample names
    #' @return A vector of sample types

    #' @examples
    #' translate_sample_names_to_sample_types(c("B", "BLANK", "TEST1"))
    #' translate_sample_names_to_sample_types(c("S", "CP3"))
    #'
    translate_sample_names_to_sample_types = function(sample_names) {}
    # this function is implemented only once at the moment of parsing the data
    # it might be better to move it to other file

    # this function is not implemented yet
    # it consists of convoluted logic that uses regular expressions
    # but luckily a lot can be scavenged from the existing code
    # in the `SampleType` class it's just need to be adapted to
    # work with new structure of the data
  )
)
